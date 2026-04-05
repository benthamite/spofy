;;; spofy-consult.el --- Consult integration for Spofy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;; Author: Pablo Stafforini
;; URL: https://github.com/pablostafforini/spofy
;; Package-Requires: ((emacs "30.1") (consult "1.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Consult completion sources for the Spofy Spotify client.  Provides seven
;; interactive commands for searching tracks, albums, artists, playlists,
;; devices, and current context tracks using the Consult async narrowing
;; framework.

;;; Code:

(require 'consult nil t)
(require 'seq)
(require 'spofy-api)

(declare-function consult--read "ext:consult")
(declare-function consult--dynamic-collection "ext:consult")
(declare-function consult--lookup-member "ext:consult")

(declare-function spofy-play-track "spofy-player" (uri &optional context-uri))
(declare-function spofy-play-context "spofy-player" (context-uri))
(declare-function spofy-player--ensure-device "spofy-player" ())
(declare-function spofy-player--poll-sync "spofy-player" ())
(declare-function spofy-player--fetch-context-tracks "spofy-player" (context-type context-id))
(declare-function spofy-view-album "spofy-browse" (album-id))
(declare-function spofy-view-artist "spofy-browse" (artist-id))
(declare-function spofy-view-playlist "spofy-browse" (playlist-id))
(declare-function spofy-ui-format-artists "spofy-ui" (artists))

(defvar spofy-player--current-state)

;;;; Customization

(defcustom spofy-consult-columns
  '((track    . (40 30 30))
    (album    . (40 30 6))
    (artist   . (40 35))
    (playlist . (40 25))
    (device   . (35)))
  "Column widths for consult candidate formatting.
Each entry is (TYPE . (COL1 COL2 ...)).  The columns vary by type:

  track:    Name, Artist(s), Album  (Duration is not truncated)
  album:    Name, Year, Artist(s)
  artist:   Name, Genres
  playlist: Name, Owner
  device:   Name

Only the listed columns are padded/truncated; trailing fields are
appended as-is."
  :type '(alist :key-type symbol
                :value-type (repeat integer))
  :group 'spofy)

(defun spofy-consult--col (type n)
  "Return the Nth column width for TYPE from `spofy-consult-columns'."
  (nth n (alist-get type spofy-consult-columns)))

(defun spofy-consult--available-p ()
  "Return non-nil when the Consult internals used by Spofy are loaded."
  (and (featurep 'consult)
       (fboundp 'consult--read)
       (fboundp 'consult--dynamic-collection)
       (fboundp 'consult--lookup-member)))

(defun spofy-consult--ensure-available ()
  "Signal a user-facing error when Consult is unavailable."
  (unless (spofy-consult--available-p)
    (user-error "Spofy: Consult is not installed")))

;;;; Helpers

(defun spofy-consult--pad (string width)
  "Pad STRING to WIDTH display columns."
  (let ((cur (string-width string)))
    (if (>= cur width)
        string
      (concat string (make-string (- width cur) ?\s)))))

(defun spofy-consult--format-track (track)
  "Format TRACK alist as a tabular consult candidate string.
The full entity is stored as a text property."
  (let* ((w1 (spofy-consult--col 'track 0))
         (w2 (spofy-consult--col 'track 1))
         (w3 (spofy-consult--col 'track 2))
         (name (or (alist-get 'name track) ""))
         (artists (or (alist-get 'artists track) []))
         (album (alist-get 'album track))
         (album-name (or (and album (alist-get 'name album)) ""))
         (artist-str (spofy-ui-format-artists artists))
         (duration-ms (or (alist-get 'duration_ms track) 0))
         (candidate
          (concat
           (spofy-consult--pad
            (spofy-ui-truncate name w1 'spofy-track-name) (+ w1 2))
           (spofy-consult--pad
            (spofy-ui-truncate artist-str w2 'spofy-artist-name) (+ w2 2))
           (spofy-consult--pad
            (spofy-ui-truncate album-name w3 'spofy-album-name) (+ w3 2))
           (propertize (spofy-ui-format-duration-ms duration-ms) 'face 'spofy-muted))))
    (propertize candidate 'spofy-entity track)))

(defun spofy-consult--format-album (album)
  "Format ALBUM alist as a tabular consult candidate string.
The full entity is stored as a text property."
  (let* ((w1 (spofy-consult--col 'album 0))
         (w2 (spofy-consult--col 'album 1))
         (w3 (spofy-consult--col 'album 2))
         (name (or (alist-get 'name album) ""))
         (artists (or (alist-get 'artists album) []))
         (artist-str (spofy-ui-format-artists artists))
         (release-date (or (alist-get 'release_date album) ""))
         (year (if (>= (length release-date) 4)
                   (substring release-date 0 4)
                 release-date))
         (total-tracks (or (alist-get 'total_tracks album) 0))
         (candidate
          (concat
           (spofy-consult--pad
            (spofy-ui-truncate name w1 'spofy-album-name) (+ w1 2))
           (spofy-consult--pad
            (propertize year 'face 'spofy-muted) w3)
           (spofy-consult--pad
            (spofy-ui-truncate artist-str w2 'spofy-artist-name) (+ w2 2))
           (propertize (number-to-string total-tracks) 'face 'spofy-muted))))
    (propertize candidate 'spofy-entity album)))

(defun spofy-consult--format-artist (artist)
  "Format ARTIST alist as a tabular consult candidate string.
The full entity is stored as a text property."
  (let* ((w1 (spofy-consult--col 'artist 0))
         (w2 (spofy-consult--col 'artist 1))
         (name (or (alist-get 'name artist) ""))
         (genres (or (alist-get 'genres artist) []))
         (genres-str (if (> (length genres) 0)
                         (mapconcat #'identity genres ", ")
                       ""))
         (followers (or (alist-get 'followers artist) nil))
         (follower-count (or (and followers (alist-get 'total followers)) 0))
         (candidate
          (concat
           (spofy-consult--pad
            (spofy-ui-truncate name w1 'spofy-artist-name) (+ w1 2))
           (spofy-consult--pad
            (spofy-ui-truncate genres-str w2 'spofy-muted) (+ w2 2))
           (propertize (number-to-string follower-count) 'face 'spofy-muted))))
    (propertize candidate 'spofy-entity artist)))

(defun spofy-consult--format-playlist (playlist)
  "Format PLAYLIST alist as a tabular consult candidate string.
The full entity is stored as a text property."
  (let* ((w1 (spofy-consult--col 'playlist 0))
         (w2 (spofy-consult--col 'playlist 1))
         (name (or (alist-get 'name playlist) ""))
         (owner (alist-get 'owner playlist))
         (owner-name (or (and owner (alist-get 'display_name owner)) ""))
         (tracks-info (alist-get 'tracks playlist))
         (total (or (and tracks-info (alist-get 'total tracks-info)) 0))
         (candidate
          (concat
           (spofy-consult--pad
            (spofy-ui-truncate name w1 'spofy-track-name) (+ w1 2))
           (spofy-consult--pad
            (spofy-ui-truncate owner-name w2 'spofy-muted) (+ w2 2))
           (propertize (number-to-string total) 'face 'spofy-muted))))
    (propertize candidate 'spofy-entity playlist)))

(defun spofy-consult--format-device (device)
  "Format DEVICE alist as a tabular consult candidate string.
The full entity is stored as a text property."
  (let* ((w1 (spofy-consult--col 'device 0))
         (name (or (alist-get 'name device) ""))
         (type (or (alist-get 'type device) ""))
         (candidate
          (concat
           (spofy-consult--pad
            (spofy-ui-truncate name w1 'spofy-track-name) (+ w1 2))
           (propertize type 'face 'spofy-muted))))
    (propertize candidate 'spofy-entity device)))

(defun spofy-consult--get-entity (candidate)
  "Extract the spofy entity from CANDIDATE text properties."
  (get-text-property 0 'spofy-entity candidate))

;;;; Collection builders

(defun spofy-consult--null-p (item)
  "Return non-nil if ITEM is a JSON null (the keyword :null)."
  (eq item :null))

(defun spofy-consult--search-collection (type format-fn)
  "Return a synchronous dynamic collection function searching Spotify for TYPE.
FORMAT-FN is called on each result item to produce a candidate string."
  (lambda (input)
    (let* ((response (spofy-api-get-sync
                      "search"
                      `(("q" . ,input) ("type" . ,type) ("limit" . "20"))))
           (section-key (intern (concat type "s")))
           (section (alist-get section-key response))
           (items (alist-get 'items section)))
      (when items
        (mapcar format-fn (seq-remove #'spofy-consult--null-p
                                      (append items nil)))))))

;;;; Track source

;;;###autoload
(defun consult-spofy-track ()
  "Search Spotify tracks and play the selected one."
  (interactive)
  (spofy-consult--ensure-available)
  (let* ((selected
          (consult--read
           (consult--dynamic-collection
            (spofy-consult--search-collection
             "track" #'spofy-consult--format-track)
            ;; 300ms debounce avoids flooding the Spotify API on fast typing;
            ;; min-input 1 prevents an empty initial search.
            :debounce 0.3
            :min-input 1)
           :prompt "Spofy track: "
           :category 'spofy-track
           :sort nil
           :require-match t
           :lookup #'consult--lookup-member)))
    (when-let* ((entity (spofy-consult--get-entity selected))
                (uri (alist-get 'uri entity)))
      (spofy-play-track uri))))

;;;; Album source

;;;###autoload
(defun consult-spofy-album ()
  "Search Spotify albums and open the selected one."
  (interactive)
  (spofy-consult--ensure-available)
  (let* ((selected
          (consult--read
           (consult--dynamic-collection
            (spofy-consult--search-collection
             "album" #'spofy-consult--format-album)
            :debounce 0.3
            :min-input 1)
           :prompt "Spofy album: "
           :category 'spofy-album
           :sort nil
           :require-match t
           :lookup #'consult--lookup-member)))
    (when-let* ((entity (spofy-consult--get-entity selected))
                (album-id (alist-get 'id entity)))
      (spofy-view-album album-id))))

;;;; Artist source

;;;###autoload
(defun consult-spofy-artist ()
  "Search Spotify artists and open the selected one."
  (interactive)
  (spofy-consult--ensure-available)
  (let* ((selected
          (consult--read
           (consult--dynamic-collection
            (spofy-consult--search-collection
             "artist" #'spofy-consult--format-artist)
            :debounce 0.3
            :min-input 1)
           :prompt "Spofy artist: "
           :category 'spofy-artist
           :sort nil
           :require-match t
           :lookup #'consult--lookup-member)))
    (when-let* ((entity (spofy-consult--get-entity selected))
                (artist-id (alist-get 'id entity)))
      (spofy-view-artist artist-id))))

;;;; Playlist source

;;;###autoload
(defun consult-spofy-playlist ()
  "Search Spotify playlists and open the selected one."
  (interactive)
  (spofy-consult--ensure-available)
  (let* ((selected
          (consult--read
           (consult--dynamic-collection
            (spofy-consult--search-collection
             "playlist" #'spofy-consult--format-playlist)
            :debounce 0.3
            :min-input 1)
           :prompt "Spofy playlist: "
           :category 'spofy-playlist
           :sort nil
           :require-match t
           :lookup #'consult--lookup-member)))
    (when-let* ((entity (spofy-consult--get-entity selected))
                (playlist-id (alist-get 'id entity)))
      (spofy-view-playlist playlist-id))))

(defun spofy-consult--my-playlist-collection ()
  "Return a synchronous dynamic collection for the user's playlists.
Results are fetched once and cached for subsequent calls."
  (let ((cache nil))
    (lambda (_input)
      (or cache
          (setq cache
                (let* ((response (spofy-api-get-sync
                                  "me/playlists" '(("limit" . "50"))))
                       (items (alist-get 'items response)))
                  (when items
                    (mapcar #'spofy-consult--format-playlist
                            (seq-remove #'spofy-consult--null-p
                                        (append items nil))))))))))

;;;###autoload
(defun consult-spofy-my-playlist ()
  "Pick from the user's Spotify playlists and open the selected one."
  (interactive)
  (spofy-consult--ensure-available)
  (let* ((selected
          (consult--read
           (consult--dynamic-collection
            (spofy-consult--my-playlist-collection)
            :min-input 0)
           :prompt "Spofy my playlist: "
           :category 'spofy-playlist
           :sort nil
           :require-match t
           :lookup #'consult--lookup-member)))
    (when-let* ((entity (spofy-consult--get-entity selected))
                (playlist-id (alist-get 'id entity)))
      (spofy-view-playlist playlist-id))))

;;;; Device source (synchronous -- fetches available devices)

(defun spofy-consult--device-collection ()
  "Return a synchronous dynamic collection for Spotify devices.
Results are fetched once and cached for subsequent calls."
  (let ((cache nil))
    (lambda (_input)
      (or cache
          (setq cache
                (let* ((data (spofy-api-get-sync
                              "me/player/devices" nil))
                       (devices (alist-get 'devices data)))
                  (when devices
                    (mapcar #'spofy-consult--format-device
                            (append devices nil)))))))))

;;;###autoload
(defun consult-spofy-device ()
  "Pick a Spotify playback device and transfer playback to it."
  (interactive)
  (spofy-consult--ensure-available)
  (let* ((selected
          (consult--read
           (consult--dynamic-collection
            (spofy-consult--device-collection)
            :min-input 0)
           :prompt "Spofy device: "
           :category 'spofy-device
           :sort nil
           :require-match t
           :lookup #'consult--lookup-member)))
    (when-let* ((entity (spofy-consult--get-entity selected))
                (device-id (alist-get 'id entity))
                (device-name (alist-get 'name entity)))
      (spofy-api-put "me/player"
                     `((device_ids . [,device-id])
                       (play . t))
                     (lambda (_)
                       (message "Spofy: transferred playback to %s"
                                device-name))))))

;;;; Context track source (jump to track in current playback)

;;;###autoload
(defun consult-spofy-context-track ()
  "Pick a track from the current playback context and play it.
Fetches the tracklist of the currently playing album or playlist
and presents all tracks for selection with narrowing.  Signals an
error when the context is unsupported (e.g. Spotify-generated
radio or daily mixes)."
  (interactive)
  (spofy-consult--ensure-available)
  (require 'spofy-player)
  (spofy-player--ensure-device)
  (unless spofy-player--current-state
    (spofy-player--poll-sync))
  (let* ((context-uri (alist-get 'context-uri spofy-player--current-state))
         (context-type (alist-get 'context-type spofy-player--current-state))
         (context-id (and context-uri
                          (car (last (split-string context-uri ":"))))))
    (unless (and context-id (member context-type '("album" "playlist")))
      (user-error "Spofy: current tracks not available for this context"))
    (let* ((raw-items (spofy-player--fetch-context-tracks context-type context-id))
           (playlist-p (equal context-type "playlist"))
           (candidates
            (when raw-items
              (cl-loop for item across raw-items
                       for track = (if playlist-p (alist-get 'track item) item)
                       when track collect (spofy-consult--format-track track)))))
      (unless candidates
        (user-error "Spofy: current tracks not available for this context"))
      (let ((selected
             (consult--read candidates
              :prompt "Jump to track: "
              :category 'spofy-track
              :sort nil
              :require-match t
              :lookup #'consult--lookup-member)))
        (when-let* ((entity (spofy-consult--get-entity selected))
                    (uri (alist-get 'uri entity)))
          (spofy-play-track uri context-uri))))))

(provide 'spofy-consult)
;;; spofy-consult.el ends here
