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

;; Consult completion sources for the Spofy Spotify client.  Provides five
;; interactive commands for searching tracks, albums, artists, playlists,
;; and devices using the Consult async narrowing framework.

;;; Code:

(require 'consult)
(require 'spofy-api)

(declare-function spofy-play-track "spofy-player" (uri &optional context-uri))
(declare-function spofy-play-context "spofy-player" (context-uri))
(declare-function spofy-view-album "spofy-browse" (album-id))
(declare-function spofy-view-artist "spofy-browse" (artist-id))
(declare-function spofy-view-playlist "spofy-browse" (playlist-id))
(declare-function spofy-ui-format-artists "spofy-ui" (artists))
(declare-function spofy-api-put "spofy-api" (endpoint &optional data callback))
(declare-function spofy-auth-access-token "spofy-auth" ())

;;;; Synchronous API helper

(defun spofy-consult--api-get-sync (endpoint params)
  "Synchronous GET from the Spotify API.
ENDPOINT is a relative path.  PARAMS is an alist of query parameters.
Return the parsed JSON response, or nil on error."
  (let* ((url (spofy-api--build-url endpoint params))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(format "Bearer %s" (spofy-auth-access-token)))))
         (url-show-status nil)
         (buf (url-retrieve-synchronously url t nil 10)))
    (when buf
      (unwind-protect
          (with-current-buffer buf
            (let ((status (spofy-api--response-status)))
              (when (and status (>= status 200) (< status 300))
                (spofy-api--parse-response))))
        (kill-buffer buf)))))

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
  (let* ((name (or (alist-get 'name track) ""))
         (artists (or (alist-get 'artists track) []))
         (album (alist-get 'album track))
         (album-name (or (and album (alist-get 'name album)) ""))
         (artist-str (spofy-ui-format-artists artists))
         (duration-ms (or (alist-get 'duration_ms track) 0))
         (candidate
          (concat
           (spofy-consult--pad
            (propertize (spofy-ui-truncate name 35) 'face 'spofy-track-name) 37)
           (spofy-consult--pad
            (propertize (spofy-ui-truncate artist-str 25) 'face 'spofy-artist-name) 27)
           (spofy-consult--pad
            (propertize (spofy-ui-truncate album-name 25) 'face 'spofy-album-name) 27)
           (propertize (spofy-ui-format-duration-ms duration-ms) 'face 'spofy-muted))))
    (propertize candidate 'spofy-entity track)))

(defun spofy-consult--format-album (album)
  "Format ALBUM alist as a tabular consult candidate string.
The full entity is stored as a text property."
  (let* ((name (or (alist-get 'name album) ""))
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
            (propertize (spofy-ui-truncate name 35) 'face 'spofy-album-name) 37)
           (spofy-consult--pad
            (propertize (spofy-ui-truncate artist-str 25) 'face 'spofy-artist-name) 27)
           (spofy-consult--pad
            (propertize year 'face 'spofy-muted) 6)
           (propertize (format "%d tracks" total-tracks) 'face 'spofy-muted))))
    (propertize candidate 'spofy-entity album)))

(defun spofy-consult--format-artist (artist)
  "Format ARTIST alist as a tabular consult candidate string.
The full entity is stored as a text property."
  (let* ((name (or (alist-get 'name artist) ""))
         (genres (or (alist-get 'genres artist) []))
         (genres-str (if (> (length genres) 0)
                         (mapconcat #'identity genres ", ")
                       ""))
         (followers (or (alist-get 'followers artist) nil))
         (follower-count (or (and followers (alist-get 'total followers)) 0))
         (candidate
          (concat
           (spofy-consult--pad
            (propertize (spofy-ui-truncate name 35) 'face 'spofy-artist-name) 37)
           (spofy-consult--pad
            (propertize (spofy-ui-truncate genres-str 30) 'face 'spofy-muted) 32)
           (propertize (number-to-string follower-count) 'face 'spofy-muted))))
    (propertize candidate 'spofy-entity artist)))

(defun spofy-consult--format-playlist (playlist)
  "Format PLAYLIST alist as a tabular consult candidate string.
The full entity is stored as a text property."
  (let* ((name (or (alist-get 'name playlist) ""))
         (owner (alist-get 'owner playlist))
         (owner-name (or (and owner (alist-get 'display_name owner)) ""))
         (tracks-info (alist-get 'tracks playlist))
         (total (or (and tracks-info (alist-get 'total tracks-info)) 0))
         (candidate
          (concat
           (spofy-consult--pad
            (propertize (spofy-ui-truncate name 35) 'face 'spofy-track-name) 37)
           (spofy-consult--pad
            (propertize (spofy-ui-truncate owner-name 20) 'face 'spofy-muted) 22)
           (propertize (format "%d tracks" total) 'face 'spofy-muted))))
    (propertize candidate 'spofy-entity playlist)))

(defun spofy-consult--format-device (device)
  "Format DEVICE alist as a tabular consult candidate string.
The full entity is stored as a text property."
  (let* ((name (or (alist-get 'name device) ""))
         (type (or (alist-get 'type device) ""))
         (candidate
          (concat
           (spofy-consult--pad
            (propertize (spofy-ui-truncate name 30) 'face 'spofy-track-name) 32)
           (propertize type 'face 'spofy-muted))))
    (propertize candidate 'spofy-entity device)))

(defun spofy-consult--get-entity (candidate)
  "Extract the spofy entity from CANDIDATE text properties."
  (get-text-property 0 'spofy-entity candidate))

;;;; Collection builders

(defun spofy-consult--search-collection (type format-fn)
  "Return a synchronous dynamic collection function searching Spotify for TYPE.
FORMAT-FN is called on each result item to produce a candidate string."
  (lambda (input)
    (let* ((response (spofy-consult--api-get-sync
                      "search"
                      `(("q" . ,input) ("type" . ,type) ("limit" . "20"))))
           (section-key (intern (concat type "s")))
           (section (alist-get section-key response))
           (items (alist-get 'items section)))
      (when items
        (mapcar format-fn (append items nil))))))

;;;; Track source

;;;###autoload
(defun consult-spofy-track ()
  "Search Spotify tracks and play the selected one."
  (interactive)
  (let* ((selected
          (consult--read
           (consult--dynamic-collection
            (spofy-consult--search-collection
             "track" #'spofy-consult--format-track)
            :debounce 0.3
            :min-input 1)
           :prompt "Spofy track: "
           :category 'spofy-track
           :sort nil
           :require-match t)))
    (when-let* ((entity (spofy-consult--get-entity selected))
                (uri (alist-get 'uri entity)))
      (spofy-play-track uri))))

;;;; Album source

;;;###autoload
(defun consult-spofy-album ()
  "Search Spotify albums and open the selected one."
  (interactive)
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
           :require-match t)))
    (when-let* ((entity (spofy-consult--get-entity selected))
                (album-id (alist-get 'id entity)))
      (spofy-view-album album-id))))

;;;; Artist source

;;;###autoload
(defun consult-spofy-artist ()
  "Search Spotify artists and open the selected one."
  (interactive)
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
           :require-match t)))
    (when-let* ((entity (spofy-consult--get-entity selected))
                (artist-id (alist-get 'id entity)))
      (spofy-view-artist artist-id))))

;;;; Playlist source

;;;###autoload
(defun consult-spofy-playlist ()
  "Search Spotify playlists and open the selected one."
  (interactive)
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
           :require-match t)))
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
                (let* ((response (spofy-consult--api-get-sync
                                  "me/playlists" '(("limit" . "50"))))
                       (items (alist-get 'items response)))
                  (when items
                    (mapcar #'spofy-consult--format-playlist
                            (append items nil)))))))))

;;;###autoload
(defun consult-spofy-my-playlist ()
  "Pick from the user's Spotify playlists and open the selected one."
  (interactive)
  (let* ((selected
          (consult--read
           (consult--dynamic-collection
            (spofy-consult--my-playlist-collection)
            :min-input 0)
           :prompt "Spofy my playlist: "
           :category 'spofy-playlist
           :sort nil
           :require-match t)))
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
                (let* ((data (spofy-consult--api-get-sync
                              "me/player/devices" nil))
                       (devices (alist-get 'devices data)))
                  (when devices
                    (mapcar #'spofy-consult--format-device
                            (append devices nil)))))))))

;;;###autoload
(defun consult-spofy-device ()
  "Pick a Spotify playback device and transfer playback to it."
  (interactive)
  (let* ((selected
          (consult--read
           (consult--dynamic-collection
            (spofy-consult--device-collection)
            :min-input 0)
           :prompt "Spofy device: "
           :category 'spofy-device
           :sort nil
           :require-match t)))
    (when-let* ((entity (spofy-consult--get-entity selected))
                (device-id (alist-get 'id entity))
                (device-name (alist-get 'name entity)))
      (spofy-api-put "me/player"
                     `((device_ids . [,device-id])
                       (play . t))
                     (lambda (_)
                       (message "Spofy: transferred playback to %s"
                                device-name))))))

(provide 'spofy-consult)
;;; spofy-consult.el ends here
