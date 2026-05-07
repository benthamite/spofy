;;; spofy-consult.el --- Consult integration for spofy  -*- lexical-binding: t; -*-

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

;; Consult completion sources for the spofy Spotify client.  Provides seven
;; interactive commands for searching tracks, albums, artists, playlists,
;; devices, and current context tracks using the Consult async narrowing
;; framework.

;;; Code:

(require 'consult nil t)
(require 'seq)
(require 'spofy-api)

(declare-function consult--read "ext:consult")
(declare-function consult--lookup-member "ext:consult")

(declare-function spofy-play-track "spofy-player" (uri &optional context-uri position))
(declare-function spofy-library-cache-get "spofy-library" (type))
(declare-function spofy-library--fetch-all-async "spofy-library" (endpoint type &optional callback))
(declare-function spofy-play-context "spofy-player" (context-uri))
(declare-function spofy-player--with-state "spofy-player" (callback))
(declare-function spofy-browse--cache-get "spofy-browse" (key))
(declare-function spofy-browse--cache-put "spofy-browse" (key items))
(declare-function spofy-browse--fetch-all-pages "spofy-browse" (collected next-url callback))
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
  "Return non-nil when the Consult internals used by spofy are loaded."
  (and (featurep 'consult)
       (fboundp 'consult--read)
       (fboundp 'consult--lookup-member)))

(defun spofy-consult--ensure-available ()
  "Signal a user-facing error when Consult is unavailable."
  (unless (spofy-consult--available-p)
    (user-error "spofy: Consult is not installed")))

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

(defun spofy-consult--search (type format-fn prompt category action)
  "Search Spotify for TYPE asynchronously and prompt with Consult.
FORMAT-FN formats each result, PROMPT and CATEGORY are passed to
`consult--read', and ACTION is called with the selected entity."
  (spofy-consult--ensure-available)
  (let ((query (read-string (format "spofy %s query: " type))))
    (when (string-empty-p (string-trim query))
      (user-error "spofy: Search query required"))
    (message "spofy: searching %ss..." type)
    (spofy-api-get
     "search"
     `(("q" . ,query) ("type" . ,type) ("limit" . "20"))
     (lambda (response)
       (let* ((section-key (intern (concat type "s")))
              (section (alist-get section-key response))
              (items (alist-get 'items section))
              (next-url (spofy-consult--extract-next-url section))
              (state `(:type ,type :query ,query :next-url ,next-url))
              (candidates
               (when items
                 (mapcar (lambda (item)
                           (spofy-consult--tag-search-state
                            (funcall format-fn item) state))
                         (seq-remove #'spofy-consult--null-p
                                     (append items nil))))))
         (unless candidates
           (user-error "spofy: No %s results for \"%s\"" type query))
         (let ((selected
                (consult--read candidates
                 :prompt prompt
                 :category category
                 :sort nil
                 :require-match t
                 :lookup #'consult--lookup-member)))
          (when-let* ((entity (spofy-consult--get-entity selected)))
            (funcall action entity))))))))

(defun spofy-consult--extract-next-url (section)
  "Extract the next-page URL from SECTION, returning nil for JSON null."
  (let ((url (alist-get 'next section)))
    (unless (spofy-consult--null-p url) url)))

(defun spofy-consult--tag-search-state (candidate state)
  "Attach search STATE as a text property on CANDIDATE.
STATE is a plist with :type, :query, and :next-url keys."
  (put-text-property 0 (length candidate)
                     'spofy-search-state state candidate)
  candidate)

;;;; Track source

;;;###autoload
(defun consult-spofy-track ()
  "Search Spotify tracks and play the selected one."
  (interactive)
  (spofy-consult--search
   "track" #'spofy-consult--format-track
   "spofy track: " 'spofy-track
   (lambda (entity)
     (when-let* ((uri (alist-get 'uri entity)))
       (spofy-play-track uri)))))

;;;; Album source

;;;###autoload
(defun consult-spofy-album ()
  "Search Spotify albums and open the selected one."
  (interactive)
  (spofy-consult--search
   "album" #'spofy-consult--format-album
   "spofy album: " 'spofy-album
   (lambda (entity)
     (when-let* ((album-id (alist-get 'id entity)))
       (spofy-view-album album-id)))))

;;;; Artist source

;;;###autoload
(defun consult-spofy-artist ()
  "Search Spotify artists and open the selected one."
  (interactive)
  (spofy-consult--search
   "artist" #'spofy-consult--format-artist
   "spofy artist: " 'spofy-artist
   (lambda (entity)
     (when-let* ((artist-id (alist-get 'id entity)))
       (spofy-view-artist artist-id)))))

;;;; Playlist source

;;;###autoload
(defun consult-spofy-playlist ()
  "Search Spotify playlists and open the selected one."
  (interactive)
  (spofy-consult--search
   "playlist" #'spofy-consult--format-playlist
   "spofy playlist: " 'spofy-playlist
   (lambda (entity)
     (when-let* ((playlist-id (alist-get 'id entity)))
       (spofy-view-playlist playlist-id)))))

;;;; Library sources (saved tracks, albums, playlists -- fetches all pages)

(declare-function spofy-library--fetch-all "spofy-library" (endpoint type))

(defun spofy-consult--fetch-library-candidates (endpoint type format-fn &optional item-key)
  "Return formatted candidates for the library ENDPOINT.
TYPE is the cache key symbol, FORMAT-FN formats each item.
When ITEM-KEY is non-nil, extract that key from each wrapper
alist before formatting (e.g., \\='track for /me/tracks).
Return candidates from cache if available.  If not cached,
start an async fetch and signal a user error asking to retry."
  (require 'spofy-library)
  (let ((items (spofy-library-cache-get type)))
    (unless items
      (spofy-library--fetch-all-async endpoint type)
      (user-error "spofy: Fetching %ss in the background, please try again shortly" type))
    (cl-loop for item in items
             for entity = (if item-key (alist-get item-key item) item)
             unless (spofy-consult--null-p entity)
             collect (funcall format-fn entity))))

;;;###autoload
(defun spofy-library-search-tracks ()
  "Search saved tracks and play the selected one."
  (interactive)
  (spofy-consult--ensure-available)
  (let ((candidates (spofy-consult--fetch-library-candidates
                     "me/tracks" 'track
                     #'spofy-consult--format-track 'track)))
    (unless candidates
      (user-error "spofy: No saved tracks found"))
    (let ((selected
           (consult--read candidates
            :prompt "spofy saved track: "
            :category 'spofy-track
            :sort nil
            :require-match t
            :lookup #'consult--lookup-member)))
      (when-let* ((entity (spofy-consult--get-entity selected))
                  (uri (alist-get 'uri entity)))
        (spofy-play-track uri)))))

;;;###autoload
(defun spofy-library-search-albums ()
  "Search saved albums and open the selected one."
  (interactive)
  (spofy-consult--ensure-available)
  (let ((candidates (spofy-consult--fetch-library-candidates
                     "me/albums" 'album
                     #'spofy-consult--format-album 'album)))
    (unless candidates
      (user-error "spofy: No saved albums found"))
    (let ((selected
           (consult--read candidates
            :prompt "spofy saved album: "
            :category 'spofy-album
            :sort nil
            :require-match t
            :lookup #'consult--lookup-member)))
      (when-let* ((entity (spofy-consult--get-entity selected))
                  (album-id (alist-get 'id entity)))
        (spofy-view-album album-id)))))

;;;###autoload
(defun spofy-library-search-playlists ()
  "Pick from the user's Spotify playlists and open the selected one."
  (interactive)
  (spofy-consult--ensure-available)
  (let ((candidates (spofy-consult--fetch-library-candidates
                     "me/playlists" 'playlist
                     #'spofy-consult--format-playlist)))
    (unless candidates
      (user-error "spofy: No playlists found"))
    (let ((selected
           (consult--read candidates
            :prompt "spofy my playlist: "
            :category 'spofy-playlist
            :sort nil
            :require-match t
            :lookup #'consult--lookup-member)))
      (when-let* ((entity (spofy-consult--get-entity selected))
                  (playlist-id (alist-get 'id entity)))
        (spofy-view-playlist playlist-id)))))

;;;; Device source

;;;###autoload
(defun consult-spofy-device ()
  "Pick a Spotify playback device and transfer playback to it."
  (interactive)
  (spofy-consult--ensure-available)
  (when-let* ((remaining (spofy-api-rate-limit-remaining)))
    (user-error
     "spofy: Spotify API rate limit exceeded; retry after %s seconds"
     remaining))
  (message "spofy: fetching devices...")
  (spofy-api-get
   "me/player/devices" nil
   (lambda (data)
     (let* ((devices (and data (alist-get 'devices data)))
            (candidates
             (when (and devices (> (length devices) 0))
               (mapcar #'spofy-consult--format-device
                       (append devices nil)))))
       (cond
        ((null data)
         (user-error "spofy: Device lookup failed; check *Messages* for the API error"))
        ((null candidates)
         (user-error "spofy: No devices found"))
        (t
         (let ((selected
                (consult--read candidates
                 :prompt "spofy device: "
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
                              (message "spofy: transferred playback to %s"
                                       device-name)))))))))))

;;;; Context track source (jump to track in current playback)

(defun spofy-consult--context-uri (context-type context-id)
  "Return a Spotify context URI for CONTEXT-TYPE and CONTEXT-ID."
  (format "spotify:%s:%s" context-type context-id))

(defun spofy-consult--fetch-context-tracks (context-type context-id callback)
  "Fetch tracks for CONTEXT-TYPE and CONTEXT-ID, then call CALLBACK."
  (require 'spofy-browse)
  (let* ((context-uri (spofy-consult--context-uri context-type context-id))
         (endpoint (pcase context-type
                     ("album" "albums")
                     ("playlist" "playlists")))
         (path (format "%s/%s/tracks" endpoint context-id)))
    (if-let* ((cached (spofy-browse--cache-get context-uri)))
        (funcall callback cached)
      (spofy-api-get
       path '(("limit" . "50"))
       (lambda (response)
         (let* ((first (append (alist-get 'items response) nil))
                (next (let ((n (alist-get 'next response)))
                        (and (stringp n) n))))
           (if next
               (spofy-browse--fetch-all-pages
                first next
                (lambda (all)
                  (spofy-browse--cache-put context-uri all)
                  (funcall callback all)))
             (spofy-browse--cache-put context-uri first)
             (funcall callback first))))))))

;;;###autoload
(defun spofy-library-search-context ()
  "Pick a track from the current playback context and play it.
Fetches the tracklist of the currently playing album or playlist
and presents all tracks for selection with narrowing.  Signals an
error when the context is unsupported (e.g. Spotify-generated
radio or daily mixes)."
  (interactive)
  (spofy-consult--ensure-available)
  (require 'spofy-player)
  (spofy-player--with-state
   (lambda ()
     (let* ((context-uri (alist-get 'context-uri spofy-player--current-state))
            (context-type (alist-get 'context-type spofy-player--current-state))
            (context-id (and context-uri
                             (car (last (split-string context-uri ":"))))))
       (unless (and context-id (member context-type '("album" "playlist")))
         (user-error "spofy: Current tracks not available for this context"))
       (spofy-consult--fetch-context-tracks
        context-type context-id
        (lambda (raw-items)
          (let* ((playlist-p (equal context-type "playlist"))
                 (candidates
                  (when raw-items
                    (cl-loop for item in raw-items
                             for track = (if playlist-p (alist-get 'track item) item)
                             when track collect (spofy-consult--format-track track)))))
            (unless candidates
              (user-error "spofy: Current tracks not available for this context"))
            (let ((selected
                   (consult--read candidates
                    :prompt "Jump to track: "
                    :category 'spofy-track
                    :sort nil
                    :require-match t
                    :lookup #'consult--lookup-member)))
              (when-let* ((entity (spofy-consult--get-entity selected))
                          (uri (alist-get 'uri entity)))
                (spofy-play-track
                 uri context-uri
                 (spofy-consult--jump-position raw-items playlist-p uri)))))))))))

(defun spofy-consult--jump-position (raw-items playlist-p uri)
  "Return the 0-indexed position of URI in RAW-ITEMS.
PLAYLIST-P indicates whether RAW-ITEMS wraps each track in a \"track\"
field (true for playlists).  Returns nil when URI is not found."
  (cl-loop for item in raw-items
           for index from 0
           for track = (if playlist-p (alist-get 'track item) item)
           when (and track (equal (alist-get 'uri track) uri))
           return index))

(provide 'spofy-consult)
;;; spofy-consult.el ends here
