;;; spofy-browse.el --- Album, artist, and playlist views for Spofy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;; Author: Pablo Stafforini
;; URL: https://github.com/pablostafforini/spofy
;; Package-Requires: ((emacs "30.1"))

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

;; Album, artist, and playlist detail views for Spofy.  Each view fetches
;; data from the Spotify Web API and displays it in a `tabulated-list-mode'
;; buffer with appropriate keybindings for playback and navigation.

;;; Code:

(require 'spofy-api)
(require 'spofy-ui)
(require 'cl-lib)

;; Functions from other Spofy modules that may not be loaded yet.
(declare-function spofy-player-current-track-id "spofy-player" ())
(declare-function spofy-play-track "spofy-player" (track-uri &optional context-uri))
(declare-function spofy-play-context "spofy-player" (context-uri))
(declare-function spofy-playlist-remove-track "spofy-playlist" (playlist-id track-uri))
(declare-function spofy-library-save "spofy-library" (type id))
(declare-function spofy-library-unsave "spofy-library" (type id))

;;;; Entity storage

(defvar-local spofy-browse--entities nil
  "Hash table mapping entity URIs to their full API alists.
Buffer-local in every Spofy browse buffer.")

(defun spofy-browse--store-entity (uri entity)
  "Store ENTITY alist under URI in the buffer-local entity table."
  (unless spofy-browse--entities
    (setq spofy-browse--entities (make-hash-table :test #'equal)))
  (puthash uri entity spofy-browse--entities))

(defun spofy-browse-entity-at-point ()
  "Return the full alist for the entity on the current tabulated-list row.
The row ID is used as the key into `spofy-browse--entities'."
  (when-let* ((entry (tabulated-list-get-entry))
              (id (tabulated-list-get-id)))
    (and spofy-browse--entities
         (gethash id spofy-browse--entities))))

;;;; Now-playing indicator

(defun spofy-browse--playing-indicator (track-uri)
  "Return the now-playing indicator string for TRACK-URI.
Shows a play icon if TRACK-URI matches the currently playing track."
  (let* ((current-id (and (fboundp 'spofy-player-current-track-id)
                          (spofy-player-current-track-id)))
         (playing-p (and current-id
                         (string-match-p (regexp-quote current-id) track-uri))))
    (if playing-p
        (propertize "\u25B6" 'face 'spofy-playing-icon)
      "")))

;;;; Helpers

(defun spofy-browse--extract-id-from-uri (uri)
  "Extract the Spotify ID from URI (e.g. \"spotify:album:xyz\" -> \"xyz\")."
  (if (string-match "spotify:[^:]+:\\(.+\\)" uri)
      (match-string 1 uri)
    uri))

(defun spofy-browse--album-year (album)
  "Extract the release year from ALBUM alist."
  (let ((date (or (alist-get 'release_date album) "")))
    (if (string-match "\\`\\([0-9]\\{4\\}\\)" date)
        (match-string 1 date)
      date)))

;;; ========================================================================
;;;; Album view
;;; ========================================================================

(defvar spofy-album-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'spofy-album-play-track)
    (define-key map (kbd "a")   #'spofy-album-play)
    (define-key map (kbd "s")   #'spofy-album-save)
    (define-key map (kbd "A")   #'spofy-album-view-artist)
    (define-key map (kbd "g")   #'spofy-album-refresh)
    (define-key map (kbd "q")   #'quit-window)
    (define-key map (kbd "m")   #'spofy-album-load-more)
    map)
  "Keymap for `spofy-album-mode'.")

(define-derived-mode spofy-album-mode tabulated-list-mode "Spofy Album"
  "Major mode for viewing a Spotify album's tracks."
  :group 'spofy
  (setq tabulated-list-format
        (vector '("#"        4 nil :right-align t)
                `("Name"     ,(spofy-ui-col 'album-track 0) t)
                `("Artist(s)" ,(spofy-ui-col 'album-track 1) t)
                '("Duration"  6 nil :right-align t))
        tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun spofy-album--format-track (track album-uri)
  "Format TRACK alist as a tabulated-list entry.
ALBUM-URI is the album context URI for playback."
  (let* ((uri (alist-get 'uri track))
         (name (or (alist-get 'name track) ""))
         (track-number (alist-get 'track_number track))
         (artists (alist-get 'artists track))
         (duration (alist-get 'duration_ms track))
         (playing (spofy-browse--playing-indicator uri))
         (num-str (if track-number (number-to-string track-number) ""))
         (artists-str (if artists (spofy-ui-format-artists artists) ""))
         (dur-str (if duration (spofy-ui-format-duration-ms duration) "")))
    (spofy-browse--store-entity uri track)
    ;; Store album-uri in buffer context for playback
    (setq-local spofy-ui--buffer-context
                (cons (cons 'album-uri album-uri)
                      (assq-delete-all 'album-uri spofy-ui--buffer-context)))
    (list uri
          (vector (if (string-empty-p playing) num-str
                    (propertize num-str 'face 'spofy-playing))
                  (if (string-empty-p playing)
                      (propertize (spofy-ui-truncate name (spofy-ui-col 'album-track 0)) 'face 'spofy-track-name)
                    (spofy-ui-propertize-playing (spofy-ui-truncate name (spofy-ui-col 'album-track 0))))
                  (if (string-empty-p playing)
                      (propertize (spofy-ui-truncate artists-str (spofy-ui-col 'album-track 1)) 'face 'spofy-artist-name)
                    (spofy-ui-propertize-playing (spofy-ui-truncate artists-str (spofy-ui-col 'album-track 1))))
                  (propertize dur-str 'face 'spofy-muted)))))

(defun spofy-album--render (album)
  "Render ALBUM data into the current buffer."
  (let* ((name (or (alist-get 'name album) "Unknown Album"))
         (artists (alist-get 'artists album))
         (year (spofy-browse--album-year album))
         (tracks-obj (alist-get 'tracks album))
         (total (alist-get 'total tracks-obj))
         (tracks (alist-get 'items tracks-obj))
         (next-url (alist-get 'next tracks-obj))
         (uri (alist-get 'uri album))
         (artist-str (if artists (spofy-ui-format-artists artists) "Unknown"))
         (buf-name (format "*Spofy Album: %s*" name)))
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (spofy-album-mode)
      (setq-local spofy-ui--buffer-context
                  `((album-id . ,(alist-get 'id album))
                    (album-uri . ,uri)
                    (album . ,album)))
      (setq-local spofy-ui--next-page-url next-url)
      (setq-local spofy-ui--entity-type 'track)
      (spofy-ui-insert-header
       (list (format "Album: %s" name)
             (format "Artist(s): %s" artist-str)
             (format "Year: %s" year)
             (format "Tracks: %s" (or total "?"))))
      (setq tabulated-list-entries
            (cl-loop for track across tracks
                     collect (spofy-album--format-track track uri)))
      (tabulated-list-print t)
      (goto-char (point-min))
      (switch-to-buffer (current-buffer)))))

;;;###autoload
(defun spofy-view-album (album-id)
  "View the Spotify album identified by ALBUM-ID.
Fetches album data from the API and displays it in a tabulated-list buffer."
  (interactive
   (list (read-string "Spotify album ID: ")))
  (spofy-api-get (format "albums/%s" album-id) nil
                 #'spofy-album--render))

(defun spofy-album-play-track ()
  "Play the track at point in the album context."
  (interactive)
  (when-let* ((uri (tabulated-list-get-id))
              (album-uri (alist-get 'album-uri spofy-ui--buffer-context)))
    (spofy-play-track uri album-uri)))

(defun spofy-album-play ()
  "Play the entire album."
  (interactive)
  (when-let* ((album-uri (alist-get 'album-uri spofy-ui--buffer-context)))
    (spofy-play-context album-uri)))

(defun spofy-album-save ()
  "Save the album to the user's library."
  (interactive)
  (when-let* ((album-id (alist-get 'album-id spofy-ui--buffer-context)))
    (spofy-library-save "albums" album-id)))

(defun spofy-album-view-artist ()
  "View the artist of the track at point."
  (interactive)
  (when-let* ((entity (spofy-browse-entity-at-point))
              (artists (alist-get 'artists entity))
              (artist (aref artists 0))
              (artist-id (alist-get 'id artist)))
    (spofy-view-artist artist-id)))

(defun spofy-album-refresh ()
  "Refresh the album view."
  (interactive)
  (when-let* ((album-id (alist-get 'album-id spofy-ui--buffer-context)))
    (spofy-view-album album-id)))

(defun spofy-album-load-more ()
  "Load the next page of tracks for this album."
  (interactive)
  (if (null spofy-ui--next-page-url)
      (message "Spofy: no more tracks to load.")
    (let ((album-uri (alist-get 'album-uri spofy-ui--buffer-context))
          (buf (current-buffer)))
      (spofy-api--request
       "GET" spofy-ui--next-page-url nil nil
       (lambda (response)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (let* ((tracks (alist-get 'items response))
                    (next-url (alist-get 'next response))
                    (new-entries
                     (cl-loop for track across tracks
                              collect (spofy-album--format-track track album-uri))))
               (setq-local spofy-ui--next-page-url next-url)
               (setq tabulated-list-entries
                     (append tabulated-list-entries new-entries))
               (tabulated-list-print t)
               (message "Spofy: loaded %d more tracks." (length tracks))))))))))

;;; ========================================================================
;;;; Artist view
;;; ========================================================================

(defvar spofy-artist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'spofy-artist-view-album)
    (define-key map (kbd "t")   #'spofy-artist-top-tracks)
    (define-key map (kbd "a")   #'spofy-artist-play-album)
    (define-key map (kbd "g")   #'spofy-artist-refresh)
    (define-key map (kbd "q")   #'quit-window)
    (define-key map (kbd "m")   #'spofy-artist-load-more)
    map)
  "Keymap for `spofy-artist-mode'.")

(define-derived-mode spofy-artist-mode tabulated-list-mode "Spofy Artist"
  "Major mode for viewing a Spotify artist's albums."
  :group 'spofy
  (setq tabulated-list-format
        (vector `("Name"   ,(spofy-ui-col 'artist-album 0) t)
                '("Year"    6 t)
                '("Type"    8 t)
                '("Tracks"  6 nil :right-align t))
        tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun spofy-artist--format-album (album)
  "Format ALBUM alist as a tabulated-list entry for the artist view."
  (let* ((uri (alist-get 'uri album))
         (name (or (alist-get 'name album) ""))
         (year (spofy-browse--album-year album))
         (album-type (or (alist-get 'album_type album) ""))
         (total-tracks (alist-get 'total_tracks album)))
    (spofy-browse--store-entity uri album)
    (list uri
          (vector (propertize (spofy-ui-truncate name (spofy-ui-col 'artist-album 0)) 'face 'spofy-album-name)
                  (propertize year 'face 'spofy-muted)
                  album-type
                  (propertize (if total-tracks
                                  (number-to-string total-tracks)
                                "")
                              'face 'spofy-muted)))))

(defun spofy-artist--render (artist-id artist albums-response)
  "Render ARTIST and ALBUMS-RESPONSE into the artist buffer.
ARTIST-ID is the Spotify artist ID."
  (let* ((name (or (alist-get 'name artist) "Unknown Artist"))
         (genres (alist-get 'genres artist))
         (followers-obj (alist-get 'followers artist))
         (followers (if followers-obj (alist-get 'total followers-obj) 0))
         (albums (alist-get 'items albums-response))
         (next-url (alist-get 'next albums-response))
         (genres-str (if (and genres (> (length genres) 0))
                         (mapconcat #'identity genres ", ")
                       "None listed"))
         (buf-name (format "*Spofy Artist: %s*" name)))
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (spofy-artist-mode)
      (setq-local spofy-ui--buffer-context
                  `((artist-id . ,artist-id)
                    (artist-name . ,name)
                    (artist . ,artist)))
      (setq-local spofy-ui--next-page-url next-url)
      (setq-local spofy-ui--entity-type 'album)
      (spofy-ui-insert-header
       (list (format "Artist: %s" name)
             (format "Genres: %s" genres-str)
             (format "Followers: %s"
                     (number-to-string followers))))
      (setq tabulated-list-entries
            (cl-loop for album across albums
                     collect (spofy-artist--format-album album)))
      (tabulated-list-print t)
      (goto-char (point-min))
      (switch-to-buffer (current-buffer)))))

;;;###autoload
(defun spofy-view-artist (artist-id)
  "View the Spotify artist identified by ARTIST-ID.
Fetches artist info and albums, then displays in a tabulated-list buffer."
  (interactive
   (list (read-string "Spotify artist ID: ")))
  ;; We need to fetch both artist info and albums.  We fire both requests
  ;; and render when both have completed.
  (let ((artist-data nil)
        (albums-data nil))
    (spofy-api-get (format "artists/%s" artist-id) nil
                   (lambda (artist)
                     (setq artist-data artist)
                     (when albums-data
                       (spofy-artist--render artist-id artist-data albums-data))))
    (spofy-api-get (format "artists/%s/albums" artist-id)
                   '(("include_groups" . "album,single")
                     ("limit" . "20"))
                   (lambda (albums)
                     (setq albums-data albums)
                     (when artist-data
                       (spofy-artist--render artist-id artist-data albums-data))))))

(defun spofy-artist-view-album ()
  "View the album at point."
  (interactive)
  (when-let* ((uri (tabulated-list-get-id))
              (album-id (spofy-browse--extract-id-from-uri uri)))
    (spofy-view-album album-id)))

(defun spofy-artist-play-album ()
  "Play the album at point."
  (interactive)
  (when-let* ((uri (tabulated-list-get-id)))
    (spofy-play-context uri)))

(defun spofy-artist-top-tracks ()
  "View the top tracks of the current artist."
  (interactive)
  (when-let* ((artist-id (alist-get 'artist-id spofy-ui--buffer-context))
              (artist-name (alist-get 'artist-name spofy-ui--buffer-context)))
    (spofy-view-artist-top-tracks artist-id artist-name)))

(defun spofy-artist-refresh ()
  "Refresh the artist view."
  (interactive)
  (when-let* ((artist-id (alist-get 'artist-id spofy-ui--buffer-context)))
    (spofy-view-artist artist-id)))

(defun spofy-artist-load-more ()
  "Load the next page of albums for this artist."
  (interactive)
  (if (null spofy-ui--next-page-url)
      (message "Spofy: no more albums to load.")
    (let ((buf (current-buffer)))
      (spofy-api--request
       "GET" spofy-ui--next-page-url nil nil
       (lambda (response)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (let* ((albums (alist-get 'items response))
                    (next-url (alist-get 'next response))
                    (new-entries
                     (cl-loop for album across albums
                              collect (spofy-artist--format-album album))))
               (setq-local spofy-ui--next-page-url next-url)
               (setq tabulated-list-entries
                     (append tabulated-list-entries new-entries))
               (tabulated-list-print t)
               (message "Spofy: loaded %d more albums."
                        (length albums))))))))))

;;; ========================================================================
;;;; Artist top tracks
;;; ========================================================================

(defvar spofy-artist-top-tracks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'spofy-top-tracks-play)
    (define-key map (kbd "a")   #'spofy-top-tracks-view-album)
    (define-key map (kbd "s")   #'spofy-top-tracks-save)
    (define-key map (kbd "g")   #'spofy-top-tracks-refresh)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `spofy-artist-top-tracks-mode'.")

(define-derived-mode spofy-artist-top-tracks-mode tabulated-list-mode
  "Spofy Top Tracks"
  "Major mode for viewing an artist's top tracks."
  :group 'spofy
  (setq tabulated-list-format
        (vector '(" "        2 nil)
                `("Name"     ,(spofy-ui-col 'artist-top-track 0) t)
                `("Album"    ,(spofy-ui-col 'artist-top-track 1) t)
                '("Duration"  6 nil :right-align t))
        tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun spofy-top-tracks--format-track (track)
  "Format TRACK alist as a tabulated-list entry for top tracks."
  (let* ((uri (alist-get 'uri track))
         (name (or (alist-get 'name track) ""))
         (album (alist-get 'album track))
         (album-name (if album (or (alist-get 'name album) "") ""))
         (duration (alist-get 'duration_ms track))
         (playing (spofy-browse--playing-indicator uri))
         (dur-str (if duration (spofy-ui-format-duration-ms duration) "")))
    (spofy-browse--store-entity uri track)
    (list uri
          (vector playing
                  (if (string-empty-p playing)
                      (propertize (spofy-ui-truncate name (spofy-ui-col 'artist-top-track 0)) 'face 'spofy-track-name)
                    (spofy-ui-propertize-playing (spofy-ui-truncate name (spofy-ui-col 'artist-top-track 0))))
                  (if (string-empty-p playing)
                      (propertize (spofy-ui-truncate album-name (spofy-ui-col 'artist-top-track 1)) 'face 'spofy-album-name)
                    (spofy-ui-propertize-playing (spofy-ui-truncate album-name (spofy-ui-col 'artist-top-track 1))))
                  (propertize dur-str 'face 'spofy-muted)))))

(defun spofy-top-tracks--render (artist-id artist-name tracks)
  "Render top TRACKS for artist ARTIST-NAME in a buffer.
ARTIST-ID is kept for refresh."
  (let ((buf-name (format "*Spofy Top Tracks: %s*" artist-name)))
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (spofy-artist-top-tracks-mode)
      (setq-local spofy-ui--buffer-context
                  `((artist-id . ,artist-id)
                    (artist-name . ,artist-name)))
      (setq-local spofy-ui--entity-type 'track)
      (spofy-ui-insert-header
       (list (format "Top tracks: %s" artist-name)))
      (setq tabulated-list-entries
            (cl-loop for track across tracks
                     collect (spofy-top-tracks--format-track track)))
      (tabulated-list-print t)
      (goto-char (point-min))
      (switch-to-buffer (current-buffer)))))

(defun spofy-view-artist-top-tracks (artist-id artist-name)
  "Fetch and display top tracks for ARTIST-ID (displayed as ARTIST-NAME)."
  (spofy-api-get (format "artists/%s/top-tracks" artist-id) nil
                 (lambda (response)
                   (let ((tracks (alist-get 'tracks response)))
                     (spofy-top-tracks--render artist-id artist-name tracks)))))

(defun spofy-top-tracks-play ()
  "Play the track at point."
  (interactive)
  (when-let* ((uri (tabulated-list-get-id)))
    (spofy-play-track uri nil)))

(defun spofy-top-tracks-view-album ()
  "View the album of the track at point."
  (interactive)
  (when-let* ((entity (spofy-browse-entity-at-point))
              (album (alist-get 'album entity))
              (album-id (alist-get 'id album)))
    (spofy-view-album album-id)))

(defun spofy-top-tracks-save ()
  "Save the track at point to the user's library."
  (interactive)
  (when-let* ((uri (tabulated-list-get-id))
              (track-id (spofy-browse--extract-id-from-uri uri)))
    (spofy-library-save "tracks" track-id)))

(defun spofy-top-tracks-refresh ()
  "Refresh the top tracks view."
  (interactive)
  (when-let* ((artist-id (alist-get 'artist-id spofy-ui--buffer-context))
              (artist-name (alist-get 'artist-name spofy-ui--buffer-context)))
    (spofy-view-artist-top-tracks artist-id artist-name)))

;;; ========================================================================
;;;; Playlist detail view
;;; ========================================================================

(defvar spofy-playlist-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'spofy-playlist-view-play-track)
    (define-key map (kbd "a")   #'spofy-playlist-view-album)
    (define-key map (kbd "A")   #'spofy-playlist-view-artist)
    (define-key map (kbd "d")   #'spofy-playlist-view-remove-track)
    (define-key map (kbd "s")   #'spofy-playlist-view-save-track)
    (define-key map (kbd "g")   #'spofy-playlist-view-refresh)
    (define-key map (kbd "q")   #'quit-window)
    (define-key map (kbd "m")   #'spofy-playlist-view-load-more)
    map)
  "Keymap for `spofy-playlist-view-mode'.")

(define-derived-mode spofy-playlist-view-mode tabulated-list-mode
  "Spofy Playlist"
  "Major mode for viewing a Spotify playlist's tracks."
  :group 'spofy
  (setq tabulated-list-format
        (vector '(" "          2 nil)
                `("Name"       ,(spofy-ui-col 'playlist-track 0) t)
                `("Artist(s)"  ,(spofy-ui-col 'playlist-track 1) t)
                `("Album"      ,(spofy-ui-col 'playlist-track 2) t)
                '("Duration"    6 nil :right-align t)
                `("Added By"   ,(spofy-ui-col 'playlist-track 3) t)
                '("Date Added" 12 t))
        tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun spofy-playlist--format-added-at (added-at)
  "Format ADDED-AT timestamp as YYYY-MM-DD."
  (if (and added-at (stringp added-at) (>= (length added-at) 10))
      (substring added-at 0 10)
    ""))

(defun spofy-playlist--format-track-item (item playlist-uri)
  "Format a playlist track ITEM (wrapper with `track' and `added_by').
PLAYLIST-URI is the playlist context URI for playback."
  (let* ((track (alist-get 'track item))
         (added-by-obj (alist-get 'added_by item))
         (added-by (if added-by-obj
                       (or (alist-get 'id added-by-obj) "")
                     ""))
         (added-at (or (alist-get 'added_at item) ""))
         (uri (if track (alist-get 'uri track) nil)))
    ;; Protect against nil tracks (local files, etc.)
    (when (and track uri)
      (let* ((name (or (alist-get 'name track) ""))
             (artists (alist-get 'artists track))
             (album (alist-get 'album track))
             (album-name (if album (or (alist-get 'name album) "") ""))
             (duration (alist-get 'duration_ms track))
             (playing (spofy-browse--playing-indicator uri))
             (artists-str (if artists (spofy-ui-format-artists artists) ""))
             (dur-str (if duration (spofy-ui-format-duration-ms duration) ""))
             (date-str (spofy-playlist--format-added-at added-at)))
        ;; Store the full wrapper so we have added_by and track data.
        (spofy-browse--store-entity uri item)
        ;; Store playlist-uri for playback context.
        (ignore playlist-uri)
        (list uri
              (vector playing
                      (if (string-empty-p playing)
                          (propertize (spofy-ui-truncate name (spofy-ui-col 'playlist-track 0)) 'face 'spofy-track-name)
                        (spofy-ui-propertize-playing (spofy-ui-truncate name (spofy-ui-col 'playlist-track 0))))
                      (if (string-empty-p playing)
                          (propertize (spofy-ui-truncate artists-str (spofy-ui-col 'playlist-track 1)) 'face 'spofy-artist-name)
                        (spofy-ui-propertize-playing (spofy-ui-truncate artists-str (spofy-ui-col 'playlist-track 1))))
                      (if (string-empty-p playing)
                          (propertize (spofy-ui-truncate album-name (spofy-ui-col 'playlist-track 2)) 'face 'spofy-album-name)
                        (spofy-ui-propertize-playing (spofy-ui-truncate album-name (spofy-ui-col 'playlist-track 2))))
                      (propertize dur-str 'face 'spofy-muted)
                      (spofy-ui-truncate added-by (spofy-ui-col 'playlist-track 3))
                      (propertize date-str 'face 'spofy-muted)))))))

(defun spofy-playlist--render (playlist)
  "Render PLAYLIST data into a buffer."
  (let* ((name (or (alist-get 'name playlist) "Unknown Playlist"))
         (owner-obj (alist-get 'owner playlist))
         (owner (if owner-obj
                    (or (alist-get 'display_name owner-obj)
                        (alist-get 'id owner-obj))
                  "Unknown"))
         (description (or (alist-get 'description playlist) ""))
         (tracks-obj (alist-get 'tracks playlist))
         (total (alist-get 'total tracks-obj))
         (items (alist-get 'items tracks-obj))
         (next-url (alist-get 'next tracks-obj))
         (uri (alist-get 'uri playlist))
         (playlist-id (alist-get 'id playlist))
         (buf-name (format "*Spofy Playlist: %s*" name)))
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (spofy-playlist-view-mode)
      (setq-local spofy-ui--buffer-context
                  `((playlist-id . ,playlist-id)
                    (playlist-uri . ,uri)
                    (playlist . ,playlist)))
      (setq-local spofy-ui--next-page-url next-url)
      (setq-local spofy-ui--entity-type 'track)
      (spofy-ui-insert-header
       (list (format "Playlist: %s" name)
             (format "Owner: %s" owner)
             (format "Description: %s" description)
             (format "Tracks: %s" (or total "?"))))
      (setq tabulated-list-entries
            (cl-loop for item across items
                     for entry = (spofy-playlist--format-track-item item uri)
                     when entry collect entry))
      (tabulated-list-print t)
      (goto-char (point-min))
      (switch-to-buffer (current-buffer)))))

;;;###autoload
(defun spofy-view-playlist (playlist-id)
  "View the Spotify playlist identified by PLAYLIST-ID.
Fetches playlist data from the API and displays it in a tabulated-list buffer."
  (interactive
   (list (read-string "Spotify playlist ID: ")))
  (spofy-api-get (format "playlists/%s" playlist-id) nil
                 #'spofy-playlist--render))

(defun spofy-playlist-view-play-track ()
  "Play the track at point in the playlist context."
  (interactive)
  (when-let* ((uri (tabulated-list-get-id))
              (playlist-uri (alist-get 'playlist-uri spofy-ui--buffer-context)))
    (spofy-play-track uri playlist-uri)))

(defun spofy-playlist-view-album ()
  "View the album of the track at point."
  (interactive)
  (when-let* ((entity (spofy-browse-entity-at-point))
              (track (or (alist-get 'track entity) entity))
              (album (alist-get 'album track))
              (album-id (alist-get 'id album)))
    (spofy-view-album album-id)))

(defun spofy-playlist-view-artist ()
  "View the artist of the track at point."
  (interactive)
  (when-let* ((entity (spofy-browse-entity-at-point))
              (track (or (alist-get 'track entity) entity))
              (artists (alist-get 'artists track))
              (artist (aref artists 0))
              (artist-id (alist-get 'id artist)))
    (spofy-view-artist artist-id)))

(defun spofy-playlist-view-remove-track ()
  "Remove the track at point from the playlist."
  (interactive)
  (when-let* ((uri (tabulated-list-get-id))
              (playlist-id (alist-get 'playlist-id spofy-ui--buffer-context)))
    (spofy-playlist-remove-track playlist-id uri)))

(defun spofy-playlist-view-save-track ()
  "Save the track at point to the user's library."
  (interactive)
  (when-let* ((uri (tabulated-list-get-id))
              (track-id (spofy-browse--extract-id-from-uri uri)))
    (spofy-library-save "tracks" track-id)))

(defun spofy-playlist-view-refresh ()
  "Refresh the playlist view."
  (interactive)
  (when-let* ((playlist-id (alist-get 'playlist-id spofy-ui--buffer-context)))
    (spofy-view-playlist playlist-id)))

(defun spofy-playlist-view-load-more ()
  "Load the next page of tracks for this playlist."
  (interactive)
  (if (null spofy-ui--next-page-url)
      (message "Spofy: no more tracks to load.")
    (let ((playlist-uri (alist-get 'playlist-uri spofy-ui--buffer-context))
          (buf (current-buffer)))
      (spofy-api--request
       "GET" spofy-ui--next-page-url nil nil
       (lambda (response)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (let* ((items (alist-get 'items response))
                    (next-url (alist-get 'next response))
                    (new-entries
                     (cl-loop for item across items
                              for entry = (spofy-playlist--format-track-item
                                           item playlist-uri)
                              when entry collect entry)))
               (setq-local spofy-ui--next-page-url next-url)
               (setq tabulated-list-entries
                     (append tabulated-list-entries new-entries))
               (tabulated-list-print t)
               (message "Spofy: loaded %d more tracks."
                        (length new-entries))))))))))

(provide 'spofy-browse)
;;; spofy-browse.el ends here
