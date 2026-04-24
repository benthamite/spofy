;;; spofy-browse.el --- Album, artist, and playlist views for spofy  -*- lexical-binding: t; -*-

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

;; Album, artist, and playlist detail views for spofy.  Each view fetches
;; data from the Spotify Web API and displays it in a `tabulated-list-mode'
;; buffer with appropriate keybindings for playback and navigation.

;;; Code:

(require 'spofy-api)
(require 'spofy-ui)
(require 'cl-lib)

;; Functions from other spofy modules that may not be loaded yet.
(declare-function spofy-play-track "spofy-player" (track-uri &optional context-uri))
(declare-function spofy-play-context "spofy-player" (context-uri))
(declare-function spofy-playlist-remove-track "spofy-playlist" (&optional playlist-id track-uri))
(declare-function spofy-library-save "spofy-library" (uri-or-type &optional id))
(declare-function spofy-follow-artist "spofy-library" (artist-id))
(declare-function spofy-unfollow-artist "spofy-library" (artist-id))
(declare-function spofy-add-to-queue "spofy-queue" (uri))
(declare-function spofy-library-unsave "spofy-library" (uri-or-type &optional id))
(declare-function spofy-follow-playlist "spofy-playlist" (playlist-id-or-uri))

;;; ========================================================================
;;;; Track item cache
;;; ========================================================================

(defvar spofy-browse--track-cache (make-hash-table :test 'equal)
  "Cache mapping context URIs to complete track item lists.
Populated after the first full fetch of a playlist or album.
Invalidated on refresh or mutation (add/remove/reorder).")

(defun spofy-browse--cache-get (context-uri)
  "Return cached track items for CONTEXT-URI, or nil."
  (gethash context-uri spofy-browse--track-cache))

(defun spofy-browse--cache-put (context-uri items)
  "Store track ITEMS for CONTEXT-URI in the cache."
  (puthash context-uri items spofy-browse--track-cache))

(defun spofy-browse--cache-invalidate (context-uri)
  "Remove CONTEXT-URI from the track cache."
  (remhash context-uri spofy-browse--track-cache))

;;; ========================================================================
;;;; Shared helpers
;;; ========================================================================

(defun spofy-browse--fetch-all-pages (collected next-url callback)
  "Fetch all remaining pages starting from NEXT-URL.
COLLECTED is the list of items accumulated so far.
CALLBACK is called with the complete item list when done."
  (spofy-api--request
   "GET" next-url nil nil
   (lambda (response)
     (let* ((items (append (alist-get 'items response) nil))
            (next (let ((n (alist-get 'next response)))
                    (and (stringp n) n)))
            (all (append collected items)))
       (if next
           (spofy-browse--fetch-all-pages all next callback)
         (funcall callback all))))))

;;; ========================================================================
;;;; Album view
;;; ========================================================================

(defvar spofy-album-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'spofy-album-play-track)
    (define-key map (kbd "a")   #'spofy-album-play)
    (define-key map (kbd "s")   #'spofy-album-save)
    (define-key map (kbd "A")   #'spofy-album-view-artist)
    (define-key map (kbd "Q")   #'spofy-add-to-queue)
    (define-key map (kbd "SPC") #'spofy-play-pause)
    (define-key map (kbd "g")   #'spofy-album-refresh)
    (define-key map (kbd "q")   #'quit-window)

    map)
  "Keymap for `spofy-album-mode'.")

(define-derived-mode spofy-album-mode tabulated-list-mode "spofy Album"
  "Major mode for viewing a Spotify album's tracks."
  :group 'spofy
  (setq tabulated-list-padding 2)
  (spofy-ui-set-format
   'album-track
   '(("#"        4 nil :right-align t)
     ("Name"     :flex t)
     ("Artist(s)" :flex t)
     ("Duration"  6 nil :right-align t)))
  (tabulated-list-init-header))

(defun spofy-album--format-track (track album-uri multi-disc-p)
  "Format TRACK alist as a tabulated-list entry.
ALBUM-URI is the album context URI for playback.
When MULTI-DISC-P is non-nil, show disc.track numbering."
  (let* ((uri (alist-get 'uri track))
         (name (or (alist-get 'name track) ""))
         (track-number (alist-get 'track_number track))
         (disc-number (alist-get 'disc_number track))
         (artists (alist-get 'artists track))
         (duration (alist-get 'duration_ms track))
         (playing (spofy-ui-playing-indicator uri))
         (num-str (cond
                   ((not track-number) "")
                   (multi-disc-p
                    (format "%d.%d" (or disc-number 1) track-number))
                   (t (number-to-string track-number))))
         (artists-str (if artists (spofy-ui-format-artists artists) ""))
         (dur-str (if duration (spofy-ui-format-duration-ms duration) "")))
    (spofy-ui-store-entity uri track)
    (let ((playing-p (not (string-empty-p playing))))
      (list uri
            (vector (if playing-p
                        (propertize num-str 'face 'spofy-playing)
                      num-str)
                    (spofy-ui-truncate name (spofy-ui-col 'album-track 0)
                                       (spofy-ui-playing-face 'spofy-track-name playing-p))
                    (spofy-ui-truncate artists-str (spofy-ui-col 'album-track 1)
                                       (spofy-ui-playing-face 'spofy-artist-name playing-p))
                    (propertize dur-str 'face
                                (spofy-ui-playing-face 'spofy-muted playing-p)))))))

(defun spofy-album--multi-disc-p (tracks)
  "Return non-nil if TRACKS (a vector) span more than one disc."
  (cl-loop for track across tracks
           for disc = (alist-get 'disc_number track)
           thereis (and disc (> disc 1))))

(defun spofy-album--multi-disc-p-list (tracks)
  "Return non-nil if TRACKS (a list) span more than one disc."
  (cl-loop for track in tracks
           for disc = (alist-get 'disc_number track)
           thereis (and disc (> disc 1))))

(defun spofy-album--render (album)
  "Render ALBUM data into the current buffer.
Uses cached track items when available; otherwise fetches all
pages asynchronously before rendering."
  (let* ((name (or (alist-get 'name album) "Unknown Album"))
         (artists (alist-get 'artists album))
         (year (spofy-ui-album-year album))
         (tracks-obj (alist-get 'tracks album))
         (total (alist-get 'total tracks-obj))
         (first-tracks (append (alist-get 'items tracks-obj) nil))
         (next-url (let ((n (alist-get 'next tracks-obj)))
                     (and (stringp n) n)))
         (uri (alist-get 'uri album))
         (album-id (alist-get 'id album))
         (artist-str (if artists (spofy-ui-format-artists artists) "Unknown"))
         (buf-name (format "*spofy Album: %s*" name))
         (cached (spofy-browse--cache-get uri)))
    (cond
     (cached
      (spofy-album--render-tracks
       cached buf-name album-id uri album
       name artist-str year (length cached)))
     (next-url
      (spofy-browse--fetch-all-pages
       first-tracks next-url
       (lambda (all-tracks)
         (spofy-browse--cache-put uri all-tracks)
         (spofy-album--render-tracks
          all-tracks buf-name album-id uri album
          name artist-str year total))))
     (t
      (spofy-browse--cache-put uri first-tracks)
      (spofy-album--render-tracks
       first-tracks buf-name album-id uri album
       name artist-str year total)))))

(defun spofy-album--render-tracks
    (tracks buf-name album-id uri album name artist-str year total)
  "Render all album TRACKS into BUF-NAME.
ALBUM-ID, URI, ALBUM, NAME, ARTIST-STR, YEAR, and TOTAL describe
the album."
  (let ((multi-disc (spofy-album--multi-disc-p-list tracks)))
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (spofy-album-mode)
      (when multi-disc
        (spofy-album--left-align-number-column))
      (setq-local spofy-ui--buffer-entity-name name)
      (setq-local spofy-ui--buffer-context
                  `((album-id . ,album-id)
                    (album-uri . ,uri)
                    (album . ,album)))
      (setq-local spofy-ui--entity-type 'track)
      (setq-local spofy-ui--entry-formatter
                  (let ((album-uri uri)
                        (multi-disc-p multi-disc))
                    (lambda (entity _idx)
                      (spofy-album--format-track entity album-uri multi-disc-p))))
      (spofy-ui-insert-header
       (list (format "Album: %s" name)
             (format "Artist(s): %s" artist-str)
             (format "Year: %s" year)
             (format "Tracks: %s" (or total "?"))))
      (setq tabulated-list-entries
            (cl-loop for track in tracks
                     collect (spofy-album--format-track track uri multi-disc)))
      (tabulated-list-print t)
      (goto-char (point-min))
      (switch-to-buffer (current-buffer)))))

(defun spofy-album--left-align-number-column ()
  "Remove right-alignment from the track number column.
Multi-disc albums use left-aligned =disc.track= notation so that
the period stays in a fixed position."
  (setq spofy-ui--format-columns
        (cons '("#" 4 nil)
              (cdr spofy-ui--format-columns)))
  (spofy-ui-set-format 'album-track spofy-ui--format-columns))

;;;###autoload
(defun spofy-view-album (album-id)
  "View the Spotify album identified by ALBUM-ID.
Fetches album data from the API and displays it in a tabulated-list buffer."
  (interactive
   (list (read-string "Spotify album ID: ")))
  (spofy-api-get (format "albums/%s" album-id) (spofy-api-with-market nil)
                 #'spofy-album--render))

(defun spofy-album-play-track ()
  "Play the track at point in the album context."
  (interactive)
  (when-let* ((uri (tabulated-list-get-id))
              (album-uri (alist-get 'album-uri spofy-ui--buffer-context)))
    (spofy-play-track uri album-uri (spofy-ui-row-position))))

(defun spofy-album-play ()
  "Play the entire album."
  (interactive)
  (when-let* ((album-uri (alist-get 'album-uri spofy-ui--buffer-context)))
    (spofy-play-context album-uri)))

(defun spofy-album-save ()
  "Save the album to the user's library."
  (interactive)
  (when-let* ((album-uri (or (alist-get 'album-uri spofy-ui--buffer-context)
                             (when-let* ((album-id (alist-get 'album-id spofy-ui--buffer-context)))
                               (format "spotify:album:%s" album-id)))))
    (spofy-library-save album-uri)))

(defun spofy-album-view-artist ()
  "View the artist of the track at point."
  (interactive)
  (when-let* ((entity (spofy-ui-entity-at-point))
              (artists (alist-get 'artists entity))
              (notempty (> (length artists) 0))
              (artist (aref artists 0))
              (artist-id (alist-get 'id artist)))
    (spofy-view-artist artist-id)))

(defun spofy-album-refresh ()
  "Refresh the album view, bypassing the track cache."
  (interactive)
  (when-let* ((uri (alist-get 'album-uri spofy-ui--buffer-context)))
    (spofy-browse--cache-invalidate uri))
  (when-let* ((album-id (alist-get 'album-id spofy-ui--buffer-context)))
    (spofy-view-album album-id)))

;;; ========================================================================
;;;; Artist view
;;; ========================================================================

(defvar spofy-artist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'spofy-artist-view-album)
    (define-key map (kbd "t")   #'spofy-artist-top-tracks)
    (define-key map (kbd "a")   #'spofy-artist-play-album)
    (define-key map (kbd "F")   #'spofy-follow-artist)
    (define-key map (kbd "U")   #'spofy-unfollow-artist)
    (define-key map (kbd "SPC") #'spofy-play-pause)
    (define-key map (kbd "g")   #'spofy-artist-refresh)
    (define-key map (kbd "q")   #'quit-window)

    map)
  "Keymap for `spofy-artist-mode'.")

(define-derived-mode spofy-artist-mode tabulated-list-mode "spofy Artist"
  "Major mode for viewing a Spotify artist's albums."
  :group 'spofy
  (setq tabulated-list-padding 2)
  (spofy-ui-set-format
   'artist-album
   '(("Name"    :flex t)
     ("Year"     6 t)
     ("Type"     8 t)
     ("Tracks"   6 nil :right-align t)))
  (tabulated-list-init-header))

(defun spofy-artist--format-album (album)
  "Format ALBUM alist as a tabulated-list entry for the artist view."
  (let* ((uri (alist-get 'uri album))
         (name (or (alist-get 'name album) ""))
         (year (spofy-ui-album-year album))
         (album-type (or (alist-get 'album_type album) ""))
         (total-tracks (alist-get 'total_tracks album)))
    (spofy-ui-store-entity uri album)
    (list uri
          (vector (spofy-ui-truncate name (spofy-ui-col 'artist-album 0) 'spofy-album-name)
                  (propertize year 'face 'spofy-muted)
                  album-type
                  (propertize (if total-tracks
                                  (number-to-string total-tracks)
                                "")
                              'face 'spofy-muted)))))

(defun spofy-artist--render (artist-id artist albums-response)
  "Render ARTIST and ALBUMS-RESPONSE into the artist buffer.
ARTIST-ID is the Spotify artist ID.  Uses cached album items
when available; otherwise fetches all pages asynchronously."
  (let* ((cache-key (format "spotify:artist:%s:albums" artist-id))
         (cached (spofy-browse--cache-get cache-key))
         (first-albums (append (alist-get 'items albums-response) nil))
         (next-url (let ((n (alist-get 'next albums-response)))
                     (and (stringp n) n))))
    (cond
     (cached
      (spofy-artist--render-albums cached artist-id artist))
     (next-url
      (spofy-browse--fetch-all-pages
       first-albums next-url
       (lambda (all-albums)
         (spofy-browse--cache-put cache-key all-albums)
         (spofy-artist--render-albums all-albums artist-id artist))))
     (t
      (spofy-browse--cache-put cache-key first-albums)
      (spofy-artist--render-albums first-albums artist-id artist)))))

(defun spofy-artist--render-albums (albums artist-id artist)
  "Render all ALBUMS for ARTIST-ID into the artist buffer.
ARTIST is the artist API object."
  (let* ((name (or (alist-get 'name artist) "Unknown Artist"))
         (genres (alist-get 'genres artist))
         (followers-obj (alist-get 'followers artist))
         (followers (if followers-obj (alist-get 'total followers-obj) 0))
         (genres-str (if (and genres (> (length genres) 0))
                         (mapconcat #'identity genres ", ")
                       "None listed"))
         (buf-name (format "*spofy Artist: %s*" name)))
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (spofy-artist-mode)
      (setq-local spofy-ui--buffer-entity-name name)
      (setq-local spofy-ui--buffer-context
                  `((artist-id . ,artist-id)
                    (artist-name . ,name)
                    (artist . ,artist)))
      (setq-local spofy-ui--entity-type 'album)
      (spofy-ui-insert-header
       (list (format "Artist: %s" name)
             (format "Genres: %s" genres-str)
             (format "Followers: %s"
                     (number-to-string followers))))
      (setq tabulated-list-entries
            (cl-loop for album in albums
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
              (album-id (spofy-ui-extract-id uri)))
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
  "Refresh the artist view, bypassing the cache."
  (interactive)
  (when-let* ((artist-id (alist-get 'artist-id spofy-ui--buffer-context)))
    (spofy-browse--cache-invalidate
     (format "spotify:artist:%s:albums" artist-id))
    (spofy-view-artist artist-id)))

;;; ========================================================================
;;;; Artist top tracks
;;; ========================================================================

(defvar spofy-artist-top-tracks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'spofy-artist-top-tracks-play)
    (define-key map (kbd "a")   #'spofy-artist-top-tracks-view-album)
    (define-key map (kbd "s")   #'spofy-artist-top-tracks-save)
    (define-key map (kbd "Q")   #'spofy-add-to-queue)
    (define-key map (kbd "SPC") #'spofy-play-pause)
    (define-key map (kbd "g")   #'spofy-artist-top-tracks-refresh)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `spofy-artist-top-tracks-mode'.")

(define-derived-mode spofy-artist-top-tracks-mode tabulated-list-mode
  "spofy Top Tracks"
  "Major mode for viewing an artist's top tracks."
  :group 'spofy
  (setq tabulated-list-padding 2)
  (spofy-ui-set-format
   'artist-top-track
   '((" "        2 nil)
     ("Name"     :flex t)
     ("Album"    :flex t)
     ("Duration"  6 nil :right-align t)))
  (tabulated-list-init-header))

(defun spofy-top-tracks--format-track (track)
  "Format TRACK alist as a tabulated-list entry for top tracks."
  (let* ((uri (alist-get 'uri track))
         (name (or (alist-get 'name track) ""))
         (album (alist-get 'album track))
         (album-name (if album (or (alist-get 'name album) "") ""))
         (duration (alist-get 'duration_ms track))
         (playing (spofy-ui-playing-indicator uri))
         (dur-str (if duration (spofy-ui-format-duration-ms duration) "")))
    (spofy-ui-store-entity uri track)
    (let ((playing-p (not (string-empty-p playing))))
      (list uri
            (vector playing
                    (spofy-ui-truncate name (spofy-ui-col 'artist-top-track 0)
                                       (spofy-ui-playing-face 'spofy-track-name playing-p))
                    (spofy-ui-truncate album-name (spofy-ui-col 'artist-top-track 1)
                                       (spofy-ui-playing-face 'spofy-album-name playing-p))
                    (propertize dur-str 'face
                                (spofy-ui-playing-face 'spofy-muted playing-p)))))))

(defun spofy-top-tracks--render (artist-id artist-name tracks)
  "Render top TRACKS for artist ARTIST-NAME in a buffer.
ARTIST-ID is kept for refresh."
  (let ((buf-name (format "*spofy Top Tracks: %s*" artist-name)))
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (spofy-artist-top-tracks-mode)
      (setq-local spofy-ui--buffer-entity-name artist-name)
      (setq-local spofy-ui--buffer-context
                  `((artist-id . ,artist-id)
                    (artist-name . ,artist-name)))
      (setq-local spofy-ui--entity-type 'track)
      (setq-local spofy-ui--entry-formatter
                  (lambda (entity _idx)
                    (spofy-top-tracks--format-track entity)))
      (spofy-ui-insert-header
       (list (format "Top tracks: %s" artist-name)))
      (setq tabulated-list-entries
            (cl-loop for track across tracks
                     collect (spofy-top-tracks--format-track track)))
      (tabulated-list-print t)
      (goto-char (point-min))
      (switch-to-buffer (current-buffer)))))

(defun spofy-view-artist-top-tracks (artist-id artist-name)
  "Fetch and display top tracks for ARTIST-ID (displayed as ARTIST-NAME).
Uses cached tracks when available."
  (let* ((cache-key (format "spotify:artist:%s:top-tracks" artist-id))
         (cached (spofy-browse--cache-get cache-key)))
    (if cached
        (spofy-top-tracks--render artist-id artist-name cached)
      (spofy-api-get (format "artists/%s/top-tracks" artist-id) nil
                     (lambda (response)
                       (let ((tracks (alist-get 'tracks response)))
                         (spofy-browse--cache-put cache-key tracks)
                         (spofy-top-tracks--render
                          artist-id artist-name tracks)))))))

(defun spofy-artist-top-tracks-play ()
  "Play the track at point."
  (interactive)
  (when-let* ((uri (tabulated-list-get-id)))
    (spofy-play-track uri nil)))

(defun spofy-artist-top-tracks-view-album ()
  "View the album of the track at point."
  (interactive)
  (when-let* ((entity (spofy-ui-entity-at-point))
              (album (alist-get 'album entity))
              (album-id (alist-get 'id album)))
    (spofy-view-album album-id)))

(defun spofy-artist-top-tracks-save ()
  "Save the track at point to the user's library."
  (interactive)
  (when-let* ((uri (tabulated-list-get-id)))
    (spofy-library-save uri)))

(defun spofy-artist-top-tracks-refresh ()
  "Refresh the artist top tracks view, bypassing the cache."
  (interactive)
  (when-let* ((artist-id (alist-get 'artist-id spofy-ui--buffer-context))
              (artist-name (alist-get 'artist-name spofy-ui--buffer-context)))
    (spofy-browse--cache-invalidate
     (format "spotify:artist:%s:top-tracks" artist-id))
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
    (define-key map (kbd "f")   #'spofy-playlist-view-follow)
    (define-key map (kbd "Q")   #'spofy-add-to-queue)
    (define-key map (kbd "SPC") #'spofy-play-pause)
    (define-key map (kbd "g")   #'spofy-playlist-view-refresh)
    (define-key map (kbd "q")   #'quit-window)

    map)
  "Keymap for `spofy-playlist-view-mode'.")

(define-derived-mode spofy-playlist-view-mode tabulated-list-mode
  "spofy Playlist"
  "Major mode for viewing a Spotify playlist's tracks."
  :group 'spofy
  (setq tabulated-list-padding 2)
  (spofy-ui-set-format
   'playlist-track
   '(("#"          4 nil :right-align t)
     ("Name"       :flex t)
     ("Artist(s)"  :flex t)
     ("Album"      :flex t)
     ("Duration"    6 nil :right-align t)))
  (tabulated-list-init-header))

(defun spofy-playlist--format-track-item (item playlist-uri track-number)
  "Format a playlist track ITEM (wrapper with `track' and `added_by').
PLAYLIST-URI is the playlist context URI for playback.
TRACK-NUMBER is the 1-based position in the playlist."
  (let* ((track (alist-get 'track item))
         (uri (if track (alist-get 'uri track) nil)))
    ;; Protect against nil tracks (local files, etc.)
    (when (and track uri)
      (let* ((name (or (alist-get 'name track) ""))
             (artists (alist-get 'artists track))
             (album (alist-get 'album track))
             (album-name (if album (or (alist-get 'name album) "") ""))
             (duration (alist-get 'duration_ms track))
             (playing (spofy-ui-playing-indicator uri))
             (num-str (number-to-string track-number))
             (artists-str (if artists (spofy-ui-format-artists artists) ""))
             (dur-str (if duration (spofy-ui-format-duration-ms duration) "")))
        ;; Store the full wrapper so we have added_by and track data.
        (spofy-ui-store-entity uri item)
        ;; Store playlist-uri for playback context.
        (ignore playlist-uri)
        (let ((playing-p (not (string-empty-p playing))))
          (list uri
                (vector (if playing-p
                            (propertize num-str 'face 'spofy-playing)
                          num-str)
                        (spofy-ui-truncate name (spofy-ui-col 'playlist-track 0)
                                           (spofy-ui-playing-face 'spofy-track-name playing-p))
                        (spofy-ui-truncate artists-str (spofy-ui-col 'playlist-track 1)
                                           (spofy-ui-playing-face 'spofy-artist-name playing-p))
                        (spofy-ui-truncate album-name (spofy-ui-col 'playlist-track 2)
                                           (spofy-ui-playing-face 'spofy-album-name playing-p))
                        (propertize dur-str 'face
                                    (spofy-ui-playing-face 'spofy-muted playing-p)))))))))

(defun spofy-playlist--render (playlist)
  "Render PLAYLIST data into a buffer.
Uses cached track items when available; otherwise fetches all
pages asynchronously before rendering."
  (let* ((name (or (alist-get 'name playlist) "Unknown Playlist"))
         (owner-obj (alist-get 'owner playlist))
         (owner (if owner-obj
                    (or (alist-get 'display_name owner-obj)
                        (alist-get 'id owner-obj))
                  "Unknown"))
         (description (or (alist-get 'description playlist) ""))
         (tracks-obj (alist-get 'tracks playlist))
         (total (alist-get 'total tracks-obj))
         (first-items (append (alist-get 'items tracks-obj) nil))
         (next-url (let ((n (alist-get 'next tracks-obj)))
                     (and (stringp n) n)))
         (uri (alist-get 'uri playlist))
         (playlist-id (alist-get 'id playlist))
         (buf-name (format "*spofy Playlist: %s*" name))
         (cached (spofy-browse--cache-get uri)))
    (cond
     (cached
      (spofy-playlist--render-tracks
       cached buf-name playlist-id uri name owner description
       (length cached)))
     (next-url
      (spofy-browse--fetch-all-pages
       first-items next-url
       (lambda (all-items)
         (spofy-browse--cache-put uri all-items)
         (spofy-playlist--render-tracks
          all-items buf-name playlist-id uri name owner description total))))
     (t
      (spofy-browse--cache-put uri first-items)
      (spofy-playlist--render-tracks
       first-items buf-name playlist-id uri name owner description total)))))

(defun spofy-playlist--render-tracks
    (items buf-name playlist-id uri name owner description total)
  "Render all playlist track ITEMS into BUF-NAME.
PLAYLIST-ID, URI, NAME, OWNER, DESCRIPTION, and TOTAL describe
the playlist."
  (with-current-buffer (get-buffer-create buf-name)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (spofy-playlist-view-mode)
    (setq-local spofy-ui--buffer-entity-name name)
    (setq-local spofy-ui--buffer-context
                `((playlist-id . ,playlist-id)
                  (playlist-uri . ,uri)))
    (setq-local spofy-ui--entity-type 'track)
    (setq-local spofy-ui--entry-formatter
                (let ((playlist-uri uri))
                  (lambda (entity idx)
                    (spofy-playlist--format-track-item
                     entity playlist-uri (1+ idx)))))
    (spofy-ui-insert-header
     (list (format "Playlist: %s" name)
           (format "Owner: %s" owner)
           (format "Description: %s" description)
           (format "Tracks: %s" (or total "?"))))
    (setq tabulated-list-entries
          (cl-loop for item in items
                   for idx from 1
                   for entry = (spofy-playlist--format-track-item item uri idx)
                   when entry collect entry))
    (tabulated-list-print t)
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))))

;;;###autoload
(defun spofy-view-playlist (playlist-id)
  "View the Spotify playlist identified by PLAYLIST-ID.
Fetches playlist data from the API and displays it in a tabulated-list buffer."
  (interactive
   (list (read-string "Spotify playlist ID: ")))
  (spofy-api-get (format "playlists/%s" playlist-id) (spofy-api-with-market nil)
                 #'spofy-playlist--render))

(defun spofy-playlist-view-play-track ()
  "Play the track at point in the playlist context."
  (interactive)
  (when-let* ((uri (tabulated-list-get-id))
              (playlist-uri (alist-get 'playlist-uri spofy-ui--buffer-context)))
    (spofy-play-track uri playlist-uri (spofy-ui-row-position))))

(defun spofy-playlist-view-album ()
  "View the album of the track at point."
  (interactive)
  (when-let* ((entity (spofy-ui-entity-at-point))
              (track (or (alist-get 'track entity) entity))
              (album (alist-get 'album track))
              (album-id (alist-get 'id album)))
    (spofy-view-album album-id)))

(defun spofy-playlist-view-artist ()
  "View the artist of the track at point."
  (interactive)
  (when-let* ((entity (spofy-ui-entity-at-point))
              (track (or (alist-get 'track entity) entity))
              (artists (alist-get 'artists track))
              (notempty (> (length artists) 0))
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
  (when-let* ((uri (tabulated-list-get-id)))
    (spofy-library-save uri)))

(defun spofy-playlist-view-follow ()
  "Follow the current playlist."
  (interactive)
  (when-let* ((playlist-id (alist-get 'playlist-id spofy-ui--buffer-context)))
    (spofy-follow-playlist playlist-id)))

(defun spofy-playlist-view-refresh ()
  "Refresh the playlist view, bypassing the track cache."
  (interactive)
  (when-let* ((uri (alist-get 'playlist-uri spofy-ui--buffer-context)))
    (spofy-browse--cache-invalidate uri))
  (when-let* ((playlist-id (alist-get 'playlist-id spofy-ui--buffer-context)))
    (spofy-view-playlist playlist-id)))

(provide 'spofy-browse)
;;; spofy-browse.el ends here
