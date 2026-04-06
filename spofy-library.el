;;; spofy-library.el --- Library management for Spofy  -*- lexical-binding: t; -*-

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

;; User library management for the Spofy Spotify client.  Provides
;; commands to list, save, and unsave tracks and albums in the user's
;; Spotify library, displayed in `tabulated-list-mode' buffers with
;; pagination support.

;;; Code:

(require 'spofy-api)
(require 'spofy-ui)
(require 'cl-lib)

;; Functions from other Spofy modules that may not be loaded yet.
(declare-function spofy-play-track "spofy-player" (track-uri &optional context-uri))
(declare-function spofy-play-context "spofy-player" (context-uri))
(declare-function spofy-view-album "spofy-browse" (album-id))
(declare-function spofy-view-artist "spofy-browse" (artist-id))
(declare-function spofy-playlist-add-track "spofy-playlist" (track-uri))

;;;; Helpers

(defun spofy-library--extract-type (uri)
  "Extract the entity type from URI.
For example, \"spotify:track:1234\" returns \"track\"."
  (if (string-match "spotify:\\([^:]+\\):" uri)
      (match-string 1 uri)
    nil))

(defun spofy-library--normalize-type (type)
  "Normalize TYPE to a singular Spotify library entity type."
  (pcase (if (symbolp type) (symbol-name type) type)
    ((or "track" "tracks") "track")
    ((or "album" "albums") "album")
    (_ nil)))

(defun spofy-library--resolve-entity (uri-or-type &optional id)
  "Resolve URI-OR-TYPE and optional ID to a normalized (TYPE . ID) pair."
  (if id
      (cons (spofy-library--normalize-type uri-or-type) id)
    (cons (spofy-library--normalize-type (spofy-library--extract-type uri-or-type))
          (spofy-ui-extract-id uri-or-type))))

(defun spofy-library--entity-type-at-point ()
  "Return the entity type at point based on buffer context.
Returns \"track\" or \"album\"."
  (symbol-name (or spofy-ui--entity-type 'track)))

;;; ========================================================================
;;;; Saved tracks
;;; ========================================================================

(defvar spofy-library-tracks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'spofy-library-tracks-play)
    (define-key map (kbd "a")   #'spofy-library-tracks-view-album)
    (define-key map (kbd "A")   #'spofy-library-tracks-view-artist)
    (define-key map (kbd "S")   #'spofy-library-tracks-unsave)
    (define-key map (kbd "p")   #'spofy-library-tracks-add-to-playlist)
    (define-key map (kbd "SPC") #'spofy-play-pause)
    (define-key map (kbd "g")   #'spofy-library-tracks-refresh)

    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `spofy-library-tracks-mode'.")

(define-derived-mode spofy-library-tracks-mode tabulated-list-mode
  "Spofy Library Tracks"
  "Major mode for viewing saved tracks in the user's Spotify library."
  :group 'spofy
  (setq tabulated-list-padding 2)
  (spofy-ui-set-format
   'library-track
   '((" "         2 nil)
     ("Name"      :flex t)
     ("Artist(s)" :flex t)
     ("Album"     :flex t)
     ("Duration"  6 nil :right-align t)))
  (setq-local spofy-ui--entity-type 'track)
  (tabulated-list-init-header))

(defun spofy-library--format-saved-track (item)
  "Format a saved-track ITEM as a `tabulated-list-entries' entry.
ITEM is the wrapper alist from /me/tracks which contains a `track' key."
  (let* ((track (alist-get 'track item))
         (uri (alist-get 'uri track))
         (name (or (alist-get 'name track) ""))
         (artists (or (alist-get 'artists track) []))
         (album (alist-get 'album track))
         (album-name (if album (or (alist-get 'name album) "") ""))
         (duration-ms (or (alist-get 'duration_ms track) 0))
         (artist-str (spofy-ui-format-artists artists))
         (duration-str (spofy-ui-format-duration-ms duration-ms))
         (playing (spofy-ui-playing-indicator uri)))
    (spofy-ui-store-entity uri track)
    (let ((playing-p (not (string-empty-p playing))))
      (list uri
            (vector playing
                    (spofy-ui-truncate name (spofy-ui-col 'library-track 0)
                                       (spofy-ui-playing-face 'spofy-track-name playing-p))
                    (spofy-ui-truncate artist-str (spofy-ui-col 'library-track 1)
                                       (spofy-ui-playing-face 'spofy-artist-name playing-p))
                    (spofy-ui-truncate album-name (spofy-ui-col 'library-track 2)
                                       (spofy-ui-playing-face 'spofy-album-name playing-p))
                    (propertize duration-str 'face
                                (spofy-ui-playing-face 'spofy-muted playing-p)))))))

(defun spofy-library--reformat-saved-track (entity _idx)
  "Re-format ENTITY as a saved-track entry for highlight refresh.
ENTITY is the inner track alist stored by `spofy-ui-store-entity'.
_IDX is the entry position (unused)."
  (spofy-library--format-saved-track `((track . ,entity))))

(defun spofy-library--render-tracks (response)
  "Render saved tracks RESPONSE into the *Spofy Saved Tracks* buffer."
  (let* ((paged (spofy-api--extract-paged-results response))
         (items (alist-get 'items paged))
         (next-url (alist-get 'next paged))
         (buf-name "*Spofy Saved Tracks*"))
    (spofy-ui-render-list
     buf-name #'spofy-library-tracks-mode
     (lambda ()
       (cl-loop for item across items
                collect (spofy-library--format-saved-track item)))
     next-url
     (lambda (response)
       (let ((items (alist-get 'items response))
             (next-url (alist-get 'next response)))
         (cons (cl-loop for item across items
                        collect (spofy-library--format-saved-track item))
               next-url))))
    (with-current-buffer buf-name
      (setq-local spofy-ui--entry-formatter
                  #'spofy-library--reformat-saved-track))))

;;;###autoload
(defun spofy-library-browse-tracks ()
  "List the user's saved tracks from Spotify.
Uses the library cache when available; otherwise fetches from
the /me/tracks endpoint with pagination."
  (interactive)
  (if-let* ((items (spofy-library-cache-get 'track)))
      (spofy-library--render-tracks-from-cache items)
    (spofy-api-get "me/tracks" '(("limit" . "50"))
                   #'spofy-library--render-tracks)))

(defun spofy-library--render-tracks-from-cache (items)
  "Render cached saved track ITEMS into the *Spofy Saved Tracks* buffer."
  (spofy-ui-render-list
   "*Spofy Saved Tracks*" #'spofy-library-tracks-mode
   (lambda ()
     (cl-loop for item in items
              collect (spofy-library--format-saved-track item)))
   nil)
  (with-current-buffer "*Spofy Saved Tracks*"
    (setq-local spofy-ui--entry-formatter
                #'spofy-library--reformat-saved-track)))

(defun spofy-library-tracks-play ()
  "Play the track at point."
  (interactive)
  (when-let* ((uri (tabulated-list-get-id)))
    (spofy-play-track uri)))

(defun spofy-library-tracks-view-album ()
  "View the album for the track at point."
  (interactive)
  (when-let* ((entity (spofy-ui-entity-at-point))
              (album (alist-get 'album entity))
              (album-id (alist-get 'id album)))
    (spofy-view-album album-id)))

(defun spofy-library-tracks-view-artist ()
  "View the artist for the track at point."
  (interactive)
  (when-let* ((entity (spofy-ui-entity-at-point))
              (artists (alist-get 'artists entity))
              ((_notempty (> (length artists) 0)))
              (artist (aref artists 0))
              (artist-id (alist-get 'id artist)))
    (spofy-view-artist artist-id)))

(defun spofy-library-tracks-unsave ()
  "Remove the track at point from the user's library."
  (interactive)
  (when-let* ((uri (tabulated-list-get-id)))
    (spofy-library-unsave uri)))

(defun spofy-library-tracks-add-to-playlist ()
  "Add the track at point to a playlist."
  (interactive)
  (when-let* ((uri (tabulated-list-get-id)))
    (spofy-playlist-add-track uri)))

(defun spofy-library-tracks-refresh ()
  "Refresh the saved tracks list."
  (interactive)
  (spofy-library-browse-tracks))

;;; ========================================================================
;;;; Saved albums
;;; ========================================================================

(defvar spofy-library-albums-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'spofy-library-albums-view)
    (define-key map (kbd "p")   #'spofy-library-albums-play)
    (define-key map (kbd "A")   #'spofy-library-albums-view-artist)
    (define-key map (kbd "S")   #'spofy-library-albums-unsave)
    (define-key map (kbd "SPC") #'spofy-play-pause)
    (define-key map (kbd "g")   #'spofy-library-albums-refresh)

    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `spofy-library-albums-mode'.")

(define-derived-mode spofy-library-albums-mode tabulated-list-mode
  "Spofy Library Albums"
  "Major mode for viewing saved albums in the user's Spotify library."
  :group 'spofy
  (setq tabulated-list-padding 2)
  (spofy-ui-set-format
   'library-album
   '(("Name"      :flex t)
     ("Artist(s)" :flex t)
     ("Year"      6 t)
     ("Tracks"    6 nil :right-align t)))
  (setq-local spofy-ui--entity-type 'album)
  (tabulated-list-init-header))

(defun spofy-library--format-saved-album (item)
  "Format a saved-album ITEM as a `tabulated-list-entries' entry.
ITEM is the wrapper alist from /me/albums which contains an `album' key."
  (let* ((album (alist-get 'album item))
         (uri (alist-get 'uri album))
         (name (or (alist-get 'name album) ""))
         (artists (or (alist-get 'artists album) []))
         (artist-str (spofy-ui-format-artists artists))
         (year (spofy-ui-album-year album))
         (total-tracks (or (alist-get 'total_tracks album) 0)))
    (spofy-ui-store-entity uri album)
    (list uri
          (vector (spofy-ui-truncate name (spofy-ui-col 'library-album 0) 'spofy-album-name)
                  (spofy-ui-truncate artist-str (spofy-ui-col 'library-album 1) 'spofy-artist-name)
                  (propertize year 'face 'spofy-muted)
                  (propertize (number-to-string total-tracks)
                              'face 'spofy-muted)))))

(defun spofy-library--render-albums (response)
  "Render saved albums RESPONSE into the *Spofy Saved Albums* buffer."
  (let* ((paged (spofy-api--extract-paged-results response))
         (items (alist-get 'items paged))
         (next-url (alist-get 'next paged))
         (buf-name "*Spofy Saved Albums*"))
    (spofy-ui-render-list
     buf-name #'spofy-library-albums-mode
     (lambda ()
       (cl-loop for item across items
                collect (spofy-library--format-saved-album item)))
     next-url
     (lambda (response)
       (let ((items (alist-get 'items response))
             (next-url (alist-get 'next response)))
         (cons (cl-loop for item across items
                        collect (spofy-library--format-saved-album item))
               next-url))))))

;;;###autoload
(defun spofy-library-browse-albums ()
  "List the user's saved albums from Spotify.
Uses the library cache when available; otherwise fetches from
the /me/albums endpoint with pagination."
  (interactive)
  (if-let* ((items (spofy-library-cache-get 'album)))
      (spofy-library--render-albums-from-cache items)
    (spofy-api-get "me/albums" '(("limit" . "50"))
                   #'spofy-library--render-albums)))

(defun spofy-library--render-albums-from-cache (items)
  "Render cached saved album ITEMS into the *Spofy Saved Albums* buffer."
  (spofy-ui-render-list
   "*Spofy Saved Albums*" #'spofy-library-albums-mode
   (lambda ()
     (cl-loop for item in items
              collect (spofy-library--format-saved-album item)))
   nil))

(defun spofy-library-albums-view ()
  "View the album at point."
  (interactive)
  (when-let* ((uri (tabulated-list-get-id))
              (album-id (spofy-ui-extract-id uri)))
    (spofy-view-album album-id)))

(defun spofy-library-albums-play ()
  "Play the album at point."
  (interactive)
  (when-let* ((uri (tabulated-list-get-id)))
    (spofy-play-context uri)))

(defun spofy-library-albums-view-artist ()
  "View the artist for the album at point."
  (interactive)
  (when-let* ((entity (spofy-ui-entity-at-point))
              (artists (alist-get 'artists entity))
              ((_notempty (> (length artists) 0)))
              (artist (aref artists 0))
              (artist-id (alist-get 'id artist)))
    (spofy-view-artist artist-id)))

(defun spofy-library-albums-unsave ()
  "Remove the album at point from the user's library."
  (interactive)
  (when-let* ((uri (tabulated-list-get-id)))
    (spofy-library-unsave uri)))

(defun spofy-library-albums-refresh ()
  "Refresh the saved albums list."
  (interactive)
  (spofy-library-browse-albums))

;;; ========================================================================
;;;; Recently played
;;; ========================================================================

(defvar spofy-recently-played-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'spofy-recently-played-play)
    (define-key map (kbd "a")   #'spofy-recently-played-view-album)
    (define-key map (kbd "A")   #'spofy-recently-played-view-artist)
    (define-key map (kbd "SPC") #'spofy-play-pause)
    (define-key map (kbd "g")   #'spofy-recently-played-refresh)

    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `spofy-recently-played-mode'.")

(define-derived-mode spofy-recently-played-mode tabulated-list-mode
  "Spofy Recently Played"
  "Major mode for viewing recently played tracks."
  :group 'spofy
  (setq tabulated-list-padding 2)
  (spofy-ui-set-format
   'library-track
   '((" "         2 nil)
     ("Name"      :flex t)
     ("Artist(s)" :flex t)
     ("Album"     :flex t)
     ("Duration"  6 nil :right-align t)))
  (setq-local spofy-ui--entity-type 'track)
  (tabulated-list-init-header))

(defun spofy-recently-played--render (response)
  "Render recently played RESPONSE into the buffer."
  (let* ((items (alist-get 'items response))
         (next-url (alist-get 'next response))
         (buf-name "*Spofy Recently Played*"))
    (spofy-ui-render-list
     buf-name #'spofy-recently-played-mode
     (lambda ()
       (cl-loop for item across items
                collect (spofy-library--format-saved-track item)))
     next-url
     (lambda (response)
       (let ((items (alist-get 'items response))
             (next-url (alist-get 'next response)))
         (cons (cl-loop for item across items
                        collect (spofy-library--format-saved-track item))
               next-url))))
    (with-current-buffer buf-name
      (setq-local spofy-ui--entry-formatter
                  #'spofy-library--reformat-saved-track))))

;;;###autoload
(defun spofy-list-recently-played ()
  "List recently played tracks from Spotify."
  (interactive)
  (spofy-api-get "me/player/recently-played" '(("limit" . "50"))
                 #'spofy-recently-played--render))

(defun spofy-recently-played-play ()
  "Play the track at point."
  (interactive)
  (when-let* ((uri (tabulated-list-get-id)))
    (spofy-play-track uri)))

(defun spofy-recently-played-view-album ()
  "View the album for the track at point."
  (interactive)
  (when-let* ((entity (spofy-ui-entity-at-point))
              (album (alist-get 'album entity))
              (album-id (alist-get 'id album)))
    (spofy-view-album album-id)))

(defun spofy-recently-played-view-artist ()
  "View the artist for the track at point."
  (interactive)
  (when-let* ((entity (spofy-ui-entity-at-point))
              (artists (alist-get 'artists entity))
              ((_notempty (> (length artists) 0)))
              (artist (aref artists 0))
              (artist-id (alist-get 'id artist)))
    (spofy-view-artist artist-id)))

(defun spofy-recently-played-refresh ()
  "Refresh the recently played list."
  (interactive)
  (spofy-list-recently-played))

;;; ========================================================================
;;;; Library cache
;;; ========================================================================

(defvar spofy-library--cache nil
  "Alist mapping entity type symbols to cached library items.
Each entry is (TYPE . ITEMS) where ITEMS is a list of API alists.
Populated by `spofy-library--fetch-all-async', invalidated by
save/unsave.")

(defvar spofy-library--fetching nil
  "List of TYPE symbols currently being fetched asynchronously.")

(defun spofy-library--fetch-all-async (endpoint type &optional callback)
  "Fetch all items from ENDPOINT asynchronously, paginating until exhausted.
TYPE is a symbol (track, album, playlist) used as cache key.
CALLBACK, if non-nil, is called with the complete item list when done.
Does nothing if TYPE is already cached or being fetched."
  (when (and (not (alist-get type spofy-library--cache))
             (not (memq type spofy-library--fetching)))
    (push type spofy-library--fetching)
    (message "Spofy: fetching %ss..." type)
    (let ((all-items nil))
      (cl-labels
          ((fetch-page (url)
             (spofy-api--request
              "GET" url nil nil
              (lambda (response)
                (let* ((items (alist-get 'items response))
                       (next (let ((n (alist-get 'next response)))
                               (and (stringp n) n))))
                  (setq all-items (append all-items (append items nil)))
                  (if next
                      (fetch-page next)
                    (setq spofy-library--fetching
                          (delq type spofy-library--fetching))
                    (setf (alist-get type spofy-library--cache) all-items)
                    (message "Spofy: fetched %d %ss" (length all-items) type)
                    (when callback
                      (funcall callback all-items))))))))
        (fetch-page (spofy-api--build-url endpoint '(("limit" . "50"))))))))

(defun spofy-library-cache-ready-p (type)
  "Return non-nil if the cache for TYPE is populated."
  (and (alist-get type spofy-library--cache) t))

(defun spofy-library-cache-get (type)
  "Return cached items for TYPE, or nil if not yet cached."
  (alist-get type spofy-library--cache))

(defun spofy-library-invalidate-cache (&optional type)
  "Invalidate the library cache for TYPE, or all types if nil."
  (if type
      (setf (alist-get type spofy-library--cache nil 'remove) nil)
    (setq spofy-library--cache nil)))

(defun spofy-library-warm-cache ()
  "Start fetching all library data in the background.
Call this early (e.g., when spofy initializes) so that library
search commands have data ready when the user invokes them."
  (spofy-library--fetch-all-async "me/tracks" 'track)
  (spofy-library--fetch-all-async "me/albums" 'album)
  (spofy-library--fetch-all-async "me/playlists" 'playlist))

;;; ========================================================================
;;;; Save / unsave commands
;;; ========================================================================

;;;###autoload
(defun spofy-library-save (uri-or-type &optional id)
  "Save a Spotify entity to the user's library.
URI-OR-TYPE may be a Spotify URI like \"spotify:track:ID\", or a
type string/symbol plus ID supplied separately."
  (interactive
   (list (or (tabulated-list-get-id)
             (read-string "Spotify URI: "))))
  (pcase-let* ((`(,type . ,entity-id)
                 (spofy-library--resolve-entity uri-or-type id))
         (endpoint (pcase type
                     ("track" "me/tracks")
                     ("album" "me/albums")
                     (_ nil))))
    (if (not endpoint)
        (message "Spofy: cannot save entity of type \"%s\"." (or type "unknown"))
      (spofy-api-put endpoint `((ids . [,entity-id]))
                     (lambda (_response)
                       (spofy-library-invalidate-cache (intern type))
                       (message "Spofy: saved %s to library." type))))))

;;;###autoload
(defun spofy-library-unsave (uri-or-type &optional id)
  "Remove a Spotify entity from the user's library.
URI-OR-TYPE may be a Spotify URI like \"spotify:track:ID\", or a
type string/symbol plus ID supplied separately."
  (interactive
   (list (or (tabulated-list-get-id)
             (read-string "Spotify URI: "))))
  (pcase-let* ((`(,type . ,entity-id)
                 (spofy-library--resolve-entity uri-or-type id))
         (endpoint (pcase type
                     ("track" "me/tracks")
                     ("album" "me/albums")
                     (_ nil))))
    (if (not endpoint)
        (message "Spofy: cannot unsave entity of type \"%s\"." (or type "unknown"))
      (spofy-api-delete endpoint `((ids . [,entity-id]))
                        (lambda (_response)
                          (spofy-library-invalidate-cache (intern type))
                          (message "Spofy: removed %s from library." type))))))

;;; ========================================================================
;;;; Top tracks
;;; ========================================================================

(defvar spofy-top-tracks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'spofy-top-tracks-play)
    (define-key map (kbd "a")   #'spofy-top-tracks-view-album)
    (define-key map (kbd "A")   #'spofy-top-tracks-view-artist)
    (define-key map (kbd "SPC") #'spofy-play-pause)
    (define-key map (kbd "g")   #'spofy-top-tracks-refresh)

    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `spofy-top-tracks-mode'.")

(define-derived-mode spofy-top-tracks-mode tabulated-list-mode
  "Spofy Top Tracks"
  "Major mode for viewing top tracks."
  :group 'spofy
  (setq tabulated-list-padding 2)
  (spofy-ui-set-format
   'library-track
   '((" "         2 nil)
     ("Name"      :flex t)
     ("Artist(s)" :flex t)
     ("Album"     :flex t)
     ("Duration"  6 nil :right-align t)))
  (setq-local spofy-ui--entity-type 'track)
  (tabulated-list-init-header))

(defvar-local spofy-top-tracks--time-range "medium_term"
  "The time range used for the current top-tracks buffer.")

(defun spofy-top-tracks--format (item)
  "Format a top-track ITEM as a `tabulated-list-entries' entry."
  (let* ((uri (alist-get 'uri item))
         (name (or (alist-get 'name item) ""))
         (artists (or (alist-get 'artists item) []))
         (album (alist-get 'album item))
         (album-name (if album (or (alist-get 'name album) "") ""))
         (duration-ms (or (alist-get 'duration_ms item) 0))
         (artist-str (spofy-ui-format-artists artists))
         (duration-str (spofy-ui-format-duration-ms duration-ms))
         (playing (spofy-ui-playing-indicator uri)))
    (spofy-ui-store-entity uri item)
    (let ((playing-p (not (string-empty-p playing))))
      (list uri
            (vector playing
                    (spofy-ui-truncate name (spofy-ui-col 'library-track 0)
                                       (spofy-ui-playing-face 'spofy-track-name playing-p))
                    (spofy-ui-truncate artist-str (spofy-ui-col 'library-track 1)
                                       (spofy-ui-playing-face 'spofy-artist-name playing-p))
                    (spofy-ui-truncate album-name (spofy-ui-col 'library-track 2)
                                       (spofy-ui-playing-face 'spofy-album-name playing-p))
                    (propertize duration-str 'face
                                (spofy-ui-playing-face 'spofy-muted playing-p)))))))

(defun spofy-top-tracks--render (response time-range)
  "Render top tracks RESPONSE into a buffer.
TIME-RANGE is the time range string used for the query."
  (let* ((items (alist-get 'items response))
         (next-url (alist-get 'next response))
         (label (pcase time-range
                  ("short_term" "4 weeks")
                  ("medium_term" "6 months")
                  ("long_term" "1 year")))
         (buf-name (format "*Spofy Top Tracks (%s)*" label)))
    (spofy-ui-render-list
     buf-name #'spofy-top-tracks-mode
     (lambda ()
       (cl-loop for item across items
                collect (spofy-top-tracks--format item)))
     next-url
     (lambda (response)
       (let ((items (alist-get 'items response))
             (next-url (alist-get 'next response)))
         (cons (cl-loop for item across items
                        collect (spofy-top-tracks--format item))
               next-url))))
    (with-current-buffer (get-buffer buf-name)
      (setq-local spofy-top-tracks--time-range time-range)
      (setq-local spofy-ui--entry-formatter
                  (lambda (entity _idx)
                    (spofy-top-tracks--format entity))))))

;;;###autoload
(defun spofy-list-top-tracks (&optional time-range)
  "List top tracks from Spotify.
TIME-RANGE is \"short_term\", \"medium_term\", or \"long_term\".
Defaults to \"medium_term\"."
  (interactive)
  (let ((tr (or time-range "medium_term")))
    (spofy-api-get "me/top/tracks"
                   `(("limit" . "50") ("time_range" . ,tr))
                   (lambda (response)
                     (spofy-top-tracks--render response tr)))))

(defun spofy-top-tracks-play ()
  "Play the track at point."
  (interactive)
  (when-let* ((uri (tabulated-list-get-id)))
    (spofy-play-track uri)))

(defun spofy-top-tracks-view-album ()
  "View the album for the track at point."
  (interactive)
  (when-let* ((entity (spofy-ui-entity-at-point))
              (album (alist-get 'album entity))
              (album-id (alist-get 'id album)))
    (spofy-view-album album-id)))

(defun spofy-top-tracks-view-artist ()
  "View the artist for the track at point."
  (interactive)
  (when-let* ((entity (spofy-ui-entity-at-point))
              (artists (alist-get 'artists entity))
              ((_notempty (> (length artists) 0)))
              (artist (aref artists 0))
              (artist-id (alist-get 'id artist)))
    (spofy-view-artist artist-id)))

(defun spofy-top-tracks-refresh ()
  "Refresh the top tracks list."
  (interactive)
  (spofy-list-top-tracks spofy-top-tracks--time-range))

;;; ========================================================================
;;;; Top artists
;;; ========================================================================

(defvar spofy-top-artists-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'spofy-top-artists-view)
    (define-key map (kbd "SPC") #'spofy-play-pause)
    (define-key map (kbd "g")   #'spofy-top-artists-refresh)

    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `spofy-top-artists-mode'.")

(define-derived-mode spofy-top-artists-mode tabulated-list-mode
  "Spofy Top Artists"
  "Major mode for viewing top artists."
  :group 'spofy
  (setq tabulated-list-padding 2)
  (spofy-ui-set-format
   'search-artist
   '(("Name"   :flex t)
     ("Genres" :flex t)))
  (setq-local spofy-ui--entity-type 'artist)
  (tabulated-list-init-header))

(defvar-local spofy-top-artists--time-range "medium_term"
  "The time range used for the current top-artists buffer.")

(defun spofy-top-artists--format (item)
  "Format a top-artist ITEM as a `tabulated-list-entries' entry."
  (let* ((id (alist-get 'id item))
         (uri (or (alist-get 'uri item) (concat "spotify:artist:" id)))
         (name (or (alist-get 'name item) ""))
         (genres (alist-get 'genres item))
         (genre-str (if (and genres (> (length genres) 0))
                        (mapconcat #'identity
                                   (seq-take (append genres nil) 3)
                                   ", ")
                      "")))
    (spofy-ui-store-entity uri item)
    (list uri
          (vector (spofy-ui-truncate name (spofy-ui-col 'search-artist 0) 'spofy-track-name)
                  (spofy-ui-truncate genre-str (spofy-ui-col 'search-artist 1) 'spofy-muted)))))

(defun spofy-top-artists--render (response time-range)
  "Render top artists RESPONSE into a buffer.
TIME-RANGE is the time range string used for the query."
  (let* ((items (alist-get 'items response))
         (next-url (alist-get 'next response))
         (label (pcase time-range
                  ("short_term" "4 weeks")
                  ("medium_term" "6 months")
                  ("long_term" "1 year")))
         (buf-name (format "*Spofy Top Artists (%s)*" label)))
    (spofy-ui-render-list
     buf-name #'spofy-top-artists-mode
     (lambda ()
       (cl-loop for item across items
                collect (spofy-top-artists--format item)))
     next-url
     (lambda (response)
       (let ((items (alist-get 'items response))
             (next-url (alist-get 'next response)))
         (cons (cl-loop for item across items
                        collect (spofy-top-artists--format item))
               next-url))))
    (with-current-buffer (get-buffer buf-name)
      (setq-local spofy-top-artists--time-range time-range))))

;;;###autoload
(defun spofy-list-top-artists (&optional time-range)
  "List top artists from Spotify.
TIME-RANGE is \"short_term\", \"medium_term\", or \"long_term\".
Defaults to \"medium_term\"."
  (interactive)
  (let ((tr (or time-range "medium_term")))
    (spofy-api-get "me/top/artists"
                   `(("limit" . "50") ("time_range" . ,tr))
                   (lambda (response)
                     (spofy-top-artists--render response tr)))))

(defun spofy-top-artists-view ()
  "View the artist at point."
  (interactive)
  (when-let* ((uri (tabulated-list-get-id))
              (id (spofy-ui-extract-id uri)))
    (spofy-view-artist id)))

(defun spofy-top-artists-refresh ()
  "Refresh the top artists list."
  (interactive)
  (spofy-list-top-artists spofy-top-artists--time-range))

;;; ========================================================================
;;;; New releases
;;; ========================================================================

(defvar spofy-new-releases-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'spofy-new-releases-view)
    (define-key map (kbd "A")   #'spofy-new-releases-view-artist)
    (define-key map (kbd "SPC") #'spofy-play-pause)
    (define-key map (kbd "g")   #'spofy-new-releases-refresh)

    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `spofy-new-releases-mode'.")

(define-derived-mode spofy-new-releases-mode tabulated-list-mode
  "Spofy New Releases"
  "Major mode for viewing new album releases."
  :group 'spofy
  (setq tabulated-list-padding 2)
  (spofy-ui-set-format
   'library-album
   '(("Album"     :flex t)
     ("Artist(s)" :flex t)
     ("Year"      6 t)
     ("Tracks"    6 nil :right-align t)))
  (setq-local spofy-ui--entity-type 'album)
  (tabulated-list-init-header))

(defun spofy-new-releases--format (item)
  "Format a new-release album ITEM as a `tabulated-list-entries' entry."
  (let* ((uri (alist-get 'uri item))
         (name (or (alist-get 'name item) ""))
         (artists (or (alist-get 'artists item) []))
         (artist-str (spofy-ui-format-artists artists))
         (year (spofy-ui-album-year item))
         (total-tracks (or (alist-get 'total_tracks item) 0)))
    (spofy-ui-store-entity uri item)
    (list uri
          (vector (spofy-ui-truncate name (spofy-ui-col 'library-album 0) 'spofy-album-name)
                  (spofy-ui-truncate artist-str (spofy-ui-col 'library-album 1) 'spofy-artist-name)
                  (propertize year 'face 'spofy-muted)
                  (propertize (number-to-string total-tracks) 'face 'spofy-muted)))))

(defun spofy-new-releases--render (response)
  "Render new releases RESPONSE into the buffer."
  (let* ((albums (alist-get 'albums response))
         (items (and albums (alist-get 'items albums)))
         (next-url (and albums (alist-get 'next albums)))
         (buf-name "*Spofy New Releases*"))
    (spofy-ui-render-list
     buf-name #'spofy-new-releases-mode
     (lambda ()
       (cl-loop for item across items
                collect (spofy-new-releases--format item)))
     next-url
     (lambda (response)
       (let* ((albums (alist-get 'albums response))
              (items (and albums (alist-get 'items albums)))
              (next-url (and albums (alist-get 'next albums))))
         (cons (cl-loop for item across items
                        collect (spofy-new-releases--format item))
               next-url))))))

;;;###autoload
(defun spofy-list-new-releases ()
  "List new album releases from Spotify."
  (interactive)
  (spofy-api-get "browse/new-releases" '(("limit" . "50"))
                 #'spofy-new-releases--render))

(defun spofy-new-releases-view ()
  "View the album at point."
  (interactive)
  (when-let* ((uri (tabulated-list-get-id))
              (album-id (spofy-ui-extract-id uri)))
    (spofy-view-album album-id)))

(defun spofy-new-releases-view-artist ()
  "View the artist for the album at point."
  (interactive)
  (when-let* ((entity (spofy-ui-entity-at-point))
              (artists (alist-get 'artists entity))
              ((_notempty (> (length artists) 0)))
              (artist (aref artists 0))
              (artist-id (alist-get 'id artist)))
    (spofy-view-artist artist-id)))

(defun spofy-new-releases-refresh ()
  "Refresh the new releases list."
  (interactive)
  (spofy-list-new-releases))

(provide 'spofy-library)
;;; spofy-library.el ends here
