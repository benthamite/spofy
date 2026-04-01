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
(declare-function spofy-player-current-track-id "spofy-player" ())
(declare-function spofy-play-track "spofy-player" (track-uri &optional context-uri))
(declare-function spofy-play-context "spofy-player" (context-uri))
(declare-function spofy-view-album "spofy-browse" (album-id))
(declare-function spofy-view-artist "spofy-browse" (artist-id))
(declare-function spofy-playlist-add-track "spofy-playlist" (track-uri))

;;;; Helpers

(defun spofy-library--extract-id (uri)
  "Extract the Spotify ID from URI.
For example, \"spotify:track:1234\" returns \"1234\"."
  (if (string-match "spotify:[^:]+:\\(.+\\)" uri)
      (match-string 1 uri)
    uri))

(defun spofy-library--extract-type (uri)
  "Extract the entity type from URI.
For example, \"spotify:track:1234\" returns \"track\"."
  (if (string-match "spotify:\\([^:]+\\):" uri)
      (match-string 1 uri)
    nil))

;;;; Entity storage

(defvar-local spofy-library--entities nil
  "Hash table mapping entity URIs to their full API response alists.
Buffer-local in every Spofy library buffer.")

(defun spofy-library--store-entity (uri entity)
  "Store ENTITY alist under URI in the buffer-local entity table."
  (unless spofy-library--entities
    (setq spofy-library--entities (make-hash-table :test #'equal)))
  (puthash uri entity spofy-library--entities))

(defun spofy-library-entity-at-point ()
  "Return the full alist for the entity on the current tabulated-list row.
The row ID (a Spotify URI) is used as the key into
`spofy-library--entities'."
  (when-let* ((id (tabulated-list-get-id)))
    (and spofy-library--entities
         (gethash id spofy-library--entities))))

(defun spofy-library--entity-type-at-point ()
  "Return the entity type at point based on buffer context.
Returns \"track\" or \"album\"."
  (symbol-name (or spofy-ui--entity-type 'track)))

;;;; Now-playing indicator

(defun spofy-library--playing-indicator (track-uri)
  "Return a now-playing indicator string for TRACK-URI.
Shows a play icon if TRACK-URI matches the currently playing track."
  (let* ((current-id (and (fboundp 'spofy-player-current-track-id)
                          (spofy-player-current-track-id)))
         (playing-p (and current-id
                         (string-match-p (regexp-quote current-id) track-uri))))
    (if playing-p
        (propertize "\u25B6" 'face 'spofy-playing-icon)
      "")))

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
    (define-key map (kbd "g")   #'spofy-library-tracks-refresh)
    (define-key map (kbd "m")   #'spofy-library-tracks-load-more)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `spofy-library-tracks-mode'.")

(define-derived-mode spofy-library-tracks-mode tabulated-list-mode
  "Spofy Library Tracks"
  "Major mode for viewing saved tracks in the user's Spotify library."
  :group 'spofy
  (setq tabulated-list-format
        (vector '(" "         2 nil)
                `("Name"      ,(spofy-ui-col 'library-track 0) t)
                `("Artist(s)" ,(spofy-ui-col 'library-track 1) t)
                `("Album"     ,(spofy-ui-col 'library-track 2) t)
                '("Duration"  6 nil :right-align t))
        tabulated-list-padding 2)
  (setq-local spofy-library--entities (make-hash-table :test #'equal))
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
         (playing (spofy-library--playing-indicator uri)))
    (spofy-library--store-entity uri track)
    (list uri
          (vector playing
                  (if (string-empty-p playing)
                      (propertize (spofy-ui-truncate name (spofy-ui-col 'library-track 0)) 'face 'spofy-track-name)
                    (spofy-ui-propertize-playing (spofy-ui-truncate name (spofy-ui-col 'library-track 0))))
                  (if (string-empty-p playing)
                      (propertize (spofy-ui-truncate artist-str (spofy-ui-col 'library-track 1)) 'face 'spofy-artist-name)
                    (spofy-ui-propertize-playing (spofy-ui-truncate artist-str (spofy-ui-col 'library-track 1))))
                  (if (string-empty-p playing)
                      (propertize (spofy-ui-truncate album-name (spofy-ui-col 'library-track 2)) 'face 'spofy-album-name)
                    (spofy-ui-propertize-playing (spofy-ui-truncate album-name (spofy-ui-col 'library-track 2))))
                  (propertize duration-str 'face 'spofy-muted)))))

(defun spofy-library--render-tracks (response)
  "Render saved tracks RESPONSE into the *Spofy Saved Tracks* buffer."
  (let* ((paged (spofy-api--extract-paged-results response))
         (items (alist-get 'items paged))
         (next-url (alist-get 'next paged))
         (buf-name "*Spofy Saved Tracks*"))
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (spofy-library-tracks-mode)
      (setq-local spofy-ui--next-page-url next-url)
      (setq tabulated-list-entries
            (cl-loop for item across items
                     collect (spofy-library--format-saved-track item)))
      (tabulated-list-print t)
      (spofy-ui-insert-pagination-footer)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun spofy-library-saved-tracks ()
  "List the user's saved tracks from Spotify.
Fetches tracks from the /me/tracks endpoint and displays them
in a tabulated-list buffer."
  (interactive)
  (spofy-api-get "me/tracks" '(("limit" . "50"))
                 #'spofy-library--render-tracks))

(defun spofy-library-tracks-play ()
  "Play the track at point."
  (interactive)
  (when-let* ((uri (tabulated-list-get-id)))
    (spofy-play-track uri)))

(defun spofy-library-tracks-view-album ()
  "View the album for the track at point."
  (interactive)
  (when-let* ((entity (spofy-library-entity-at-point))
              (album (alist-get 'album entity))
              (album-id (alist-get 'id album)))
    (spofy-view-album album-id)))

(defun spofy-library-tracks-view-artist ()
  "View the artist for the track at point."
  (interactive)
  (when-let* ((entity (spofy-library-entity-at-point))
              (artists (alist-get 'artists entity))
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
  (spofy-library-saved-tracks))

(defun spofy-library-tracks-load-more ()
  "Load the next page of saved tracks and append to the current buffer."
  (interactive)
  (if (null spofy-ui--next-page-url)
      (message "Spofy: no more tracks to load.")
    (let ((buf (current-buffer)))
      (spofy-api--request
       "GET" spofy-ui--next-page-url nil nil
       (lambda (response)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (let* ((items (alist-get 'items response))
                    (next-url (alist-get 'next response))
                    (new-entries
                     (cl-loop for item across items
                              collect (spofy-library--format-saved-track item))))
               (setq-local spofy-ui--next-page-url next-url)
               (setq tabulated-list-entries
                     (append tabulated-list-entries new-entries))
               (tabulated-list-print t)
               (spofy-ui-insert-pagination-footer)
               (message "Spofy: loaded %d more tracks."
                        (length new-entries))))))))))

;;; ========================================================================
;;;; Saved albums
;;; ========================================================================

(defvar spofy-library-albums-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'spofy-library-albums-view)
    (define-key map (kbd "p")   #'spofy-library-albums-play)
    (define-key map (kbd "A")   #'spofy-library-albums-view-artist)
    (define-key map (kbd "S")   #'spofy-library-albums-unsave)
    (define-key map (kbd "g")   #'spofy-library-albums-refresh)
    (define-key map (kbd "m")   #'spofy-library-albums-load-more)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `spofy-library-albums-mode'.")

(define-derived-mode spofy-library-albums-mode tabulated-list-mode
  "Spofy Library Albums"
  "Major mode for viewing saved albums in the user's Spotify library."
  :group 'spofy
  (setq tabulated-list-format
        (vector `("Name"      ,(spofy-ui-col 'library-album 0) t)
                `("Artist(s)" ,(spofy-ui-col 'library-album 1) t)
                '("Year"      6 t)
                '("Tracks"    6 nil :right-align t))
        tabulated-list-padding 2)
  (setq-local spofy-library--entities (make-hash-table :test #'equal))
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
         (release-date (or (alist-get 'release_date album) ""))
         (year (if (>= (length release-date) 4)
                   (substring release-date 0 4)
                 release-date))
         (total-tracks (or (alist-get 'total_tracks album) 0)))
    (spofy-library--store-entity uri album)
    (list uri
          (vector (propertize (spofy-ui-truncate name (spofy-ui-col 'library-album 0)) 'face 'spofy-album-name)
                  (propertize (spofy-ui-truncate artist-str (spofy-ui-col 'library-album 1)) 'face 'spofy-artist-name)
                  (propertize year 'face 'spofy-muted)
                  (propertize (number-to-string total-tracks)
                              'face 'spofy-muted)))))

(defun spofy-library--render-albums (response)
  "Render saved albums RESPONSE into the *Spofy Saved Albums* buffer."
  (let* ((paged (spofy-api--extract-paged-results response))
         (items (alist-get 'items paged))
         (next-url (alist-get 'next paged))
         (buf-name "*Spofy Saved Albums*"))
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (spofy-library-albums-mode)
      (setq-local spofy-ui--next-page-url next-url)
      (setq tabulated-list-entries
            (cl-loop for item across items
                     collect (spofy-library--format-saved-album item)))
      (tabulated-list-print t)
      (spofy-ui-insert-pagination-footer)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun spofy-library-saved-albums ()
  "List the user's saved albums from Spotify.
Fetches albums from the /me/albums endpoint and displays them
in a tabulated-list buffer."
  (interactive)
  (spofy-api-get "me/albums" '(("limit" . "50"))
                 #'spofy-library--render-albums))

(defun spofy-library-albums-view ()
  "View the album at point."
  (interactive)
  (when-let* ((uri (tabulated-list-get-id))
              (album-id (spofy-library--extract-id uri)))
    (spofy-view-album album-id)))

(defun spofy-library-albums-play ()
  "Play the album at point."
  (interactive)
  (when-let* ((uri (tabulated-list-get-id)))
    (spofy-play-context uri)))

(defun spofy-library-albums-view-artist ()
  "View the artist for the album at point."
  (interactive)
  (when-let* ((entity (spofy-library-entity-at-point))
              (artists (alist-get 'artists entity))
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
  (spofy-library-saved-albums))

(defun spofy-library-albums-load-more ()
  "Load the next page of saved albums and append to the current buffer."
  (interactive)
  (if (null spofy-ui--next-page-url)
      (message "Spofy: no more albums to load.")
    (let ((buf (current-buffer)))
      (spofy-api--request
       "GET" spofy-ui--next-page-url nil nil
       (lambda (response)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (let* ((items (alist-get 'items response))
                    (next-url (alist-get 'next response))
                    (new-entries
                     (cl-loop for item across items
                              collect (spofy-library--format-saved-album item))))
               (setq-local spofy-ui--next-page-url next-url)
               (setq tabulated-list-entries
                     (append tabulated-list-entries new-entries))
               (tabulated-list-print t)
               (spofy-ui-insert-pagination-footer)
               (message "Spofy: loaded %d more albums."
                        (length new-entries))))))))))

;;; ========================================================================
;;;; Save / unsave commands
;;; ========================================================================

;;;###autoload
(defun spofy-library-save (uri)
  "Save the Spotify entity identified by URI to the user's library.
URI should be a Spotify URI like \"spotify:track:ID\" or
\"spotify:album:ID\".  The entity type is determined from the URI."
  (interactive
   (list (or (tabulated-list-get-id)
             (read-string "Spotify URI: "))))
  (let* ((type (spofy-library--extract-type uri))
         (id (spofy-library--extract-id uri))
         (endpoint (pcase type
                     ("track" "me/tracks")
                     ("album" "me/albums")
                     (_ nil))))
    (if (not endpoint)
        (message "Spofy: cannot save entity of type \"%s\"." (or type "unknown"))
      (spofy-api-put endpoint `((ids . [,id]))
                     (lambda (_response)
                       (message "Spofy: saved %s to library." type))))))

;;;###autoload
(defun spofy-library-unsave (uri)
  "Remove the Spotify entity identified by URI from the user's library.
URI should be a Spotify URI like \"spotify:track:ID\" or
\"spotify:album:ID\".  The entity type is determined from the URI."
  (interactive
   (list (or (tabulated-list-get-id)
             (read-string "Spotify URI: "))))
  (let* ((type (spofy-library--extract-type uri))
         (id (spofy-library--extract-id uri))
         (endpoint (pcase type
                     ("track" "me/tracks")
                     ("album" "me/albums")
                     (_ nil))))
    (if (not endpoint)
        (message "Spofy: cannot unsave entity of type \"%s\"." (or type "unknown"))
      (spofy-api-delete endpoint `((ids . [,id]))
                        (lambda (_response)
                          (message "Spofy: removed %s from library." type))))))

(provide 'spofy-library)
;;; spofy-library.el ends here
