;;; spofy-search.el --- Search commands for Spofy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;; Author: Pablo Stafforini
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

;; Search commands for the Spofy Spotify client.  Provides interactive
;; search for tracks, albums, artists, and playlists, displaying results
;; in `tabulated-list-mode' buffers with pagination support.

;;; Code:

(require 'spofy-api)
(require 'spofy-ui)
(require 'tabulated-list)

(declare-function spofy-player-current-track-id "spofy-player" ())
(declare-function spofy-play-context "spofy-player" (context-uri))
(declare-function spofy-play-track "spofy-player" (track-uri &optional context-uri))
(declare-function spofy-view-album "spofy-browse" (album-id))
(declare-function spofy-view-artist "spofy-browse" (artist-id))
(declare-function spofy-playlist-add-track "spofy-playlist" (track-uri))
(declare-function spofy-library-save "spofy-library" (uri))
(declare-function spofy-library-unsave "spofy-library" (uri))
(declare-function consult-spofy-track "spofy-consult" ())
(declare-function consult-spofy-album "spofy-consult" ())
(declare-function consult-spofy-artist "spofy-consult" ())
(declare-function consult-spofy-playlist "spofy-consult" ())

;;;; Buffer-local variables

(defvar-local spofy-search--entities (make-hash-table :test #'equal)
  "Hash table mapping entity URIs to their full API response alists.")

(defvar-local spofy-search--query nil
  "The search query used for the current buffer.")

(defvar-local spofy-search--type nil
  "The search type used for the current buffer (e.g. \"track\").")

;;;; Mode definition

(defvar spofy-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'spofy-search-play-item)
    (define-key map (kbd "a")   #'spofy-search-view-album)
    (define-key map (kbd "A")   #'spofy-search-view-artist)
    (define-key map (kbd "p")   #'spofy-search-add-to-playlist)
    (define-key map (kbd "s")   #'spofy-search-save)
    (define-key map (kbd "S")   #'spofy-search-unsave)
    (define-key map (kbd "g")   #'spofy-search-refresh)
    (define-key map (kbd "m")   #'spofy-search-load-more)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `spofy-search-mode'.")

(define-derived-mode spofy-search-mode tabulated-list-mode "Spofy-Search"
  "Major mode for Spofy search results.
Derived from `tabulated-list-mode' with keybindings for playing,
browsing, and managing Spotify entities."
  :group 'spofy
  (setq-local spofy-search--entities (make-hash-table :test #'equal)))

;;;; Formatting helpers

(defun spofy-search--format-track-entry (track)
  "Format TRACK alist as a `tabulated-list-entries' entry.
Return a list of (ID [COLUMNS...])."
  (let* ((uri (alist-get 'uri track))
         (name (or (alist-get 'name track) ""))
         (artists (or (alist-get 'artists track) []))
         (album (or (alist-get 'album track) nil))
         (album-name (or (and album (alist-get 'name album)) ""))
         (duration-ms (or (alist-get 'duration_ms track) 0))
         (artist-str (spofy-ui-format-artists artists))
         (duration-str (spofy-ui-format-duration-ms duration-ms))
         (current-id (condition-case nil
                         (spofy-player-current-track-id)
                       (error nil)))
         (track-id (alist-get 'id track))
         (playing-p (and current-id track-id
                         (string= current-id track-id)))
         (play-icon (if playing-p
                        (propertize "▶" 'face 'spofy-playing-icon)
                      " ")))
    (list uri
          (vector play-icon
                  (propertize (spofy-ui-truncate name 35) 'face
                              (if playing-p 'spofy-playing 'spofy-track-name))
                  (propertize (spofy-ui-truncate artist-str 25) 'face 'spofy-artist-name)
                  (propertize (spofy-ui-truncate album-name 25) 'face 'spofy-album-name)
                  (propertize duration-str 'face 'spofy-muted)))))

(defun spofy-search--format-album-entry (album)
  "Format ALBUM alist as a `tabulated-list-entries' entry.
Return a list of (ID [COLUMNS...])."
  (let* ((uri (alist-get 'uri album))
         (name (or (alist-get 'name album) ""))
         (artists (or (alist-get 'artists album) []))
         (artist-str (spofy-ui-format-artists artists))
         (release-date (or (alist-get 'release_date album) ""))
         (year (if (>= (length release-date) 4)
                   (substring release-date 0 4)
                 release-date))
         (total-tracks (or (alist-get 'total_tracks album) 0)))
    (list uri
          (vector (propertize (spofy-ui-truncate name 35) 'face 'spofy-album-name)
                  (propertize (spofy-ui-truncate artist-str 25) 'face 'spofy-artist-name)
                  (propertize year 'face 'spofy-muted)
                  (propertize (number-to-string total-tracks) 'face 'spofy-muted)))))

(defun spofy-search--format-artist-entry (artist)
  "Format ARTIST alist as a `tabulated-list-entries' entry.
Return a list of (ID [COLUMNS...])."
  (let* ((uri (alist-get 'uri artist))
         (name (or (alist-get 'name artist) ""))
         (genres (or (alist-get 'genres artist) []))
         (genres-str (mapconcat #'identity genres ", "))
         (followers (or (alist-get 'followers artist) nil))
         (follower-count (or (and followers (alist-get 'total followers)) 0))
         (followers-str (number-to-string follower-count)))
    (list uri
          (vector (propertize (spofy-ui-truncate name 35) 'face 'spofy-artist-name)
                  (propertize (spofy-ui-truncate genres-str 30) 'face 'spofy-muted)
                  (propertize followers-str 'face 'spofy-muted)))))

(defun spofy-search--format-playlist-entry (playlist)
  "Format PLAYLIST alist as a `tabulated-list-entries' entry.
Return a list of (ID [COLUMNS...])."
  (let* ((uri (alist-get 'uri playlist))
         (name (or (alist-get 'name playlist) ""))
         (owner (or (alist-get 'owner playlist) nil))
         (owner-name (or (and owner (alist-get 'display_name owner)) ""))
         (tracks-info (or (alist-get 'tracks playlist) nil))
         (total-tracks (or (and tracks-info (alist-get 'total tracks-info)) 0))
         (public-p (alist-get 'public playlist))
         (public-str (if public-p "Yes" "No")))
    (list uri
          (vector (propertize (spofy-ui-truncate name 35) 'face 'spofy-track-name)
                  (propertize (spofy-ui-truncate owner-name 20) 'face 'spofy-muted)
                  (propertize (number-to-string total-tracks) 'face 'spofy-muted)
                  (propertize public-str 'face 'spofy-muted)))))

;;;; Column definitions

(defconst spofy-search--track-columns
  [(" " 2 nil) ("Name" 35 nil) ("Artist(s)" 25 nil) ("Album" 25 nil) ("Duration" 6 nil)]
  "Column format for track search results.")

(defconst spofy-search--album-columns
  [("Name" 35 nil) ("Artist(s)" 25 nil) ("Year" 6 nil) ("Tracks" 6 nil)]
  "Column format for album search results.")

(defconst spofy-search--artist-columns
  [("Name" 35 nil) ("Genres" 30 nil) ("Followers" 12 nil)]
  "Column format for artist search results.")

(defconst spofy-search--playlist-columns
  [("Name" 35 nil) ("Owner" 20 nil) ("Tracks" 8 nil) ("Public" 7 nil)]
  "Column format for playlist search results.")

;;;; Search internals

(defun spofy-search--type-plural (type)
  "Return the plural form of TYPE (e.g. \"track\" -> \"tracks\")."
  (concat type "s"))

(defun spofy-search--columns-for-type (type)
  "Return the column format vector for search TYPE."
  (pcase type
    ("track" spofy-search--track-columns)
    ("album" spofy-search--album-columns)
    ("artist" spofy-search--artist-columns)
    ("playlist" spofy-search--playlist-columns)))

(defun spofy-search--format-entry (type entity)
  "Format ENTITY according to search TYPE.
Return a `tabulated-list-entries' entry (ID [COLUMNS...])."
  (pcase type
    ("track" (spofy-search--format-track-entry entity))
    ("album" (spofy-search--format-album-entry entity))
    ("artist" (spofy-search--format-artist-entry entity))
    ("playlist" (spofy-search--format-playlist-entry entity))))

(defun spofy-search--store-entities (entities-table items type)
  "Store ITEMS in ENTITIES-TABLE, keyed by URI.
TYPE is the search type (unused but reserved for future use)."
  (ignore type)
  (seq-doseq (item items)
    (let ((uri (alist-get 'uri item)))
      (when uri
        (puthash uri item entities-table)))))

(defun spofy-search--display-results (type query items next-url)
  "Display search ITEMS of TYPE for QUERY in a tabulated-list buffer.
NEXT-URL is the URL for the next page of results, or nil."
  (let ((buf-name (format "*Spofy Search: %s*" (spofy-search--type-plural type)))
        (columns (spofy-search--columns-for-type type)))
    (with-current-buffer (get-buffer-create buf-name)
      (spofy-search-mode)
      (setq spofy-search--query query)
      (setq spofy-search--type type)
      (setq spofy-ui--next-page-url next-url)
      (setq spofy-ui--entity-type type)
      (setq tabulated-list-format columns)
      (let ((entries (mapcar (lambda (item)
                               (spofy-search--format-entry type item))
                             (append items nil))))
        (spofy-search--store-entities spofy-search--entities items type)
        (setq tabulated-list-entries entries))
      (tabulated-list-init-header)
      (tabulated-list-print t)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(defun spofy-search--perform (type query)
  "Search Spotify for QUERY of TYPE and display results."
  (spofy-api-get
   "search"
   `(("q" . ,query) ("type" . ,type) ("limit" . "20"))
   (lambda (response)
     (let* ((section-key (intern (spofy-search--type-plural type)))
            (section (alist-get section-key response))
            (items (alist-get 'items section))
            (next-url (alist-get 'next section)))
       (if items
           (spofy-search--display-results type query items next-url)
         (message "Spofy: no results for \"%s\"." query))))))

;;;; Interactive commands

;;;###autoload
(defun spofy-search-tracks (query)
  "Search Spotify for tracks matching QUERY.
When `spofy-consult' is loaded, use incremental search instead."
  (interactive
   (list (unless (require 'spofy-consult nil t)
           (read-string "Search tracks: "))))
  (if query
      (spofy-search--perform "track" query)
    (consult-spofy-track)))

;;;###autoload
(defun spofy-search-albums (query)
  "Search Spotify for albums matching QUERY.
When `spofy-consult' is loaded, use incremental search instead."
  (interactive
   (list (unless (require 'spofy-consult nil t)
           (read-string "Search albums: "))))
  (if query
      (spofy-search--perform "album" query)
    (consult-spofy-album)))

;;;###autoload
(defun spofy-search-artists (query)
  "Search Spotify for artists matching QUERY.
When `spofy-consult' is loaded, use incremental search instead."
  (interactive
   (list (unless (require 'spofy-consult nil t)
           (read-string "Search artists: "))))
  (if query
      (spofy-search--perform "artist" query)
    (consult-spofy-artist)))

;;;###autoload
(defun spofy-search-playlists (query)
  "Search Spotify for playlists matching QUERY.
When `spofy-consult' is loaded, use incremental search instead."
  (interactive
   (list (unless (require 'spofy-consult nil t)
           (read-string "Search playlists: "))))
  (if query
      (spofy-search--perform "playlist" query)
    (consult-spofy-playlist)))

;;;; Actions

(defun spofy-search--entity-at-point ()
  "Return the entity alist for the item at point, or nil."
  (when-let* ((id (tabulated-list-get-id)))
    (gethash id spofy-search--entities)))

(defun spofy-search--uri-at-point ()
  "Return the Spotify URI for the item at point, or nil."
  (tabulated-list-get-id))

(defun spofy-search-play-item ()
  "Play the item at point.
For tracks, play the track directly.  For albums and playlists,
play the context (all tracks from the beginning)."
  (interactive)
  (when-let* ((uri (spofy-search--uri-at-point)))
    (pcase spofy-search--type
      ("track" (spofy-play-track uri))
      ((or "album" "playlist") (spofy-play-context uri))
      ("artist" (spofy-play-context uri)))))

(defun spofy-search-view-album ()
  "View the album for the item at point."
  (interactive)
  (when-let* ((entity (spofy-search--entity-at-point)))
    (let ((album-id
           (pcase spofy-search--type
             ("track"
              (let ((album (alist-get 'album entity)))
                (alist-get 'id album)))
             ("album"
              (alist-get 'id entity)))))
      (if album-id
          (spofy-view-album album-id)
        (message "Spofy: no album for this item.")))))

(defun spofy-search-view-artist ()
  "View the artist for the item at point."
  (interactive)
  (when-let* ((entity (spofy-search--entity-at-point)))
    (let ((artist-id
           (pcase spofy-search--type
             ("artist"
              (alist-get 'id entity))
             ((or "track" "album")
              (let ((artists (alist-get 'artists entity)))
                (when (and artists (> (length artists) 0))
                  (alist-get 'id (aref artists 0))))))))
      (if artist-id
          (spofy-view-artist artist-id)
        (message "Spofy: no artist for this item.")))))

(defun spofy-search-add-to-playlist ()
  "Add the track at point to a playlist."
  (interactive)
  (if (not (string= spofy-search--type "track"))
      (message "Spofy: can only add tracks to playlists.")
    (when-let* ((uri (spofy-search--uri-at-point)))
      (spofy-playlist-add-track uri))))

(defun spofy-search-save ()
  "Save the item at point to the user's library."
  (interactive)
  (when-let* ((uri (spofy-search--uri-at-point)))
    (spofy-library-save uri)))

(defun spofy-search-unsave ()
  "Remove the item at point from the user's library."
  (interactive)
  (when-let* ((uri (spofy-search--uri-at-point)))
    (spofy-library-unsave uri)))

(defun spofy-search-refresh ()
  "Re-run the current search query."
  (interactive)
  (when (and spofy-search--type spofy-search--query)
    (spofy-search--perform spofy-search--type spofy-search--query)))

(defun spofy-search-load-more ()
  "Load the next page of search results and append to the current buffer."
  (interactive)
  (if (not spofy-ui--next-page-url)
      (message "Spofy: no more results to load.")
    (let ((type spofy-search--type)
          (buf (current-buffer)))
      (spofy-api--request
       "GET" spofy-ui--next-page-url nil nil
       (lambda (response)
         (let* ((section-key (intern (spofy-search--type-plural type)))
                (section (alist-get section-key response))
                (items (alist-get 'items section))
                (next-url (alist-get 'next section)))
           (when (and items (buffer-live-p buf))
             (with-current-buffer buf
               (spofy-search--store-entities spofy-search--entities items type)
               (let ((new-entries (mapcar (lambda (item)
                                            (spofy-search--format-entry type item))
                                          (append items nil))))
                 (setq tabulated-list-entries
                       (append tabulated-list-entries new-entries)))
               (setq spofy-ui--next-page-url next-url)
               (tabulated-list-print t)))))))))

(provide 'spofy-search)
;;; spofy-search.el ends here
