;;; spofy-playlist.el --- Playlist management for Spofy  -*- lexical-binding: t; -*-

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

;; Playlist CRUD operations for the Spofy Spotify client.  Provides
;; commands for listing, creating, renaming, deleting, and reordering
;; playlists, as well as adding and removing tracks.

;;; Code:

(require 'spofy-api)
(require 'spofy-ui)
(require 'cl-lib)

(declare-function spofy-view-playlist "spofy-browse" (playlist-id))
(declare-function spofy-play-context "spofy-player" (context-uri))

;;;; User ID cache

(defvar spofy-playlist--user-id-cache nil
  "Cached Spotify user ID.  Fetched once from GET /me.")

(defun spofy-playlist--user-id (callback)
  "Call CALLBACK with the current user's Spotify ID.
The ID is fetched from GET /me on the first call and cached for
subsequent calls."
  (if spofy-playlist--user-id-cache
      (funcall callback spofy-playlist--user-id-cache)
    (spofy-api-get "me" nil
                   (lambda (data)
                     (let ((id (alist-get 'id data)))
                       (setq spofy-playlist--user-id-cache id)
                       (funcall callback id))))))

;;;; Entity storage

(defvar-local spofy-playlist--entities nil
  "Hash table mapping playlist URIs to their full API alists.
Buffer-local in `spofy-playlists-mode' buffers.")

;;;; Helpers

(defun spofy-playlist--extract-id-from-uri (uri)
  "Extract the Spotify ID from URI (e.g. \"spotify:playlist:xyz\" -> \"xyz\")."
  (if (string-match "spotify:[^:]+:\\(.+\\)" uri)
      (match-string 1 uri)
    uri))

(defun spofy-playlist--entity-at-point ()
  "Return the playlist alist for the entry at point, or nil."
  (when-let* ((id (tabulated-list-get-id)))
    (and spofy-playlist--entities
         (gethash id spofy-playlist--entities))))

(defun spofy-playlist--id-at-point ()
  "Return the Spotify playlist ID for the entry at point, or nil."
  (when-let* ((uri (tabulated-list-get-id)))
    (spofy-playlist--extract-id-from-uri uri)))

(defun spofy-playlist--store-entity (uri entity)
  "Store ENTITY alist under URI in the buffer-local entity table."
  (unless spofy-playlist--entities
    (setq spofy-playlist--entities (make-hash-table :test #'equal)))
  (puthash uri entity spofy-playlist--entities))

;;;; Mode definition

(defvar spofy-playlists-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'spofy-playlists-view)
    (define-key map (kbd "p")   #'spofy-playlists-play)
    (define-key map (kbd "c")   #'spofy-create-playlist)
    (define-key map (kbd "r")   #'spofy-rename-playlist)
    (define-key map (kbd "d")   #'spofy-delete-playlist)
    (define-key map (kbd "f")   #'spofy-follow-playlist)
    (define-key map (kbd "v")   #'spofy-playlist-set-public)
    (define-key map (kbd "g")   #'spofy-playlists-refresh)

    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `spofy-playlists-mode'.")

(define-derived-mode spofy-playlists-mode tabulated-list-mode "Spofy Playlists"
  "Major mode for listing the user's Spotify playlists.
Derived from `tabulated-list-mode'."
  :group 'spofy
  (setq tabulated-list-padding 2)
  (spofy-ui-set-format
   'playlist-list
   '(("Name"    :flex t)
     ("Owner"   :flex t)
     ("Tracks"   8 nil :right-align t)
     ("Public"   7 t)))
  (setq-local spofy-playlist--entities (make-hash-table :test #'equal))
  (tabulated-list-init-header))

;;;; Formatting

(defun spofy-playlist--format-entry (playlist)
  "Format PLAYLIST alist as a `tabulated-list-entries' entry.
Return a list of (ID [COLUMNS...])."
  (let* ((uri (alist-get 'uri playlist))
         (name (or (alist-get 'name playlist) ""))
         (owner (alist-get 'owner playlist))
         (owner-name (or (and owner (alist-get 'display_name owner)) ""))
         (tracks-info (alist-get 'tracks playlist))
         (total-tracks (or (and tracks-info (alist-get 'total tracks-info)) 0))
         (public-p (alist-get 'public playlist))
         (public-str (if public-p "Yes" "No")))
    (spofy-playlist--store-entity uri playlist)
    (list uri
          (vector (spofy-ui-truncate name (spofy-ui-col 'playlist-list 0) 'spofy-track-name)
                  (spofy-ui-truncate owner-name (spofy-ui-col 'playlist-list 1) 'spofy-muted)
                  (propertize (number-to-string total-tracks) 'face 'spofy-muted)
                  (propertize public-str 'face 'spofy-muted)))))

;;;; List playlists

(defun spofy-playlist--display (items next-url)
  "Display playlist ITEMS in the *Spofy Playlists* buffer.
NEXT-URL is the URL for the next page of results, or nil."
  (let ((buf (get-buffer-create "*Spofy Playlists*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer))
      (spofy-playlists-mode)
      (setq spofy-ui--next-page-url next-url)
      (setq spofy-ui--entity-type "playlist")
      (setq-local spofy-ui--load-more-handler
                  (lambda (response)
                    (let ((items (alist-get 'items response))
                          (next-url (alist-get 'next response)))
                      (cons (cl-loop for item across items
                                     collect (spofy-playlist--format-entry item))
                            next-url))))
      (setq tabulated-list-entries
            (cl-loop for item across items
                     collect (spofy-playlist--format-entry item)))
      (tabulated-list-print t)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;;###autoload
(defun spofy-list-playlists ()
  "List the current user's Spotify playlists."
  (interactive)
  (spofy-api-get "me/playlists" '(("limit" . "50"))
                 (lambda (response)
                   (let ((items (alist-get 'items response))
                         (next-url (alist-get 'next response)))
                     (spofy-playlist--display items next-url)))))

;;;; Buffer actions

(defun spofy-playlists-view ()
  "View the playlist at point in a detail buffer."
  (interactive)
  (when-let* ((playlist-id (spofy-playlist--id-at-point)))
    (spofy-view-playlist playlist-id)))

(defun spofy-playlists-play ()
  "Play the playlist at point."
  (interactive)
  (when-let* ((uri (tabulated-list-get-id)))
    (spofy-play-context uri)))

(defun spofy-playlists-refresh ()
  "Refresh the playlists buffer."
  (interactive)
  (spofy-list-playlists))

;;;; Create playlist

;;;###autoload
(defun spofy-create-playlist (name description)
  "Create a new Spotify playlist with NAME and DESCRIPTION.
The playlist is created as private by default.  Refreshes the
playlists buffer if it exists."
  (interactive "sPlaylist name: \nsDescription: ")
  (spofy-playlist--user-id
   (lambda (user-id)
     (spofy-api-post
      (format "users/%s/playlists" user-id)
      `((name . ,name)
        (description . ,description)
        (public . :false))
      (lambda (_response)
        (message "Spofy: created playlist \"%s\"." name)
        (when (get-buffer "*Spofy Playlists*")
          (spofy-list-playlists)))))))

;;;; Rename playlist

(defun spofy-rename-playlist ()
  "Rename the playlist at point.
Prompts for a new name and sends a PUT request to update it."
  (interactive)
  (let ((playlist-id (spofy-playlist--id-at-point))
        (entity (spofy-playlist--entity-at-point)))
    (unless playlist-id
      (user-error "No playlist at point"))
    (let* ((old-name (or (and entity (alist-get 'name entity)) ""))
           (new-name (read-string "New name: " old-name)))
      (spofy-api-put
       (format "playlists/%s" playlist-id)
       `((name . ,new-name))
       (lambda (_)
         (message "Spofy: renamed playlist to \"%s\"." new-name)
         (when (get-buffer "*Spofy Playlists*")
           (spofy-list-playlists)))))))

;;;; Delete (unfollow) playlist

(defun spofy-delete-playlist ()
  "Unfollow (delete) the playlist at point after confirmation."
  (interactive)
  (let ((playlist-id (spofy-playlist--id-at-point))
        (entity (spofy-playlist--entity-at-point)))
    (unless playlist-id
      (user-error "No playlist at point"))
    (let ((name (or (and entity (alist-get 'name entity)) playlist-id)))
      (when (y-or-n-p (format "Unfollow playlist \"%s\"? " name))
        (spofy-api-delete
         (format "playlists/%s/followers" playlist-id)
         nil
         (lambda (_)
           (message "Spofy: unfollowed playlist \"%s\"." name)
           (when (get-buffer "*Spofy Playlists*")
             (spofy-list-playlists))))))))

;;;; Toggle public/private

(defun spofy-playlist-set-public ()
  "Toggle the public/private state of the playlist at point."
  (interactive)
  (let ((playlist-id (spofy-playlist--id-at-point))
        (entity (spofy-playlist--entity-at-point)))
    (unless playlist-id
      (user-error "No playlist at point"))
    (let* ((currently-public (eq (alist-get 'public entity) t))
           (new-public (not currently-public)))
      (spofy-api-put
       (format "playlists/%s" playlist-id)
       `((public . ,(if new-public t :false)))
       (lambda (_)
         (message "Spofy: playlist is now %s."
                  (if new-public "public" "private"))
         (when (get-buffer "*Spofy Playlists*")
           (spofy-list-playlists)))))))

;;;; Follow/unfollow

;;;###autoload
(defun spofy-follow-playlist (playlist-id-or-uri)
  "Follow a Spotify playlist.
PLAYLIST-ID-OR-URI is the playlist ID or URI to follow."
  (interactive "sPlaylist ID or URI: ")
  (let ((playlist-id (spofy-playlist--extract-id-from-uri playlist-id-or-uri)))
    (spofy-api-put
     (format "playlists/%s/followers" playlist-id)
     nil
     (lambda (_)
       (message "Spofy: now following playlist %s." playlist-id)
       (when (get-buffer "*Spofy Playlists*")
         (spofy-list-playlists))))))

(defun spofy-unfollow-playlist ()
  "Unfollow the playlist at point.
This is the same operation as `spofy-delete-playlist'."
  (interactive)
  (call-interactively #'spofy-delete-playlist))

;;;; Add track to playlist

;;;###autoload
(defun spofy-playlist-add-track (track-uri)
  "Add TRACK-URI to a playlist chosen via `completing-read'.
If called interactively without a prefix argument, attempts to
get the track URI from the entity at point."
  (interactive
   (list (or (tabulated-list-get-id)
             (read-string "Track URI: "))))
  (spofy-api-get
   "me/playlists" '(("limit" . "50"))
   (lambda (response)
     (let* ((items (alist-get 'items response))
            (playlists (append items nil))
            (names (mapcar (lambda (pl) (or (alist-get 'name pl) ""))
                           playlists))
            (choice (completing-read "Add to playlist: " names nil t))
            (target (cl-find choice playlists
                             :key (lambda (pl) (alist-get 'name pl))
                             :test #'equal))
            (playlist-id (and target (alist-get 'id target))))
       (if (not playlist-id)
           (message "Spofy: no playlist selected.")
         (spofy-api-post
          (format "playlists/%s/tracks" playlist-id)
          `((uris . [,track-uri]))
          (lambda (_)
            (message "Spofy: added track to \"%s\"." choice))))))))

;;;; Remove track from playlist

(defun spofy-playlist-remove-track (&optional playlist-id track-uri)
  "Remove TRACK-URI from PLAYLIST-ID.
When called interactively, operate on the playlist track at point
using `spofy-ui--buffer-context'."
  (interactive)
  (let ((playlist-id (or playlist-id
                         (alist-get 'playlist-id spofy-ui--buffer-context)))
        (track-uri (or track-uri
                       (tabulated-list-get-id))))
    (unless playlist-id
      (user-error "Not in a playlist detail buffer"))
    (unless track-uri
      (user-error "No track at point"))
    (spofy-api-delete
     (format "playlists/%s/tracks" playlist-id)
     `((tracks . [((uri . ,track-uri))]))
     (lambda (_)
       (message "Spofy: removed track from playlist.")
       ;; Refresh the playlist view.
       (when-let* ((pid playlist-id))
         (spofy-view-playlist pid))))))

;;;; Reorder tracks

(defun spofy-playlist-reorder (new-position)
  "Move the track at point to NEW-POSITION in the current playlist.
Only works in playlist detail buffers.  Prompts for the target
position number (1-indexed for the user, converted to 0-indexed
for the API)."
  (interactive "nMove to position (1-indexed): ")
  (let ((playlist-id (alist-get 'playlist-id spofy-ui--buffer-context)))
    (unless playlist-id
      (user-error "Not in a playlist detail buffer"))
    (let* ((entries tabulated-list-entries)
           (track-uri (tabulated-list-get-id))
           (current-index
            (cl-position track-uri entries
                         :key #'car :test #'equal)))
      (unless current-index
        (user-error "Cannot determine track position"))
      (let ((insert-before (max 0 (1- new-position))))
        (spofy-api-put
         (format "playlists/%s/tracks" playlist-id)
         `((range_start . ,current-index)
           (insert_before . ,insert-before))
         (lambda (_)
           (message "Spofy: moved track to position %d." new-position)
           (spofy-view-playlist playlist-id)))))))

(provide 'spofy-playlist)
;;; spofy-playlist.el ends here
