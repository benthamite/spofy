;;; spofy-embark.el --- Embark integration for Spofy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;; Author: Pablo Stafforini
;; URL: https://github.com/pablostafforini/spofy
;; Package-Requires: ((emacs "30.1") (embark "1.0"))

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

;; Embark action categories and keymaps for Spotify entities.  Provides
;; contextual actions (play, save, view, copy URI) for tracks, albums,
;; artists, and playlists selected via Consult or tabulated-list buffers.

;;; Code:

(require 'embark nil t)

(defvar embark-general-map)
(defvar embark-keymap-alist)

(declare-function spofy-play-track "spofy-player" (uri &optional context-uri))
(declare-function spofy-play-context "spofy-player" (context-uri))
(declare-function spofy-playlist-add-track "spofy-playlist" (track-uri))
(declare-function spofy-library-save "spofy-library" (uri-or-type &optional id))
(declare-function spofy-view-album "spofy-browse" (album-id))
(declare-function spofy-view-artist "spofy-browse" (artist-id))
(declare-function spofy-view-playlist "spofy-browse" (playlist-id))
(declare-function spofy-view-artist-top-tracks "spofy-browse" (artist-id artist-name))
(declare-function spofy-follow-playlist "spofy-playlist" (playlist-id-or-uri))
(declare-function spofy-unfollow-playlist "spofy-playlist" ())
(declare-function spofy-wikipedia-track "spofy-wikipedia" (target))
(declare-function spofy-wikipedia-album "spofy-wikipedia" (target))
(declare-function spofy-wikipedia-artist "spofy-wikipedia" (target))

;;;; Entity extraction

(defun spofy-embark--get-entity (target)
  "Extract the spofy entity alist from TARGET string's text properties."
  (get-text-property 0 'spofy-entity target))

(defun spofy-embark--get-uri (target)
  "Extract the Spotify URI from TARGET.
First tries the `spofy-entity' text property, then falls back to
treating TARGET as a URI directly."
  (if-let* ((entity (spofy-embark--get-entity target)))
      (alist-get 'uri entity)
    target))

(defun spofy-embark--get-id (target)
  "Extract the Spotify ID from TARGET."
  (if-let* ((entity (spofy-embark--get-entity target)))
      (alist-get 'id entity)
    (when (string-match "spotify:[^:]+:\\(.+\\)" target)
      (match-string 1 target))))

;;;; Copy URI helper

;;;###autoload
(defun spofy-embark-copy-uri (target)
  "Copy the Spotify URI of TARGET to the kill ring."
  (interactive "sTarget: ")
  (let ((uri (spofy-embark--get-uri target)))
    (if uri
        (progn
          (kill-new uri)
          (message "Spofy: copied %s" uri))
      (message "Spofy: no URI found."))))

;;;; Track actions

(defun spofy-embark-play-track (target)
  "Play the track identified by TARGET."
  (when-let* ((uri (spofy-embark--get-uri target)))
    (spofy-play-track uri)))

(defun spofy-embark-add-track-to-playlist (target)
  "Add the track identified by TARGET to a playlist."
  (when-let* ((uri (spofy-embark--get-uri target)))
    (spofy-playlist-add-track uri)))

(defun spofy-embark-save-track (target)
  "Save the track identified by TARGET to the user's library."
  (when-let* ((uri (spofy-embark--get-uri target)))
    (spofy-library-save uri)))

(defun spofy-embark-track-view-album (target)
  "View the album of the track identified by TARGET."
  (when-let* ((entity (spofy-embark--get-entity target))
              (album (alist-get 'album entity))
              (album-id (alist-get 'id album)))
    (spofy-view-album album-id)))

(defun spofy-embark-track-view-artist (target)
  "View the artist of the track identified by TARGET."
  (when-let* ((entity (spofy-embark--get-entity target))
              (artists (alist-get 'artists entity))
              ((_notempty (> (length artists) 0)))
              (artist (aref artists 0))
              (artist-id (alist-get 'id artist)))
    (spofy-view-artist artist-id)))

;;;; Album actions

(defun spofy-embark-play-album (target)
  "Play the album identified by TARGET."
  (when-let* ((uri (spofy-embark--get-uri target)))
    (spofy-play-context uri)))

(defun spofy-embark-save-album (target)
  "Save the album identified by TARGET to the user's library."
  (when-let* ((uri (spofy-embark--get-uri target)))
    (spofy-library-save uri)))

(defun spofy-embark-album-view-tracks (target)
  "View the tracks of the album identified by TARGET."
  (when-let* ((album-id (spofy-embark--get-id target)))
    (spofy-view-album album-id)))

(defun spofy-embark-album-view-artist (target)
  "View the artist of the album identified by TARGET."
  (when-let* ((entity (spofy-embark--get-entity target))
              (artists (alist-get 'artists entity))
              ((_notempty (> (length artists) 0)))
              (artist (aref artists 0))
              (artist-id (alist-get 'id artist)))
    (spofy-view-artist artist-id)))

;;;; Artist actions

(defun spofy-embark-artist-play-top-tracks (target)
  "Play the top tracks of the artist identified by TARGET."
  (when-let* ((entity (spofy-embark--get-entity target))
              (artist-id (alist-get 'id entity))
              (artist-name (alist-get 'name entity)))
    (spofy-view-artist-top-tracks artist-id artist-name)))

(defun spofy-embark-artist-view-albums (target)
  "View the albums of the artist identified by TARGET."
  (when-let* ((artist-id (spofy-embark--get-id target)))
    (spofy-view-artist artist-id)))

;;;; Playlist actions

(defun spofy-embark-play-playlist (target)
  "Play the playlist identified by TARGET."
  (when-let* ((uri (spofy-embark--get-uri target)))
    (spofy-play-context uri)))

(defun spofy-embark-view-playlist (target)
  "View the tracks of the playlist identified by TARGET."
  (when-let* ((playlist-id (spofy-embark--get-id target)))
    (spofy-view-playlist playlist-id)))

(defun spofy-embark-follow-playlist (target)
  "Follow the playlist identified by TARGET."
  (when-let* ((playlist-id (spofy-embark--get-id target)))
    (spofy-follow-playlist playlist-id)))

(defun spofy-embark-unfollow-playlist (target)
  "Unfollow the playlist identified by TARGET."
  (when-let* ((uri (spofy-embark--get-uri target)))
    ;; spofy-unfollow-playlist is interactive and works at point, so we
    ;; use the API directly here for programmatic access.
    (let ((playlist-id (spofy-embark--get-id target)))
      (when playlist-id
        (spofy-api-delete
         (format "playlists/%s/followers" playlist-id)
         nil
         (lambda (_)
           (message "Spofy: unfollowed playlist.")))))))

(declare-function spofy-api-delete "spofy-api" (endpoint &optional data callback))

;;;; Keymaps

(defun spofy-embark-track-wikipedia (target)
  "Open the Wikipedia article for the album/work of TARGET track."
  (require 'spofy-wikipedia)
  (spofy-wikipedia-track target))

(defvar-keymap spofy-embark-track-map
  :doc "Embark keymap for Spofy tracks."
  :parent embark-general-map
  "RET" #'spofy-embark-play-track
  "p"   #'spofy-embark-add-track-to-playlist
  "s"   #'spofy-embark-save-track
  "a"   #'spofy-embark-track-view-album
  "A"   #'spofy-embark-track-view-artist
  "w"   #'spofy-embark-track-wikipedia
  "W"   #'spofy-embark-copy-uri)

(defun spofy-embark-album-wikipedia (target)
  "Open the Wikipedia article for TARGET album."
  (require 'spofy-wikipedia)
  (spofy-wikipedia-album target))

(defvar-keymap spofy-embark-album-map
  :doc "Embark keymap for Spofy albums."
  :parent embark-general-map
  "RET" #'spofy-embark-play-album
  "s"   #'spofy-embark-save-album
  "v"   #'spofy-embark-album-view-tracks
  "A"   #'spofy-embark-album-view-artist
  "w"   #'spofy-embark-album-wikipedia
  "W"   #'spofy-embark-copy-uri)

(defun spofy-embark-artist-wikipedia (target)
  "Open the Wikipedia article for TARGET artist."
  (require 'spofy-wikipedia)
  (spofy-wikipedia-artist target))

(defvar-keymap spofy-embark-artist-map
  :doc "Embark keymap for Spofy artists."
  :parent embark-general-map
  "RET" #'spofy-embark-artist-play-top-tracks
  "v"   #'spofy-embark-artist-view-albums
  "w"   #'spofy-embark-artist-wikipedia
  "W"   #'spofy-embark-copy-uri)

(defvar-keymap spofy-embark-playlist-map
  :doc "Embark keymap for Spofy playlists."
  :parent embark-general-map
  "RET" #'spofy-embark-play-playlist
  "v"   #'spofy-embark-view-playlist
  "f"   #'spofy-embark-follow-playlist
  "u"   #'spofy-embark-unfollow-playlist
  "w"   #'spofy-embark-copy-uri)

;;;; Registration

(add-to-list 'embark-keymap-alist '(spofy-track . spofy-embark-track-map))
(add-to-list 'embark-keymap-alist '(spofy-album . spofy-embark-album-map))
(add-to-list 'embark-keymap-alist '(spofy-artist . spofy-embark-artist-map))
(add-to-list 'embark-keymap-alist '(spofy-playlist . spofy-embark-playlist-map))

(provide 'spofy-embark)
;;; spofy-embark.el ends here
