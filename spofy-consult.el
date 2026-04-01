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

;;;; Helpers

(defun spofy-consult--format-track (track)
  "Format TRACK alist as a consult candidate string.
The full entity is stored as a text property."
  (let* ((name (or (alist-get 'name track) ""))
         (artists (or (alist-get 'artists track) []))
         (album (alist-get 'album track))
         (album-name (or (and album (alist-get 'name album)) ""))
         (artist-str (spofy-ui-format-artists artists))
         (candidate (format "%s \u2014 %s (%s)" name artist-str album-name)))
    (propertize candidate 'spofy-entity track)))

(defun spofy-consult--format-album (album)
  "Format ALBUM alist as a consult candidate string.
The full entity is stored as a text property."
  (let* ((name (or (alist-get 'name album) ""))
         (artists (or (alist-get 'artists album) []))
         (artist-str (spofy-ui-format-artists artists))
         (release-date (or (alist-get 'release_date album) ""))
         (year (if (>= (length release-date) 4)
                   (substring release-date 0 4)
                 release-date))
         (candidate (format "%s \u2014 %s (%s)" name artist-str year)))
    (propertize candidate 'spofy-entity album)))

(defun spofy-consult--format-artist (artist)
  "Format ARTIST alist as a consult candidate string.
The full entity is stored as a text property."
  (let* ((name (or (alist-get 'name artist) ""))
         (genres (or (alist-get 'genres artist) []))
         (genres-str (if (> (length genres) 0)
                         (mapconcat #'identity genres ", ")
                       ""))
         (candidate (format "%s (%s)" name genres-str)))
    (propertize candidate 'spofy-entity artist)))

(defun spofy-consult--format-playlist (playlist)
  "Format PLAYLIST alist as a consult candidate string.
The full entity is stored as a text property."
  (let* ((name (or (alist-get 'name playlist) ""))
         (tracks-info (alist-get 'tracks playlist))
         (total (or (and tracks-info (alist-get 'total tracks-info)) 0))
         (candidate (format "%s (%d tracks)" name total)))
    (propertize candidate 'spofy-entity playlist)))

(defun spofy-consult--format-device (device)
  "Format DEVICE alist as a consult candidate string.
The full entity is stored as a text property."
  (let* ((name (or (alist-get 'name device) ""))
         (type (or (alist-get 'type device) ""))
         (candidate (format "%s (%s)" name type)))
    (propertize candidate 'spofy-entity device)))

(defun spofy-consult--get-entity (candidate)
  "Extract the spofy entity from CANDIDATE text properties."
  (get-text-property 0 'spofy-entity candidate))

;;;; Async search builder

(defun spofy-consult--search-collection (type format-fn)
  "Return a dynamic collection function searching Spotify for TYPE.
FORMAT-FN is called on each result item to produce a candidate string.
The returned function accepts INPUT and CALLBACK arguments as required
by `consult--dynamic-collection'."
  (lambda (input callback)
    (spofy-api-get
     "search"
     `(("q" . ,input) ("type" . ,type) ("limit" . "20"))
     (lambda (response)
       (let* ((section-key (intern (concat type "s")))
              (section (alist-get section-key response))
              (items (alist-get 'items section))
              (candidates (when items
                            (mapcar format-fn (append items nil)))))
         (funcall callback (or candidates nil)))))))

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
  "Return a dynamic collection function for the user's playlists.
Results are fetched once from the API and filtered client-side."
  (let ((cache nil)
        (fetched nil))
    (lambda (_input callback)
      (if fetched
          (funcall callback cache)
        (spofy-api-get
         "me/playlists" '(("limit" . "50"))
         (lambda (response)
           (let* ((items (alist-get 'items response))
                  (candidates (when items
                                (mapcar #'spofy-consult--format-playlist
                                        (append items nil)))))
             (setq cache (or candidates nil))
             (setq fetched t)
             (funcall callback cache))))))))

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
  "Return a dynamic collection function for Spotify devices.
Results are fetched once from the API and filtered client-side."
  (let ((cache nil)
        (fetched nil))
    (lambda (_input callback)
      (if fetched
          (funcall callback cache)
        (spofy-api-get
         "me/player/devices" nil
         (lambda (data)
           (let* ((devices (alist-get 'devices data))
                  (candidates (when devices
                                (mapcar #'spofy-consult--format-device
                                        (append devices nil)))))
             (setq cache (or candidates nil))
             (setq fetched t)
             (funcall callback cache))))))))

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
