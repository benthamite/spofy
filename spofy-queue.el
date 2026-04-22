;;; spofy-queue.el --- Queue management for Spofy  -*- lexical-binding: t; -*-

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

;; View and manipulate the Spotify playback queue.  Provides
;; `spofy-view-queue' for listing upcoming tracks and
;; `spofy-add-to-queue' for enqueueing a track or podcast episode.

;;; Code:

(require 'spofy-api)
(require 'spofy-ui)
(require 'cl-lib)
(require 'url-util)

(declare-function spofy-play-pause "spofy-player" ())
(declare-function spofy-play-track "spofy-player"
                  (uri &optional context-uri position))
(declare-function spofy-view-album "spofy-browse" (album-id))
(declare-function spofy-view-artist "spofy-browse" (artist-id))

;;;; Mode

(defvar spofy-queue-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'spofy-queue-play-track)
    (define-key map (kbd "a")   #'spofy-queue-view-album)
    (define-key map (kbd "A")   #'spofy-queue-view-artist)
    (define-key map (kbd "SPC") #'spofy-play-pause)
    (define-key map (kbd "g")   #'spofy-view-queue)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `spofy-queue-mode'.")

(define-derived-mode spofy-queue-mode tabulated-list-mode "Spofy Queue"
  "Major mode for viewing the Spotify playback queue."
  :group 'spofy
  (setq tabulated-list-padding 2)
  (spofy-ui-set-format
   'library-track
   '(("Name"      :flex t)
     ("Artist(s)" :flex t)
     ("Album"     :flex t)
     ("Duration"  6 nil :right-align t)))
  (setq-local spofy-ui--entity-type 'track)
  (tabulated-list-init-header))

;;;; Commands

;;;###autoload
(defun spofy-view-queue ()
  "Open a buffer listing upcoming tracks in the Spotify queue."
  (interactive)
  (spofy-api-get "me/player/queue" nil #'spofy-queue--render))

;;;###autoload
(defun spofy-add-to-queue (uri)
  "Add URI to the Spotify playback queue.
URI must be a Spotify track or episode URI.  When called
interactively, uses the URI at point or prompts for one."
  (interactive
   (list (or (and (derived-mode-p 'tabulated-list-mode)
                  (tabulated-list-get-id))
             (read-string "Spotify URI: "))))
  (unless (spofy-queue--queueable-uri-p uri)
    (user-error "Spofy: %s is not a track or episode URI" uri))
  (spofy-api-post
   (format "me/player/queue?uri=%s" (url-hexify-string uri))
   nil
   (lambda (_) (message "Spofy: added to queue"))))

(defun spofy-queue-play-track ()
  "Play the track at point."
  (interactive)
  (when-let* ((uri (tabulated-list-get-id)))
    (spofy-play-track uri)))

(defun spofy-queue-view-album ()
  "View the album for the track at point."
  (interactive)
  (when-let* ((entity (spofy-ui-entity-at-point))
              (album (alist-get 'album entity))
              (album-id (alist-get 'id album)))
    (spofy-view-album album-id)))

(defun spofy-queue-view-artist ()
  "View the artist for the track at point."
  (interactive)
  (when-let* ((entity (spofy-ui-entity-at-point))
              (artists (alist-get 'artists entity))
              (notempty (> (length artists) 0))
              (artist (aref artists 0))
              (artist-id (alist-get 'id artist)))
    (spofy-view-artist artist-id)))

;;;; Helpers

(defun spofy-queue--queueable-uri-p (uri)
  "Return non-nil if URI is a Spotify track or episode URI."
  (and (stringp uri)
       (string-match-p "\\`spotify:\\(track\\|episode\\):" uri)))

(defun spofy-queue--render (response)
  "Render queue RESPONSE into the *Spofy Queue* buffer."
  (let* ((currently (alist-get 'currently_playing response))
         (queue (or (alist-get 'queue response) []))
         (buf-name "*Spofy Queue*"))
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (spofy-queue-mode)
      (spofy-ui-insert-header (spofy-queue--header currently queue))
      (setq tabulated-list-entries
            (cl-loop for track across queue
                     collect (spofy-queue--format-track track)))
      (tabulated-list-print t)
      (goto-char (point-min))
      (switch-to-buffer (current-buffer)))))

(defun spofy-queue--format-track (track)
  "Format TRACK alist as a `tabulated-list-entries' entry."
  (let* ((uri (alist-get 'uri track))
         (name (or (alist-get 'name track) ""))
         (artists (or (alist-get 'artists track) []))
         (album (alist-get 'album track))
         (album-name (if album (or (alist-get 'name album) "") ""))
         (duration-ms (or (alist-get 'duration_ms track) 0))
         (artist-str (spofy-ui-format-artists artists))
         (duration-str (spofy-ui-format-duration-ms duration-ms)))
    (spofy-ui-store-entity uri track)
    (list uri
          (vector
           (spofy-ui-truncate name (spofy-ui-col 'library-track 0)
                              'spofy-track-name)
           (spofy-ui-truncate artist-str (spofy-ui-col 'library-track 1)
                              'spofy-artist-name)
           (spofy-ui-truncate album-name (spofy-ui-col 'library-track 2)
                              'spofy-album-name)
           (propertize duration-str 'face 'spofy-muted)))))

(defun spofy-queue--header (currently queue)
  "Return header-line strings for a queue buffer.
CURRENTLY is the currently-playing track alist (or nil).  QUEUE
is the vector of upcoming tracks."
  (let* ((count (length queue))
         (upcoming (format "Upcoming: %d track%s"
                           count (if (= count 1) "" "s"))))
    (if currently
        (list (format "Currently playing: %s — %s"
                      (or (alist-get 'name currently) "")
                      (spofy-ui-format-artists
                       (or (alist-get 'artists currently) [])))
              upcoming)
      (list upcoming))))

(provide 'spofy-queue)
;;; spofy-queue.el ends here
