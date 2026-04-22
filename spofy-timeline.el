;;; spofy-timeline.el --- Unified history/now-playing/queue view  -*- lexical-binding: t; -*-

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

;; `spofy-view-timeline' opens a single buffer that combines three
;; chronologically-ordered sections: recently played tracks at the top,
;; the currently playing track in the middle, and upcoming queue
;; entries at the bottom.  The buffer refreshes automatically on every
;; track change while it is visible.
;;
;; Row actions (RET, Q, a, A, s) work on history and queue rows as well
;; as on the now-playing row.

;;; Code:

(require 'spofy-api)
(require 'spofy-ui)
(require 'cl-lib)

(defvar spofy-player--current-state)
(declare-function spofy-play-pause "spofy-player" ())
(declare-function spofy-play-track "spofy-player"
                  (uri &optional context-uri position))
(declare-function spofy-view-album "spofy-browse" (album-id))
(declare-function spofy-view-artist "spofy-browse" (artist-id))
(declare-function spofy-library-save "spofy-library" (uri-or-type &optional id))
(declare-function spofy-add-to-queue "spofy-queue" (uri))

;;;; User options

(defcustom spofy-timeline-history-limit 20
  "Number of past tracks to display in the timeline's history section."
  :type 'integer
  :group 'spofy)

(defcustom spofy-timeline-column-widths '(35 25 25)
  "Column widths for Name, Artist, and Album in the timeline view."
  :type '(list integer integer integer)
  :group 'spofy)

(defconst spofy-timeline--buffer-name "*Spofy Timeline*"
  "Name of the buffer that displays the unified timeline.")

;;;; Mode

(defvar spofy-timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'spofy-timeline-play-at-point)
    (define-key map (kbd "a")   #'spofy-timeline-view-album-at-point)
    (define-key map (kbd "A")   #'spofy-timeline-view-artist-at-point)
    (define-key map (kbd "s")   #'spofy-timeline-save-at-point)
    (define-key map (kbd "Q")   #'spofy-timeline-queue-at-point)
    (define-key map (kbd "SPC") #'spofy-play-pause)
    (define-key map (kbd "g")   #'spofy-view-timeline)
    (define-key map (kbd "n")   #'next-line)
    (define-key map (kbd "p")   #'previous-line)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `spofy-timeline-mode'.")

(define-derived-mode spofy-timeline-mode special-mode "Spofy Timeline"
  "Major mode for the combined history/now-playing/queue view."
  :group 'spofy
  (buffer-disable-undo)
  (setq truncate-lines t))

;;;; Interactive commands

;;;###autoload
(defun spofy-view-timeline ()
  "Open the combined history / now-playing / queue timeline buffer."
  (interactive)
  (let ((buf (get-buffer-create spofy-timeline--buffer-name)))
    (spofy-timeline--fetch
     (lambda (history queue currently)
       (when (buffer-live-p buf)
         (spofy-timeline--render buf history queue currently))))
    (pop-to-buffer buf)))

(defun spofy-timeline-play-at-point ()
  "Play the track at point."
  (interactive)
  (when-let* ((entity (spofy-timeline--entity-at-point))
              (uri (alist-get 'uri entity)))
    (spofy-play-track uri)))

(defun spofy-timeline-view-album-at-point ()
  "View the album for the track at point."
  (interactive)
  (when-let* ((entity (spofy-timeline--entity-at-point))
              (album (alist-get 'album entity))
              (album-id (alist-get 'id album)))
    (spofy-view-album album-id)))

(defun spofy-timeline-view-artist-at-point ()
  "View the first artist for the track at point."
  (interactive)
  (when-let* ((entity (spofy-timeline--entity-at-point))
              (artists (alist-get 'artists entity))
              ((> (length artists) 0))
              (artist (aref artists 0))
              (artist-id (alist-get 'id artist)))
    (spofy-view-artist artist-id)))

(defun spofy-timeline-save-at-point ()
  "Save the track at point to the user's library."
  (interactive)
  (when-let* ((entity (spofy-timeline--entity-at-point))
              (uri (alist-get 'uri entity)))
    (spofy-library-save uri)))

(defun spofy-timeline-queue-at-point ()
  "Add the track at point to the playback queue."
  (interactive)
  (when-let* ((entity (spofy-timeline--entity-at-point))
              (uri (alist-get 'uri entity)))
    (spofy-add-to-queue uri)))

;;;; Track-change hook

(defun spofy-timeline--refresh-if-visible ()
  "Silently refresh the timeline buffer if it is visible."
  (when-let* ((buf (get-buffer spofy-timeline--buffer-name))
              ((get-buffer-window buf 'visible)))
    (spofy-timeline--fetch
     (lambda (history queue currently)
       (when (buffer-live-p buf)
         (spofy-timeline--render buf history queue currently))))))

(add-hook 'spofy-player-track-changed-hook
          #'spofy-timeline--refresh-if-visible)

;;;; Helpers

(defun spofy-timeline--entity-at-point ()
  "Return the track alist on the current line, or nil."
  (get-text-property (point) 'spofy-entity))

(defun spofy-timeline--fetch (callback)
  "Fetch history and queue data, then call CALLBACK.
CALLBACK is called with three arguments: the history items vector,
the queue items vector, and the currently-playing track alist (or
nil)."
  (let ((history nil) (queue nil) (currently nil) (done 0))
    (cl-labels
        ((maybe-finish ()
           (setq done (1+ done))
           (when (= done 2)
             (funcall callback history queue currently))))
      (spofy-api-get
       "me/player/recently-played"
       `(("limit" . ,(number-to-string spofy-timeline-history-limit)))
       (lambda (response)
         (setq history (or (alist-get 'items response) []))
         (maybe-finish)))
      (spofy-api-get
       "me/player/queue" nil
       (lambda (response)
         (setq queue (or (alist-get 'queue response) []))
         (setq currently (alist-get 'currently_playing response))
         (maybe-finish))))))

(defun spofy-timeline--render (buf history queue currently)
  "Render HISTORY, CURRENTLY, and QUEUE into BUF."
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (spofy-timeline-mode)
      (erase-buffer)
      (spofy-timeline--insert-section
       "Recently played"
       (spofy-timeline--history-tracks history)
       'history)
      (spofy-timeline--insert-section
       "Now playing"
       (when currently (list currently))
       'now-playing)
      (spofy-timeline--insert-section
       "Up next"
       (append queue nil)
       'queue)
      (setq-local buffer-read-only t))
    (spofy-timeline--goto-now-playing)))

(defun spofy-timeline--history-tracks (history)
  "Return HISTORY items as a list of track alists, oldest first."
  (let ((tracks (cl-loop for item across history
                         for track = (alist-get 'track item)
                         when track collect track)))
    (nreverse tracks)))

(defun spofy-timeline--insert-section (title tracks section)
  "Insert a section titled TITLE containing TRACKS, tagged as SECTION.
SECTION is a symbol: `history', `now-playing', or `queue'."
  (insert (propertize (format "%s\n" title) 'face 'spofy-header))
  (insert (propertize (make-string 60 ?─) 'face 'spofy-header) "\n")
  (if (null tracks)
      (insert (propertize "  (empty)\n" 'face 'spofy-muted))
    (dolist (track tracks)
      (spofy-timeline--insert-track track section)))
  (insert "\n"))

(defun spofy-timeline--insert-track (track section)
  "Insert a single TRACK row tagged as SECTION."
  (pcase-let*
      ((`(,name-w ,artist-w ,album-w) spofy-timeline-column-widths)
       (uri (alist-get 'uri track))
       (name (or (alist-get 'name track) ""))
       (artists (or (alist-get 'artists track) []))
       (album (alist-get 'album track))
       (album-name (if album (or (alist-get 'name album) "") ""))
       (duration-ms (or (alist-get 'duration_ms track) 0))
       (duration-str (spofy-ui-format-duration-ms duration-ms))
       (artist-str (spofy-ui-format-artists artists))
       (playing (eq section 'now-playing))
       (indicator (if playing
                      (propertize "▶ " 'face 'spofy-playing-icon)
                    "  "))
       (face-track (spofy-ui-playing-face 'spofy-track-name playing))
       (face-artist (spofy-ui-playing-face 'spofy-artist-name playing))
       (face-album (spofy-ui-playing-face 'spofy-album-name playing))
       (line-start (point)))
    (insert indicator)
    (insert (spofy-ui-truncate name name-w face-track))
    (insert "  ")
    (insert (spofy-ui-truncate artist-str artist-w face-artist))
    (insert "  ")
    (insert (spofy-ui-truncate album-name album-w face-album))
    (insert "  ")
    (insert (propertize duration-str 'face 'spofy-muted))
    (insert "\n")
    (add-text-properties line-start (point)
                         (list 'spofy-entity track
                               'spofy-uri uri
                               'spofy-section section
                               'follow-link t))))

(defun spofy-timeline--goto-now-playing ()
  "Move point to the now-playing row and recenter if visible."
  (goto-char (point-min))
  (when-let* ((match (text-property-search-forward
                      'spofy-section 'now-playing t)))
    (goto-char (prop-match-beginning match))
    (beginning-of-line)
    (when-let* ((win (get-buffer-window (current-buffer) 'visible))
                ((eq win (selected-window))))
      (recenter))))

(provide 'spofy-timeline)
;;; spofy-timeline.el ends here
