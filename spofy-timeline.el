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
;; entries at the bottom.  The buffer auto-refreshes on every track
;; change while it is visible, and recenters on the now-playing row.
;;
;; Row actions (RET, Q, a, A, s, g) work on history and queue rows as
;; well as on the now-playing row.  Playing a track from the timeline
;; preserves the current playback context when one is active.

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

(defconst spofy-timeline--buffer-name "*Spofy Timeline*"
  "Name of the buffer that displays the unified timeline.")

(defconst spofy-timeline--section-titles
  '((":sep:history" . "Recently played")
    (":sep:now"     . "Now playing")
    (":sep:queue"   . "Up next"))
  "Titles for section-separator entry IDs.")

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
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `spofy-timeline-mode'.")

;; Install remaps outside the `defvar', which skips reinitialization on
;; reload, so the bindings reach a running Emacs when the file changes.
(define-key spofy-timeline-mode-map [remap next-line]
            #'spofy-timeline-next-line)
(define-key spofy-timeline-mode-map [remap previous-line]
            #'spofy-timeline-previous-line)

(define-derived-mode spofy-timeline-mode tabulated-list-mode "Spofy Timeline"
  "Major mode for the combined history/now-playing/queue view."
  :group 'spofy
  (setq tabulated-list-padding 2)
  (spofy-ui-set-format
   'library-track
   '((" "         2 nil)
     ("Name"      :flex nil)
     ("Artist(s)" :flex nil)
     ("Album"     :flex nil)
     ("Duration"  6 nil :right-align t)))
  (setq-local spofy-ui--entity-type 'track)
  (setq-local spofy-ui--hide-header-line t)
  (setq-local tabulated-list-printer #'spofy-timeline--printer)
  (setq-local spofy-ui--entry-formatter #'spofy-timeline--reformat-entry)
  (tabulated-list-init-header)
  (setq header-line-format nil)
  (spofy-ui-enable-line-highlight))

;;;; Interactive commands

;;;###autoload
(defun spofy-view-timeline ()
  "Open the combined history / now-playing / queue timeline buffer."
  (interactive)
  (let ((buf (get-buffer-create spofy-timeline--buffer-name)))
    (with-current-buffer buf
      (spofy-timeline-mode))
    (spofy-timeline--fetch
     (lambda (history queue currently)
       (when (buffer-live-p buf)
         (spofy-timeline--render buf history queue currently))))
    (pop-to-buffer buf)))

(defun spofy-timeline-play-at-point ()
  "Play the track at point.
For tracks in the \"Up next\" section, preserves the current
playback context so the remainder of the queue stays intact.
For history and now-playing rows, plays the bare track URI, since
historical plays are often unrelated to the current context."
  (interactive)
  (when-let* ((entity (spofy-timeline--entity-at-point))
              (uri (alist-get 'uri entity)))
    (spofy-play-track
     uri (when (eq (spofy-timeline--section-at-point) 'queue)
           (alist-get 'context-uri spofy-player--current-state)))))

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
  (spofy-ui-entity-at-point))

(defun spofy-timeline--section-at-point ()
  "Return the section symbol (`history', `now', `queue') for the row at point.
Returns nil when point is not under any section."
  (save-excursion
    (when (text-property-search-backward 'spofy-sep-section)
      (get-text-property (point) 'spofy-sep-section))))

(defun spofy-timeline--fetch (callback)
  "Fetch history and queue data, then call CALLBACK.
CALLBACK is called with three arguments: the history items
vector, the queue items vector, and the currently-playing track
alist (or nil)."
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
    (unless (derived-mode-p 'spofy-timeline-mode)
      (spofy-timeline-mode))
    (setq tabulated-list-entries
          (spofy-timeline--build-entries history queue currently))
    (tabulated-list-print t)
    (spofy-timeline--goto-now-playing)))

(defun spofy-timeline--build-entries (history queue currently)
  "Return a list of tabulated-list entries from HISTORY, QUEUE, and CURRENTLY."
  (let ((entries (list (spofy-timeline--separator-entry ":sep:history"))))
    (dolist (track (spofy-timeline--history-tracks history))
      (push (spofy-timeline--format-track track 'history) entries))
    (push (spofy-timeline--separator-entry ":sep:now") entries)
    (when currently
      (push (spofy-timeline--format-track currently 'now-playing) entries))
    (push (spofy-timeline--separator-entry ":sep:queue") entries)
    (cl-loop for track across queue
             do (push (spofy-timeline--format-track track 'queue) entries))
    (nreverse entries)))

(defun spofy-timeline--history-tracks (history)
  "Return HISTORY items as a list of track alists, oldest first."
  (let ((tracks (cl-loop for item across history
                         for track = (alist-get 'track item)
                         when track collect track)))
    (nreverse tracks)))

(defun spofy-timeline--separator-entry (id)
  "Return a tabulated-list entry representing a section separator ID."
  (list id (vector "" "" "" "" "")))

(defun spofy-timeline--format-track (track section)
  "Format TRACK alist as a `tabulated-list-entries' entry.
SECTION is `history', `now-playing', or `queue' and fully
determines whether the row gets the now-playing face.  A track
that also appears in the history section is not highlighted just
because its URI happens to match the currently playing one — only
the authoritative `now-playing' row carries the bold styling."
  (let* ((uri (alist-get 'uri track))
         (name (or (alist-get 'name track) ""))
         (artists (or (alist-get 'artists track) []))
         (album (alist-get 'album track))
         (album-name (if album (or (alist-get 'name album) "") ""))
         (duration-ms (or (alist-get 'duration_ms track) 0))
         (artist-str (spofy-ui-format-artists artists))
         (duration-str (spofy-ui-format-duration-ms duration-ms))
         (playing (eq section 'now-playing))
         (indicator " "))
    (spofy-ui-store-entity uri track)
    (list uri
          (vector indicator
                  (spofy-ui-truncate
                   name (spofy-ui-col 'library-track 0)
                   (spofy-ui-playing-face 'spofy-track-name playing))
                  (spofy-ui-truncate
                   artist-str (spofy-ui-col 'library-track 1)
                   (spofy-ui-playing-face 'spofy-artist-name playing))
                  (spofy-ui-truncate
                   album-name (spofy-ui-col 'library-track 2)
                   (spofy-ui-playing-face 'spofy-album-name playing))
                  (propertize duration-str 'face
                              (spofy-ui-playing-face 'spofy-muted playing))))))

(defun spofy-timeline--reformat-entry (entity _idx)
  "Re-format ENTITY as a track row on track change.
Called before the async refresh has returned fresh data, so rows
are still in their old sections.  Highlight whichever row's URI
matches the just-updated current track; the full rerender via
`spofy-timeline--refresh-if-visible' follows shortly afterwards
and replaces this with the authoritative section layout."
  (let ((section (if (spofy-timeline--current-uri-p
                      (alist-get 'uri entity))
                     'now-playing
                   'queue)))
    (spofy-timeline--format-track entity section)))

(defun spofy-timeline--current-uri-p (uri)
  "Return non-nil when URI belongs to the currently playing track."
  (and uri (not (string-empty-p (spofy-ui-playing-indicator uri)))))

(defun spofy-timeline--printer (id cols)
  "Custom tabulated-list printer.
Render section separators specially; delegate track rows to
`tabulated-list-print-entry'.  ID is the row ID, COLS its columns."
  (if (spofy-timeline--separator-id-p id)
      (spofy-timeline--insert-separator id)
    (tabulated-list-print-entry id cols)))

(defun spofy-timeline--separator-id-p (id)
  "Return non-nil when ID is a section-separator entry ID."
  (and (stringp id) (string-prefix-p ":sep:" id)))

(defun spofy-timeline--insert-separator (id)
  "Insert a section separator block for the separator ID."
  (unless (bobp) (insert "\n"))
  (let* ((start (point))
         (title (alist-get id spofy-timeline--section-titles
                           nil nil #'equal))
         (pad (make-string (or tabulated-list-padding 0) ?\s)))
    (insert pad (propertize title 'face 'spofy-header) "\n\n")
    (put-text-property start (point) 'spofy-sep-section
                       (intern (substring id 5)))))

(defun spofy-timeline--goto-now-playing ()
  "Move point to the now-playing row and center it in every visible window.
Centering every window (not only the selected one) keeps the
current track anchored visually as the history and queue sections
grow or shrink around it, so the row never drifts into the
\"Up next\" area."
  (goto-char (point-min))
  (when-let* ((match (text-property-search-forward
                      'spofy-sep-section 'now t)))
    (let ((now-pt (prop-match-end match)))
      (goto-char now-pt)
      (dolist (win (get-buffer-window-list (current-buffer) nil 'visible))
        (spofy-timeline--recenter-window-on win now-pt)))))

(defun spofy-timeline--recenter-window-on (win pt)
  "Move WIN's point to PT and recenter WIN on that line."
  (set-window-point win pt)
  (with-selected-window win
    (recenter)))

;;;; Track-row navigation

(defun spofy-timeline-next-line (&optional n)
  "Move to the Nth next track row, skipping section separators.
N defaults to 1.  Negative N moves backwards.  Bound to remap
`next-line' so every binding that normally calls `next-line'
(C-n, <down>, etc.) also skips.  Non-motion commands, mouse
clicks, and scroll commands keep their usual behavior — if they
strand point on a separator, press the motion key once to skip
off."
  (interactive "^p")
  (spofy-ui-move-by-matching-lines #'spofy-timeline--track-row-p
                                   (or n 1)))

(defun spofy-timeline-previous-line (&optional n)
  "Move to the Nth previous track row, skipping section separators.
N defaults to 1.  Negative N moves forwards."
  (interactive "^p")
  (spofy-ui-move-by-matching-lines #'spofy-timeline--track-row-p
                                   (- (or n 1))))

(defun spofy-timeline--track-row-p ()
  "Return non-nil when the current row is a track, not a separator."
  (not (null (tabulated-list-get-id))))

(provide 'spofy-timeline)
;;; spofy-timeline.el ends here
