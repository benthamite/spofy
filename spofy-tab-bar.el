;;; spofy-tab-bar.el --- Tab-bar display for spofy  -*- lexical-binding: t; -*-

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

;; Tab-bar display for the spofy Spotify client.  Shows the currently
;; playing track in the Emacs tab bar with a configurable format string.
;; Enable `spofy-tab-bar-mode' to activate.

;;; Code:

(require 'spofy-ui)

(declare-function spofy-player-interpolated-progress "spofy-player" ())

(defvar spofy-player--current-state)
(defvar spofy-player-state-changed-hook)

;;;; Customization

(defcustom spofy-tab-bar-format "%t %a %s%r %G"
  "Format string for the spofy tab-bar segment.
The following format specifiers are supported:
  %t  track name
  %a  artist name
  %b  album name
  %p  play/pause icon (⏸ when playing, ▶ when paused)
  %s  shuffle indicator (⇌ when on, empty when off)
  %r  repeat indicator (↻ for context, ↻₁ for track, empty for off)
  %G  progress bar
  %T  timestamps (e.g. \"1:23 / 3:45\")"
  :type 'string
  :group 'spofy)

(defcustom spofy-tab-bar-progress-width 15
  "Width in characters of the progress bar in the tab bar.
Only used when the format string contains %G."
  :type 'integer
  :group 'spofy)

(defcustom spofy-tab-bar-max-length nil
  "Optional upper bound on the spofy tab-bar string length.
The segment is always truncated dynamically to fit the available
frame width.  When this option is non-nil, the segment is further
capped at this many columns even when more space is available."
  :type '(choice (const :tag "Dynamic only" nil) integer)
  :group 'spofy)

(defcustom spofy-tab-bar-alignment 'left
  "Alignment of the spofy segment in the tab bar.
When `left', the segment is placed before any right-aligned entries.
When `right', the segment is placed after
`tab-bar-format-align-right', inserting it if necessary."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right))
  :group 'spofy)

;;;; Internal state

(defvar spofy-tab-bar--added-align-right nil
  "Non-nil if this mode inserted `tab-bar-format-align-right'.")

;;;; Building the tab-bar string

(defun spofy-tab-bar--construct-string ()
  "Build the tab-bar string from `spofy-tab-bar-format' and current state.
Returns nil if no player state is available.  The text portion
(track, artist, album) is truncated with a fade-out gradient when
it would cause the tab bar to exceed the frame width; the status
portion (indicators, progress bar) is never truncated."
  (when-let* ((state spofy-player--current-state)
              (track (or (alist-get 'track state) ""))
              (artist (or (alist-get 'artist state) ""))
              (album (or (alist-get 'album state) "")))
    (let* ((play-pause (spofy-ui-play-pause-icon state))
           (shuffle (spofy-ui-shuffle-indicator state))
           (repeat (spofy-ui-repeat-indicator state))
           (propertized-track (propertize track 'face 'spofy-track-name))
           (propertized-artist (propertize artist 'face 'spofy-artist-name))
           (parts (spofy-tab-bar--split-format))
           (text (spofy-tab-bar--substitute-text
                  (car parts) propertized-track propertized-artist album))
           (status (spofy-tab-bar--substitute-status
                    (cdr parts) play-pause shuffle repeat state))
           (text (spofy-tab-bar--truncate-text text status)))
      (concat text status))))

(defun spofy-tab-bar--split-format ()
  "Split `spofy-tab-bar-format' at the last variable specifier.
Return (TEXT-FMT . STATUS-FMT) where TEXT-FMT contains %t, %a,
%b and STATUS-FMT contains the remaining fixed-width specifiers."
  (let ((fmt spofy-tab-bar-format)
        (last-var-end 0))
    (dolist (spec '("%t" "%a" "%b"))
      (when-let* ((pos (string-search spec fmt)))
        (setq last-var-end (max last-var-end (+ pos 2)))))
    (if (zerop last-var-end)
        (cons fmt "")
      (cons (substring fmt 0 last-var-end)
            (substring fmt last-var-end)))))

(defun spofy-tab-bar--substitute-text (fmt track artist album)
  "Substitute variable specifiers in FMT with TRACK, ARTIST, ALBUM."
  (let ((result fmt))
    (setq result (string-replace "%t" track result))
    (setq result (string-replace "%a" artist result))
    (string-replace "%b" album result)))

(defun spofy-tab-bar--substitute-status (fmt play-pause shuffle repeat state)
  "Substitute fixed-width specifiers in FMT.
PLAY-PAUSE, SHUFFLE, REPEAT are pre-built indicator strings.
STATE is the player state alist for progress computation."
  (let ((result fmt))
    (setq result (string-replace "%p" play-pause result))
    (setq result (string-replace "%s" shuffle result))
    (setq result (string-replace "%r" repeat result))
    (when (string-match-p "%[GT]" result)
      (let* ((progress (and (fboundp 'spofy-player-interpolated-progress)
                            (spofy-player-interpolated-progress)))
             (duration (alist-get 'duration state))
             (prog-val (or progress 0))
             (dur-val (or duration 0)))
        (when (string-search "%G" result)
          (setq result (string-replace "%G"
                                       (spofy-ui-progress-bar-only
                                        prog-val dur-val
                                        spofy-tab-bar-progress-width)
                                       result)))
        (when (string-search "%T" result)
          (setq result (string-replace "%T"
                                       (spofy-ui-progress-time
                                        prog-val dur-val)
                                       result)))))
    result))

(defun spofy-tab-bar--truncate-text (text status)
  "Truncate TEXT with a fade gradient to fit alongside STATUS.
Returns TEXT unchanged when it already fits."
  (let* ((available (spofy-tab-bar--available-width))
         (budget (if spofy-tab-bar-max-length
                     (min available spofy-tab-bar-max-length)
                   available))
         (text-budget (max 10 (- budget (string-width status)))))
    (if (> (string-width text) text-budget)
        (spofy-tab-bar--apply-string-fade
         (truncate-string-to-width text text-budget))
      text)))

(defun spofy-tab-bar--apply-string-fade (string)
  "Apply a fade-out gradient to the last 3 characters of STRING.
Blend each character's foreground toward the `tab-bar' face
background, producing the same truncation indicator used in spofy
list buffers but encoded directly as text properties rather than
overlays."
  (let* ((len (length string))
         (bg (spofy-tab-bar--fade-background)))
    (when (and (>= len 3) bg)
      (dotimes (i 3)
        (spofy-tab-bar--fade-char string (+ (- len 3) i) (1+ i) bg))))
  string)

(defun spofy-tab-bar--fade-char (string pos level bg)
  "Fade the character at POS in STRING by LEVEL toward BG.
LEVEL is 1, 2, or 3 (25%, 50%, 75% blend toward background)."
  (let* ((existing (get-text-property pos 'face string))
         (fg (or (and existing (spofy-ui--resolve-foreground existing))
                 (face-foreground 'default nil t)))
         (blended (when fg
                    (spofy-ui--blend-color fg bg (* 0.25 level)))))
    (when blended
      (put-text-property
       pos (1+ pos) 'face
       (if existing
           `((:foreground ,blended) ,existing)
         `(:foreground ,blended))
       string))))

(defun spofy-tab-bar--fade-background ()
  "Return the background color to fade toward."
  (or (face-background 'tab-bar nil t)
      (face-background 'default nil t)))

;;;; Available width computation

(defun spofy-tab-bar--available-width ()
  "Return the display columns available for the spofy segment.
Measure the combined width of all other tab-bar items and subtract
from the frame width."
  (let ((other-width 0))
    (dolist (fn tab-bar-format)
      (unless (memq fn '(tab-bar-format-spofy tab-bar-format-align-right))
        (when-let* ((items (ignore-errors (funcall fn))))
          (dolist (item items)
            (when-let* ((label (spofy-tab-bar--item-label item)))
              (setq other-width (+ other-width (string-width label))))))))
    (max 10 (- (frame-width) other-width 1))))

(defun spofy-tab-bar--item-label (item)
  "Extract the display label string from a tab-bar ITEM, or nil."
  (and (consp item)
       (eq (cadr item) 'menu-item)
       (let ((label (caddr item)))
         (and (stringp label) label))))

;;;; Tab-bar format function

(defun tab-bar-format-spofy ()
  "Produce a tab-bar segment showing the currently playing Spotify track.
The string is computed fresh on each call so it adapts to changes
in other tab-bar elements without requiring explicit invalidation."
  (when-let* ((str (spofy-tab-bar--construct-string)))
    `((spofy-tab-bar menu-item ,str ignore
                     :help "Currently playing on Spotify"))))

;;;; Updating

(defun spofy-tab-bar--update ()
  "Force a tab-bar re-render."
  (force-mode-line-update t))

;;;; Tab-bar format management

(defun spofy-tab-bar--insert-into-format ()
  "Insert `tab-bar-format-spofy' into `tab-bar-format'.
Respects `spofy-tab-bar-alignment'."
  (unless (memq 'tab-bar-format-spofy tab-bar-format)
    (pcase spofy-tab-bar-alignment
      ('left (spofy-tab-bar--insert-left))
      ('right (spofy-tab-bar--insert-right)))))

(defun spofy-tab-bar--insert-left ()
  "Insert the spofy segment before any right-aligned entries."
  (if-let* ((tail (memq 'tab-bar-format-align-right tab-bar-format))
            (pos (- (length tab-bar-format) (length tail))))
      (setq tab-bar-format
            (append (take pos tab-bar-format)
                    '(tab-bar-format-spofy)
                    tail))
    (setq tab-bar-format
          (append tab-bar-format '(tab-bar-format-spofy)))))

(defun spofy-tab-bar--insert-right ()
  "Insert the spofy segment at the end, adding alignment if needed."
  (unless (memq 'tab-bar-format-align-right tab-bar-format)
    (setq tab-bar-format
          (append tab-bar-format '(tab-bar-format-align-right)))
    (setq spofy-tab-bar--added-align-right t))
  (setq tab-bar-format
        (append tab-bar-format '(tab-bar-format-spofy))))

(defun spofy-tab-bar--remove-from-format ()
  "Remove `tab-bar-format-spofy' from `tab-bar-format'.
Also removes `tab-bar-format-align-right' if it was added by this mode."
  (setq tab-bar-format (delq 'tab-bar-format-spofy tab-bar-format))
  (when spofy-tab-bar--added-align-right
    (setq tab-bar-format
          (delq 'tab-bar-format-align-right tab-bar-format))
    (setq spofy-tab-bar--added-align-right nil)))

;;;; Progress timer

(defvar spofy-tab-bar--progress-timer nil
  "Timer for updating the tab-bar progress bar every second.")

(defun spofy-tab-bar--needs-progress-timer-p ()
  "Return non-nil if the tab-bar format includes a progress bar or timestamps."
  (string-match-p "%[GT]" spofy-tab-bar-format))

(defun spofy-tab-bar--start-progress-timer ()
  "Start a 1-second timer to refresh the tab-bar progress bar."
  (spofy-tab-bar--stop-progress-timer)
  (when (spofy-tab-bar--needs-progress-timer-p)
    (setq spofy-tab-bar--progress-timer
          (run-with-timer 1 1 #'spofy-tab-bar--update))))

(defun spofy-tab-bar--stop-progress-timer ()
  "Stop the tab-bar progress timer."
  (when spofy-tab-bar--progress-timer
    (cancel-timer spofy-tab-bar--progress-timer)
    (setq spofy-tab-bar--progress-timer nil)))

;;;; Theme change handling

(defvar spofy-tab-bar-mode)

(defun spofy-tab-bar--on-theme-change (&rest _)
  "Rebuild the tab-bar string to recompute fade colors."
  (when spofy-tab-bar-mode
    (spofy-tab-bar--update)))

;;;; Global minor mode

;;;###autoload
(define-minor-mode spofy-tab-bar-mode
  "Global minor mode to display the current Spotify track in the tab bar."
  :global t
  :group 'spofy
  (if spofy-tab-bar-mode
      (progn
        (spofy-tab-bar--insert-into-format)
        (add-hook 'spofy-player-state-changed-hook #'spofy-tab-bar--update)
        (add-hook 'enable-theme-functions #'spofy-tab-bar--on-theme-change)
        (spofy-tab-bar--update)
        (spofy-tab-bar--start-progress-timer))
    (spofy-tab-bar--stop-progress-timer)
    (spofy-tab-bar--remove-from-format)
    (remove-hook 'spofy-player-state-changed-hook #'spofy-tab-bar--update)
    (remove-hook 'enable-theme-functions #'spofy-tab-bar--on-theme-change)
    (force-mode-line-update t)))

(provide 'spofy-tab-bar)
;;; spofy-tab-bar.el ends here
