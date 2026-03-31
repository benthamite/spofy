;;; spofy-mode-line.el --- Mode-line display for Spofy  -*- lexical-binding: t; -*-

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

;; Mode-line display for the Spofy Spotify client.  Shows the currently
;; playing track in the Emacs mode line with a configurable format string.
;; Enable `spofy-mode-line-mode' to activate.

;;; Code:

(require 'spofy-ui)

(defvar spofy-player--current-state)
(defvar spofy-player-state-changed-hook)

;;;; Customization

(defcustom spofy-mode-line-format "▶ %t — %a"
  "Format string for the Spofy mode-line segment.
The following format specifiers are supported:
  %t  track name
  %a  artist name
  %b  album name
  %p  play/pause icon (▶ or ⏸)
  %s  shuffle indicator (🔀 when on, empty when off)
  %r  repeat indicator (🔁 for context, 🔂 for track, empty for off)"
  :type 'string
  :group 'spofy)

(defcustom spofy-mode-line-max-length 40
  "Maximum length of the Spofy mode-line string.
If the formatted string exceeds this length, it is truncated with an
ellipsis."
  :type 'integer
  :group 'spofy)

(defcustom spofy-mode-line-separator " "
  "Separator inserted before the Spofy mode-line segment."
  :type 'string
  :group 'spofy)

;;;; Internal state

(defvar spofy-mode-line--string nil
  "Cached mode-line string, updated on player state changes.")

(defvar spofy-mode-line--construct
  '(:eval (spofy-mode-line--render))
  "Mode-line construct for the Spofy segment.")

(put 'spofy-mode-line--construct 'risky-local-variable t)

;;;; Building the mode-line string

(defun spofy-mode-line--play-pause-icon (state)
  "Return the play/pause icon based on STATE."
  (if (alist-get 'is-playing state) "▶" "⏸"))

(defun spofy-mode-line--shuffle-indicator (state)
  "Return the shuffle indicator based on STATE."
  (if (alist-get 'shuffle state) "🔀" ""))

(defun spofy-mode-line--repeat-indicator (state)
  "Return the repeat indicator based on STATE."
  (pcase (alist-get 'repeat state)
    ("context" "🔁")
    ("track"   "🔂")
    (_         "")))

(defun spofy-mode-line--construct-string ()
  "Build the mode-line string from `spofy-mode-line-format' and current state.
Returns nil if no player state is available."
  (when-let* ((state spofy-player--current-state)
              (track (or (alist-get 'track state) ""))
              (artist (or (alist-get 'artist state) ""))
              (album (or (alist-get 'album state) "")))
    (let* ((play-pause (spofy-mode-line--play-pause-icon state))
           (shuffle (spofy-mode-line--shuffle-indicator state))
           (repeat (spofy-mode-line--repeat-indicator state))
           (propertized-track (propertize track 'face 'spofy-track-name))
           (propertized-artist (propertize artist 'face 'spofy-artist-name))
           (result spofy-mode-line-format))
      ;; Replace format specifiers.  Use propertized versions for %t and %a.
      (setq result (string-replace "%t" propertized-track result))
      (setq result (string-replace "%a" propertized-artist result))
      (setq result (string-replace "%b" album result))
      (setq result (string-replace "%p" play-pause result))
      (setq result (string-replace "%s" shuffle result))
      (setq result (string-replace "%r" repeat result))
      ;; Truncate if needed.
      (spofy-ui-truncate result spofy-mode-line-max-length))))

;;;; Rendering and updating

(defun spofy-mode-line--render ()
  "Return the cached mode-line string with separator, or empty string."
  (if spofy-mode-line--string
      (concat spofy-mode-line-separator spofy-mode-line--string)
    ""))

(defun spofy-mode-line--update ()
  "Rebuild the cached mode-line string and force a mode-line update."
  (setq spofy-mode-line--string (spofy-mode-line--construct-string))
  (force-mode-line-update t))

;;;; Global minor mode

;;;###autoload
(define-minor-mode spofy-mode-line-mode
  "Global minor mode to display the current Spotify track in the mode line."
  :global t
  :group 'spofy
  (if spofy-mode-line-mode
      (progn
        (add-to-list 'global-mode-string spofy-mode-line--construct t)
        (add-hook 'spofy-player-state-changed-hook #'spofy-mode-line--update)
        (spofy-mode-line--update))
    (setq global-mode-string
          (delete spofy-mode-line--construct global-mode-string))
    (remove-hook 'spofy-player-state-changed-hook #'spofy-mode-line--update)
    (setq spofy-mode-line--string nil)
    (force-mode-line-update t)))

(provide 'spofy-mode-line)
;;; spofy-mode-line.el ends here
