;;; spofy-tab-bar.el --- Tab-bar display for Spofy  -*- lexical-binding: t; -*-

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

;; Tab-bar display for the Spofy Spotify client.  Shows the currently
;; playing track in the Emacs tab bar with a configurable format string.
;; Enable `spofy-tab-bar-mode' to activate.

;;; Code:

(require 'spofy-ui)

(defvar spofy-player--current-state)
(defvar spofy-player-state-changed-hook)

;;;; Customization

(defcustom spofy-tab-bar-format "%p %t — %a"
  "Format string for the Spofy tab-bar segment.
The following format specifiers are supported:
  %t  track name
  %a  artist name
  %b  album name
  %p  play/pause icon (▶ or ⏸)
  %s  shuffle indicator (🔀 when on, empty when off)
  %r  repeat indicator (🔁 for context, 🔂 for track, empty for off)"
  :type 'string
  :group 'spofy)

(defcustom spofy-tab-bar-max-length 50
  "Maximum length of the Spofy tab-bar string.
If the formatted string exceeds this length, it is truncated with an
ellipsis."
  :type 'integer
  :group 'spofy)

;;;; Internal state

(defvar spofy-tab-bar--string nil
  "Cached tab-bar string, updated on player state changes.")

;;;; Building the tab-bar string

(defun spofy-tab-bar--play-pause-icon (state)
  "Return the play/pause icon based on STATE."
  (if (alist-get 'is-playing state) "▶" "⏸"))

(defun spofy-tab-bar--shuffle-indicator (state)
  "Return the shuffle indicator based on STATE."
  (if (alist-get 'shuffle state) "🔀" ""))

(defun spofy-tab-bar--repeat-indicator (state)
  "Return the repeat indicator based on STATE."
  (pcase (alist-get 'repeat state)
    ("context" "🔁")
    ("track"   "🔂")
    (_         "")))

(defun spofy-tab-bar--construct-string ()
  "Build the tab-bar string from `spofy-tab-bar-format' and current state.
Returns nil if no player state is available."
  (when-let* ((state spofy-player--current-state)
              (track (or (alist-get 'track state) ""))
              (artist (or (alist-get 'artist state) ""))
              (album (or (alist-get 'album state) "")))
    (let* ((play-pause (spofy-tab-bar--play-pause-icon state))
           (shuffle (spofy-tab-bar--shuffle-indicator state))
           (repeat (spofy-tab-bar--repeat-indicator state))
           (propertized-track (propertize track 'face 'spofy-track-name))
           (propertized-artist (propertize artist 'face 'spofy-artist-name))
           (result spofy-tab-bar-format))
      (setq result (string-replace "%t" propertized-track result))
      (setq result (string-replace "%a" propertized-artist result))
      (setq result (string-replace "%b" album result))
      (setq result (string-replace "%p" play-pause result))
      (setq result (string-replace "%s" shuffle result))
      (setq result (string-replace "%r" repeat result))
      (spofy-ui-truncate result spofy-tab-bar-max-length))))

;;;; Tab-bar format function

(defun tab-bar-format-spofy ()
  "Produce a tab-bar segment showing the currently playing Spotify track."
  (when spofy-tab-bar--string
    `((spofy-tab-bar menu-item ,spofy-tab-bar--string ignore
                     :help "Currently playing on Spotify"))))

;;;; Updating

(defun spofy-tab-bar--update ()
  "Rebuild the cached tab-bar string and force a tab-bar update."
  (setq spofy-tab-bar--string (spofy-tab-bar--construct-string))
  (force-mode-line-update t))

;;;; Global minor mode

;;;###autoload
(define-minor-mode spofy-tab-bar-mode
  "Global minor mode to display the current Spotify track in the tab bar."
  :global t
  :group 'spofy
  (if spofy-tab-bar-mode
      (progn
        (unless (memq 'tab-bar-format-spofy tab-bar-format)
          (setq tab-bar-format (append tab-bar-format '(tab-bar-format-spofy))))
        (add-hook 'spofy-player-state-changed-hook #'spofy-tab-bar--update)
        (spofy-tab-bar--update))
    (setq tab-bar-format (delq 'tab-bar-format-spofy tab-bar-format))
    (remove-hook 'spofy-player-state-changed-hook #'spofy-tab-bar--update)
    (setq spofy-tab-bar--string nil)
    (force-mode-line-update t)))

(provide 'spofy-tab-bar)
;;; spofy-tab-bar.el ends here
