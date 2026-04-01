;;; spofy-ui.el --- Shared UI utilities for Spofy  -*- lexical-binding: t; -*-

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

;; Shared faces, customization group, and UI helper functions used across
;; all Spofy modules.

;;; Code:

;;;; Customization group

(defgroup spofy nil
  "Spotify client for Emacs."
  :group 'multimedia
  :prefix "spofy-")

;;;; Faces

(defgroup spofy-faces nil
  "Faces for Spofy."
  :group 'spofy
  :group 'faces)

(defface spofy-track-name
  '((t :inherit font-lock-keyword-face))
  "Face for track names."
  :group 'spofy-faces)

(defface spofy-artist-name
  '((t :inherit font-lock-function-name-face))
  "Face for artist names."
  :group 'spofy-faces)

(defface spofy-album-name
  '((t :inherit font-lock-type-face))
  "Face for album names."
  :group 'spofy-faces)

(defface spofy-playing
  '((t :inherit bold :foreground "#1DB954"))
  "Face for the currently playing track."
  :group 'spofy-faces)

(defface spofy-playing-icon
  '((t :inherit spofy-playing))
  "Face for the play icon on the currently playing track."
  :group 'spofy-faces)

(defface spofy-header
  '((t :inherit header-line))
  "Face for header areas in detail views."
  :group 'spofy-faces)

(defface spofy-muted
  '((t :inherit shadow))
  "Face for secondary text such as duration and dates."
  :group 'spofy-faces)

;;;; Utilities

(defun spofy-ui-format-duration-ms (ms)
  "Format MS (milliseconds) as a \"M:SS\" string."
  (let* ((total-seconds (/ ms 1000))
         (minutes (/ total-seconds 60))
         (seconds (% total-seconds 60)))
    (format "%d:%02d" minutes seconds)))

(defun spofy-ui-format-artists (artists)
  "Format ARTISTS (a list of alists with a `name' key) as a comma-separated string."
  (mapconcat (lambda (a) (alist-get 'name a)) artists ", "))

(defun spofy-ui-truncate (string max-width)
  "Truncate STRING to MAX-WIDTH display columns, appending \"...\" if needed."
  (if (> (string-width string) max-width)
      (truncate-string-to-width string max-width nil nil "...")
    string))

(defun spofy-ui-propertize-playing (string)
  "Propertize STRING with the `spofy-playing' face."
  (propertize string 'face 'spofy-playing))

(defvar-local spofy-ui--next-page-url nil
  "URL for the next page of results in this buffer.")

(defvar-local spofy-ui--entity-type nil
  "The type of Spotify entity displayed in this buffer (track, album, etc.).")

(defvar-local spofy-ui--buffer-context nil
  "Alist of context data for the current Spofy buffer.")

(defun spofy-ui-insert-header (lines)
  "Insert header LINES at the top of the current buffer.
LINES is a list of strings, each displayed on its own line with the
`spofy-header' face."
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (dolist (line lines)
      (insert (propertize line 'face 'spofy-header) "\n"))
    (insert "\n")))

(provide 'spofy-ui)
;;; spofy-ui.el ends here
