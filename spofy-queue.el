;;; spofy-queue.el --- Queue management for spofy  -*- lexical-binding: t; -*-

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

;; Spotify playback queue primitives.  Provides `spofy-add-to-queue'
;; for enqueueing a track or podcast episode and the
;; `spofy-queue--queueable-uri-p' predicate used to validate URIs
;; before calling the Spotify API.  The unified timeline view lives in
;; `spofy-timeline'.

;;; Code:

(require 'spofy-api)
(require 'url-util)

;;;###autoload
(defun spofy-add-to-queue (uri)
  "Add URI to the Spotify playback queue.
URI must be a Spotify track or episode URI.  When called
interactively, uses the URI at point or prompts for one."
  (interactive
   (list (or (and (derived-mode-p 'tabulated-list-mode)
                  (tabulated-list-get-id))
             (get-text-property (point) 'spofy-uri)
             (read-string "Spotify URI: "))))
  (unless (spofy-queue--queueable-uri-p uri)
    (user-error "spofy: %s is not a track or episode URI" uri))
  (spofy-api-post
   (format "me/player/queue?uri=%s" (url-hexify-string uri))
   nil
   (lambda (_) (message "spofy: added to queue"))))

(defun spofy-queue--queueable-uri-p (uri)
  "Return non-nil if URI is a Spotify track or episode URI."
  (and (stringp uri)
       (string-match-p "\\`spotify:\\(track\\|episode\\):" uri)))

(provide 'spofy-queue)
;;; spofy-queue.el ends here
