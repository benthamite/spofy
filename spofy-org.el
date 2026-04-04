;;; spofy-org.el --- Org-mode link support for Spofy  -*- lexical-binding: t; -*-

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

;; Org-mode link type for Spotify URIs.  Registers a `spotify:' link type
;; that supports following (opening/playing), storing from Spofy buffers,
;; and exporting to HTML with links to open.spotify.com.

;;; Code:

(require 'org)
(require 'ol)
(require 'spofy-ui)

(declare-function spofy-play-track "spofy-player" (uri &optional context-uri))
(declare-function spofy-view-album "spofy-browse" (album-id))
(declare-function spofy-view-artist "spofy-browse" (artist-id))
(declare-function spofy-view-playlist "spofy-browse" (playlist-id))

;;;; URI parsing

(defun spofy-org--parse-uri (path)
  "Parse a Spotify URI PATH into (TYPE . ID).
PATH is the part after \"spotify:\", e.g. \"track:abc123\".
Returns a cons cell like (\"track\" . \"abc123\"), or nil if
the format is not recognized."
  (when (string-match "\\`\\(track\\|album\\|artist\\|playlist\\):\\(.+\\)\\'" path)
    (cons (match-string 1 path) (match-string 2 path))))

;;;; Follow handler

(defun spofy-org-follow (path _arg)
  "Follow a Spotify link.
PATH is the part after \"spotify:\", e.g. \"track:abc123\"."
  (if-let* ((parsed (spofy-org--parse-uri path))
            (type (car parsed))
            (id (cdr parsed)))
      (pcase type
        ("track"    (spofy-play-track (format "spotify:track:%s" id)))
        ("album"    (spofy-view-album id))
        ("artist"   (spofy-view-artist id))
        ("playlist" (spofy-view-playlist id)))
    (message "Spofy: unrecognized Spotify URI: spotify:%s" path)))

;;;; Store handler

(defun spofy-org--entity-at-point ()
  "Return the Spotify entity at point in any Spofy buffer."
  (spofy-ui-entity-at-point))

(defun spofy-org--entity-type-from-uri (uri)
  "Extract the entity type from a Spotify URI.
For example, \"spotify:track:1234\" returns \"track\"."
  (when (string-match "spotify:\\([^:]+\\):" uri)
    (match-string 1 uri)))

(defun spofy-org-store ()
  "Store an org link to the Spotify entity at point.
Works in any Spofy buffer (browse, search, library, playlist).
Returns non-nil if a link was stored, nil otherwise."
  (when-let* ((entity (spofy-org--entity-at-point)))
    ;; The entity may be a playlist track wrapper with a `track' key.
    (let* ((inner (or (alist-get 'track entity) entity))
           (uri (alist-get 'uri inner))
           (name (alist-get 'name inner)))
      (when (and uri name)
        (let* ((type (spofy-org--entity-type-from-uri uri))
               (description
                (pcase type
                  ("track"
                   (let ((artists (alist-get 'artists inner)))
                     (if (and artists (> (length artists) 0))
                         (format "%s — %s"
                                 name
                                 (mapconcat
                                  (lambda (a) (alist-get 'name a))
                                  artists ", "))
                       name)))
                  (_ name))))
          (org-link-store-props
           :type "spotify"
           :link (format "spotify:%s"
                         (string-remove-prefix "spotify:" uri))
           :description description)
          t)))))

;;;; Export handler

(defun spofy-org-export (path description backend _info)
  "Export a Spotify link.
PATH is the part after \"spotify:\", e.g. \"track:abc123\".
DESCRIPTION is the link description.
BACKEND is the export backend."
  (let* ((parsed (spofy-org--parse-uri path))
         (type (car parsed))
         (id (cdr parsed))
         (desc (or description (format "spotify:%s" path)))
         (url (when (and type id)
                (format "https://open.spotify.com/%s/%s" type id))))
    (pcase backend
      ('html
       (if url
           (format "<a href=\"%s\">%s</a>" url desc)
         desc))
      ('latex
       (if url
           (format "\\href{%s}{%s}" url desc)
         desc))
      ('md
       (if url
           (format "[%s](%s)" desc url)
         desc))
      (_
       desc))))

;;;; Registration

(org-link-set-parameters "spotify"
                         :follow #'spofy-org-follow
                         :store  #'spofy-org-store
                         :export #'spofy-org-export)

(provide 'spofy-org)
;;; spofy-org.el ends here
