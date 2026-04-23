;;; spofy.el --- Spotify player for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;; Author: Pablo Stafforini
;; URL: https://github.com/pablostafforini/spofy
;; Version: 0.3.0
;; Package-Requires: ((emacs "30.1") (transient "0.7"))
;; Keywords: multimedia

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

;; Spofy is a full-featured Spotify client for Emacs, providing playback
;; control, search, browsing, library management, and playlist management
;; via the Spotify Web API.
;;
;; Getting started:
;; 1. Register a Spotify app at <https://developer.spotify.com/dashboard>.
;; 2. Set `spofy-client-id' and `spofy-client-secret'.
;; 3. Run `spofy-authenticate' to log in.
;; 4. Run `spofy' to open the dashboard.
;;
;; Enable `spofy-global-mode' for a global keybinding (default C-c s) that
;; opens the Spofy transient popup.

;;; Code:

(require 'cl-lib)
(require 'transient)
(require 'spofy-ui)

;;;; Declarations for functions from other Spofy modules

;; spofy-auth
(declare-function spofy-auth-access-token "spofy-auth" ())
(declare-function spofy-auth--load-tokens "spofy-auth" ())
(declare-function spofy-auth--token-expired-p "spofy-auth" ())
(declare-function spofy-authenticate "spofy-auth" ())

;; spofy-api
(declare-function spofy-api-get "spofy-api" (endpoint &optional params callback))

;; spofy-player
(declare-function spofy-player-start-polling "spofy-player" ())
(declare-function spofy-player-stop-polling "spofy-player" ())
(declare-function spofy-player-current-track "spofy-player" ())
(declare-function spofy-player-playing-p "spofy-player" ())
(declare-function spofy-player-interpolated-progress "spofy-player" ())
(declare-function spofy-play-pause "spofy-player" ())
(declare-function spofy-next "spofy-player" ())
(declare-function spofy-previous "spofy-player" ())
(declare-function spofy-seek-forward "spofy-player" ())
(declare-function spofy-seek-backward "spofy-player" ())
(declare-function spofy-volume-up "spofy-player" ())
(declare-function spofy-volume-down "spofy-player" ())
(declare-function spofy-volume-set "spofy-player" (volume))
(declare-function spofy-toggle-shuffle "spofy-player" ())
(declare-function spofy-toggle-repeat "spofy-player" ())
(declare-function spofy-play-track "spofy-player" (uri &optional context-uri))
(declare-function spofy-play-context "spofy-player" (context-uri))
(declare-function spofy-select-device "spofy-player" ())
(declare-function spofy-player--ensure-device "spofy-player" ())
(declare-function spofy-player--poll-sync "spofy-player" ())
(declare-function spofy-seek-to "spofy-player" ())
(declare-function spofy-jump-to-playing-track "spofy-player" ())

;; spofy-search
(declare-function spofy-search-tracks "spofy-search" (query))
(declare-function spofy-search-albums "spofy-search" (query))
(declare-function spofy-search-artists "spofy-search" (query))
(declare-function spofy-search-playlists "spofy-search" (query))

;; spofy-browse
(declare-function spofy-view-playlist "spofy-browse" (playlist-id))
(declare-function spofy-view-album "spofy-browse" (album-id))
(declare-function spofy-view-artist "spofy-browse" (artist-id))

;; spofy-playlist
(declare-function spofy-library-browse-playlists "spofy-playlist" ())

;; spofy-library
(declare-function spofy-library-browse-tracks "spofy-library" ())
(declare-function spofy-library-browse-albums "spofy-library" ())
(declare-function spofy-list-top-tracks "spofy-library" (&optional time-range))
(declare-function spofy-list-top-artists "spofy-library" (&optional time-range))
(declare-function spofy-list-new-releases "spofy-library" ())
(declare-function spofy-library-warm-cache "spofy-library" ())
(declare-function spofy-save-current-track "spofy-library" ())
(declare-function spofy-unsave-current-track "spofy-library" ())

;; spofy-queue
(declare-function spofy-add-to-queue "spofy-queue" (uri))

;; spofy-timeline
(declare-function spofy-view-timeline "spofy-timeline" ())

;; spofy-mode-line
(declare-function spofy-mode-line-mode "spofy-mode-line" (&optional arg))

;; spofy-tab-bar
(declare-function spofy-tab-bar-mode "spofy-tab-bar" (&optional arg))

;; spofy-ui
(declare-function spofy-cursor-follows-playback-mode "spofy-ui" (&optional arg))
(declare-function spofy-copy-url "spofy-ui" ())

;; spofy-wikipedia
(declare-function spofy-wikipedia "spofy-wikipedia" ())

;;;; Variables from other modules (for byte-compiler)

(defvar spofy-auth--access-token)
(defvar spofy-player--current-state)
(defvar spofy-player-state-changed-hook)
(defvar spofy-player-track-changed-hook)
(defvar spofy-player--timer)
(defvar spofy-poll-interval)
(defvar spofy-global-mode)

;;;; Customization

(defcustom spofy-global-key "C-c s"
  "Prefix key for `spofy-global-mode'.
When `spofy-global-mode' is enabled, this key opens `spofy-menu'."
  :type 'string
  :group 'spofy)

(defcustom spofy-enable-mode-line t
  "Whether to enable `spofy-mode-line-mode' when `spofy-global-mode' is on."
  :type 'boolean
  :group 'spofy)

(defcustom spofy-enable-tab-bar nil
  "Whether to enable `spofy-tab-bar-mode' when `spofy-global-mode' is on."
  :type 'boolean
  :group 'spofy)

(defcustom spofy-dashboard-album-art t
  "Whether to display album art in the now-playing section.
Requires a graphical Emacs frame with image support."
  :type 'boolean
  :group 'spofy)

(defcustom spofy-dashboard-sections
  '(recently-played playlists)
  "Sections to display on the dashboard, in order.
Each element is a symbol naming a section.  Available sections:

  `recently-played'    Last 10 tracks played.
  `playlists'          First 10 user playlists.
  `top-tracks-short'   Top tracks from the last 4 weeks.
  `top-tracks-medium'  Top tracks from the last 6 months.
  `top-tracks-long'    Top tracks from the last year.
  `top-artists-short'  Top artists from the last 4 weeks.
  `top-artists-medium' Top artists from the last 6 months.
  `top-artists-long'   Top artists from the last year.
  `new-releases'       New album releases."
  :type '(repeat (choice (const :tag "Recently played" recently-played)
                         (const :tag "Your playlists" playlists)
                         (const :tag "Top tracks (4 weeks)" top-tracks-short)
                         (const :tag "Top tracks (6 months)" top-tracks-medium)
                         (const :tag "Top tracks (1 year)" top-tracks-long)
                         (const :tag "Top artists (4 weeks)" top-artists-short)
                         (const :tag "Top artists (6 months)" top-artists-medium)
                         (const :tag "Top artists (1 year)" top-artists-long)
                         (const :tag "New releases" new-releases)))
  :group 'spofy)

;;;; Dashboard buffer

(defvar spofy-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'spofy-play-pause)
    (define-key map (kbd "n")   #'spofy-next)
    (define-key map (kbd "p")   #'spofy-previous)
    (define-key map (kbd "/")   #'spofy-menu)
    (define-key map (kbd "g")   #'spofy-dashboard-refresh)
    (define-key map (kbd ".")   #'spofy-jump-to-playing-track)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `spofy-dashboard-mode'.")

;; Install remaps outside the `defvar', which skips reinitialization on
;; reload, so the bindings reach a running Emacs when the file changes.
(define-key spofy-dashboard-mode-map [remap next-line]
            #'spofy-dashboard-next-line)
(define-key spofy-dashboard-mode-map [remap previous-line]
            #'spofy-dashboard-previous-line)

(define-derived-mode spofy-dashboard-mode special-mode "Spofy"
  "Major mode for the Spofy dashboard buffer."
  :group 'spofy
  (setq-local mode-line-format spofy-ui-mode-line-format)
  (hl-line-mode 1))

(defun spofy-dashboard-next-line (&optional n)
  "Move to the Nth next actionable line in the dashboard.
An actionable line is one that contains at least one clickable
button — a track, artist, album, playlist, or section link.
N defaults to 1; negative N moves backwards.  Bound to remap
`next-line' so C-n, <down>, and any other binding that normally
invokes `next-line' skip straight from one clickable line to
the next, past the album art, progress bar, shuffle/repeat line,
section headers, and blank spacer lines."
  (interactive "^p")
  (spofy-ui-move-by-matching-lines #'spofy-dashboard--actionable-line-p
                                   (or n 1)))

(defun spofy-dashboard-previous-line (&optional n)
  "Move to the Nth previous actionable line in the dashboard.
N defaults to 1; negative N moves forwards."
  (interactive "^p")
  (spofy-ui-move-by-matching-lines #'spofy-dashboard--actionable-line-p
                                   (- (or n 1))))

(defun spofy-dashboard--actionable-line-p ()
  "Return non-nil when the current line contains at least one button."
  (text-property-not-all (line-beginning-position)
                         (line-end-position)
                         'button nil))

;;;;; Dashboard: album art

(defvar spofy--dashboard-recently-played-timer nil
  "Timer for delayed recently-played section refresh.")

(defvar spofy--album-art-cache-dir nil
  "Directory for cached album art images.")

(defvar spofy--album-art-current-album-id nil
  "Album ID whose art is currently loaded or being fetched.")

(defvar spofy--album-art-image nil
  "Emacs image object for the current album art, or nil if unavailable.")

(defun spofy--album-art-cache-dir ()
  "Return the album art cache directory, creating it if needed."
  (unless spofy--album-art-cache-dir
    (setq spofy--album-art-cache-dir
          (expand-file-name "spofy/album-art/"
                            (or (getenv "XDG_CACHE_HOME")
                                (expand-file-name "~/.cache")))))
  (unless (file-directory-p spofy--album-art-cache-dir)
    (make-directory spofy--album-art-cache-dir t))
  spofy--album-art-cache-dir)

(defun spofy--album-art-cache-file (album-id)
  "Return the cache file path for ALBUM-ID."
  (expand-file-name (concat album-id ".dat")
                    (spofy--album-art-cache-dir)))

(defun spofy--album-art-load (file)
  "Load album art from FILE.
The next 1-second progress timer tick will pick up the new image."
  (setq spofy--album-art-image
        (create-image file nil nil
                      :max-height 300
                      :max-width 300
                      :ascent 'center)))

(defun spofy--album-art-download (url file)
  "Download album art from URL to FILE, then load it."
  (url-retrieve
   url
   (lambda (status)
     (unwind-protect
         (unless (plist-get status :error)
           (goto-char (point-min))
           (when (re-search-forward "\r?\n\r?\n" nil t)
             (let ((coding-system-for-write 'binary)
                   (data (buffer-substring-no-properties (point) (point-max))))
               (with-temp-file file
                 (set-buffer-multibyte nil)
                 (insert data)))))
       (kill-buffer))
     (when (file-exists-p file)
       (spofy--album-art-load file)))
   nil t))

(defun spofy--album-art-update (album-id image-url)
  "Fetch album art for ALBUM-ID from IMAGE-URL if it changed."
  (when (and spofy-dashboard-album-art
             (display-graphic-p)
             album-id image-url
             (not (equal album-id spofy--album-art-current-album-id)))
    (setq spofy--album-art-current-album-id album-id)
    (setq spofy--album-art-image nil)
    (let ((cache-file (spofy--album-art-cache-file album-id)))
      (if (file-exists-p cache-file)
          (spofy--album-art-load cache-file)
        (spofy--album-art-download image-url cache-file)))))

;;;;; Dashboard: now-playing section

(defun spofy--dashboard-insert-now-playing ()
  "Insert the now-playing section into the current buffer."
  (let* ((state (and (boundp 'spofy-player--current-state)
                     spofy-player--current-state))
         (track (and state (alist-get 'track state)))
         (track-id (and state (alist-get 'track-id state)))
         (artist (and state (alist-get 'artist state)))
         (artists (and state (alist-get 'artists state)))
         (album (and state (alist-get 'album state)))
         (album-id (and state (alist-get 'album-id state)))
         (album-date (and state (alist-get 'album-date state)))
         (progress (and state (spofy-player-interpolated-progress)))
         (duration (and state (alist-get 'duration state)))
         (shuffle (and state (alist-get 'shuffle state)))
         (repeat-state (and state (alist-get 'repeat state))))
    (insert (propertize "Now playing" 'face 'spofy-header) "\n\n")
    (if (not track)
        (progn
          (setq spofy--album-art-current-album-id nil
                spofy--album-art-image nil)
          (insert (propertize "  No track playing" 'face 'spofy-muted) "\n"))
      (insert "  ")
      (spofy--dashboard-insert-now-playing-track track track-id)
      (insert "\n  ")
      (spofy--dashboard-insert-now-playing-artists artists artist)
      (insert "\n  ")
      (spofy--dashboard-insert-now-playing-album album album-id album-date)
      (insert "\n")
      ;; Album art
      (let ((art-char-width nil))
        (when (and spofy-dashboard-album-art (display-graphic-p))
          (spofy--album-art-update (alist-get 'album-id state)
                                   (alist-get 'album-image-url state))
          (when (and spofy--album-art-image
                     (equal (alist-get 'album-id state)
                            spofy--album-art-current-album-id))
            (insert "  ")
            (insert-image spofy--album-art-image "[album art]")
            (insert "\n")
            (setq art-char-width (car (image-size spofy--album-art-image)))))
        (let ((bar-width (if art-char-width
                            (max 10 (truncate art-char-width))
                          20)))
          (let* ((time-str (spofy-ui-progress-time progress (or duration 0)))
                 (left (format "  shuffle: %s  repeat: %s"
                               (if shuffle "on" "off")
                               (or repeat-state "off")))
                 (right-col (+ 2 bar-width))
                 (gap (max 2 (- right-col (length left) (length time-str)))))
            (insert "  "
                    (spofy-ui-progress-bar-only progress (or duration 0) bar-width)
                    "\n"
                    left
                    (make-string gap ?\s)
                    time-str
                    "\n")))))))

(defun spofy--dashboard-insert-now-playing-track (track track-id)
  "Insert the TRACK title as a clickable link when TRACK-ID is non-nil.
Clicking invokes `spofy-jump-to-playing-track' to reveal the track in its
playing context."
  (let ((text (spofy--dashboard-truncate
               (propertize track 'face 'spofy-now-playing-track))))
    (if track-id
        (insert-text-button text
                            'action (lambda (_btn)
                                      (require 'spofy-player)
                                      (spofy-jump-to-playing-track))
                            'follow-link t)
      (insert text))))

(defun spofy--dashboard-insert-now-playing-artists (artists fallback)
  "Insert ARTISTS as clickable links, or FALLBACK when unavailable.
ARTISTS is a list of (NAME . ID) pairs.  Each name links to
`spofy-view-artist' for its ID.  FALLBACK is the preformatted artist
string used when ARTISTS is nil or any entry lacks an ID."
  (if (and artists (seq-every-p #'cdr artists))
      (spofy--dashboard-insert-clickable-artists artists)
    (insert (spofy--dashboard-truncate
             (propertize (or fallback "") 'face 'spofy-now-playing-artist)))))

(defun spofy--dashboard-insert-clickable-artists (artists)
  "Insert ARTISTS as a comma-separated list of clickable links.
ARTISTS is a list of (NAME . ID) pairs, each non-nil."
  (let ((separator (propertize ", " 'face 'spofy-now-playing-artist))
        (first t))
    (dolist (artist artists)
      (unless first (insert separator))
      (setq first nil)
      (let ((name (car artist))
            (id (cdr artist)))
        (insert-text-button
         (propertize name 'face 'spofy-now-playing-artist)
         'action (lambda (_btn)
                   (require 'spofy-browse)
                   (spofy-view-artist id))
         'follow-link t)))))

(defun spofy--dashboard-insert-now-playing-album (album album-id album-date)
  "Insert the ALBUM title as a clickable link when ALBUM-ID is non-nil.
ALBUM-DATE, if non-nil, contributes a parenthesized year suffix.  Clicking
invokes `spofy-view-album' for ALBUM-ID."
  (let* ((year (and album-date
                    (substring album-date 0 (min 4 (length album-date)))))
         (album-str (if year
                        (format "%s (%s)" (or album "") year)
                      (or album "")))
         (text (spofy--dashboard-truncate
                (propertize album-str 'face 'spofy-now-playing-album))))
    (if album-id
        (insert-text-button text
                            'action (lambda (_btn)
                                      (require 'spofy-browse)
                                      (spofy-view-album album-id))
                            'follow-link t)
      (insert text))))

;;;;; Dashboard: text truncation

(defun spofy--dashboard-truncate (text)
  "Truncate TEXT to fit within the window, accounting for indent.
When the text face has a `:height' scale (e.g. 1.15), reduce
the available width proportionally so the text does not overflow.
When truncated, mark the last three characters with the
`spofy-fade' text property for gradient fade-out rendering."
  (let* ((scale (spofy--dashboard-face-scale
                 (get-text-property 0 'face text)))
         (max-width (max 10 (floor (/ (- (window-width) 4) scale)))))
    (if (<= (string-width text) max-width)
        text
      (let ((result (truncate-string-to-width text max-width)))
        (let ((len (length result)))
          (when (>= len 3)
            (dotimes (i 3)
              (put-text-property (+ (- len 3) i) (+ (- len 3) i 1)
                                 'spofy-fade (1+ i) result))))
        result))))

(defun spofy--dashboard-face-scale (face)
  "Return the height scale factor for FACE, defaulting to 1.0.
A float `:height' attribute (e.g. 1.15) indicates a scale
relative to the default font; an absolute or unspecified value
maps to 1.0."
  (if (and face (facep face))
      (let ((h (face-attribute face :height)))
        (if (floatp h) h 1.0))
    1.0))

;;;;; Dashboard: recently played section

(defun spofy--dashboard-insert-recently-played (items)
  "Insert the recently-played section from ITEMS into the current buffer."
  (insert "\n" (propertize "Recently played" 'face 'spofy-header) "\n\n")
  (if (or (null items) (= (length items) 0))
      (insert (propertize "  No recent tracks" 'face 'spofy-muted) "\n")
    (seq-doseq (item items)
      (let* ((track-obj (alist-get 'track item))
             (name (and track-obj (or (alist-get 'name track-obj) "")))
             (artists (and track-obj (alist-get 'artists track-obj)))
             (artist-str (if artists (spofy-ui-format-artists artists) ""))
             (uri (and track-obj (alist-get 'uri track-obj))))
        (when name
          (insert "  ")
          (insert-text-button
           (spofy--dashboard-truncate
            (concat (propertize (concat name " ") 'face 'spofy-track-name)
                    (propertize artist-str 'face 'spofy-artist-name)))
           'action (lambda (_btn)
                     (require 'spofy-player)
                     (spofy-play-track uri))
           'spofy-uri uri
           'follow-link t)
          (insert "\n")))))
  (insert "\n  ")
  (insert-text-button
   (propertize "Open timeline" 'face 'spofy-muted)
   'action (lambda (_btn)
             (require 'spofy-timeline)
             (spofy-view-timeline))
   'follow-link t)
  (insert "\n"))

;;;;; Dashboard: playlists section

(defun spofy--dashboard-insert-playlists (items)
  "Insert the playlists section from ITEMS into the current buffer."
  (insert "\n" (propertize "Your playlists" 'face 'spofy-header) "\n\n")
  (if (or (null items) (= (length items) 0))
      (insert (propertize "  No playlists" 'face 'spofy-muted) "\n")
    (seq-doseq (item items)
      (let* ((name (or (alist-get 'name item) ""))
             (tracks-info (alist-get 'tracks item))
             (total (or (and tracks-info (alist-get 'total tracks-info)) 0))
             (playlist-id (alist-get 'id item)))
        (insert "  ")
        (insert-text-button
         (spofy--dashboard-truncate
          (concat (propertize (concat name " ") 'face 'spofy-track-name)
                  (propertize (format "(%d tracks)" total) 'face 'spofy-muted)))
         'action (lambda (_btn)
                   (require 'spofy-browse)
                   (spofy-view-playlist playlist-id))
         'spofy-playlist-id playlist-id
         'follow-link t)
        (insert "\n"))))
  (insert "\n  ")
  (insert-text-button
   (propertize "View all playlists" 'face 'spofy-muted)
   'action (lambda (_btn)
             (require 'spofy-playlist)
             (spofy-library-browse-playlists))
   'follow-link t)
  (insert "\n"))

;;;;; Dashboard: top tracks section

(defun spofy--dashboard-insert-top-tracks (items time-range)
  "Insert a top-tracks section from ITEMS into the current buffer.
TIME-RANGE is \"short_term\", \"medium_term\", or \"long_term\"."
  (let ((label (pcase time-range
                 ("short_term" "Top tracks (4 weeks)")
                 ("medium_term" "Top tracks (6 months)")
                 ("long_term" "Top tracks (1 year)")
                 (_ (format "Top tracks (%s)" time-range)))))
    (insert "\n" (propertize label 'face 'spofy-header) "\n\n")
    (if (or (null items) (= (length items) 0))
        (insert (propertize "  No tracks" 'face 'spofy-muted) "\n")
      (seq-doseq (item items)
        (let* ((name (or (alist-get 'name item) ""))
               (artists (alist-get 'artists item))
               (artist-str (if artists (spofy-ui-format-artists artists) ""))
               (uri (alist-get 'uri item)))
          (insert "  ")
          (insert-text-button
           (spofy--dashboard-truncate
            (concat (propertize (concat name " ") 'face 'spofy-track-name)
                    (propertize artist-str 'face 'spofy-artist-name)))
           'action (lambda (_btn)
                     (require 'spofy-player)
                     (spofy-play-track uri))
           'spofy-uri uri
           'follow-link t)
          (insert "\n"))))
    (insert "\n  ")
    (insert-text-button
     (propertize "View all" 'face 'spofy-muted)
     'action (let ((tr time-range))
               (lambda (_btn)
                 (require 'spofy-library)
                 (spofy-list-top-tracks tr)))
     'follow-link t)
    (insert "\n")))

;;;;; Dashboard: top artists section

(defun spofy--dashboard-insert-top-artists (items time-range)
  "Insert a top-artists section from ITEMS into the current buffer.
TIME-RANGE is \"short_term\", \"medium_term\", or \"long_term\"."
  (let ((label (pcase time-range
                 ("short_term" "Top artists (4 weeks)")
                 ("medium_term" "Top artists (6 months)")
                 ("long_term" "Top artists (1 year)")
                 (_ (format "Top artists (%s)" time-range)))))
    (insert "\n" (propertize label 'face 'spofy-header) "\n\n")
    (if (or (null items) (= (length items) 0))
        (insert (propertize "  No artists" 'face 'spofy-muted) "\n")
      (seq-doseq (item items)
        (let* ((name (or (alist-get 'name item) ""))
               (genres (alist-get 'genres item))
               (genre-str (if (and genres (> (length genres) 0))
                              (mapconcat #'identity
                                         (seq-take (append genres nil) 3)
                                         ", ")
                            ""))
               (id (alist-get 'id item)))
          (insert "  ")
          (insert-text-button
           (spofy--dashboard-truncate
            (concat (propertize name 'face 'spofy-track-name)
                    (unless (string-empty-p genre-str)
                      (propertize (concat " " genre-str) 'face 'spofy-muted))))
           'action (lambda (_btn)
                     (require 'spofy-browse)
                     (spofy-view-artist id))
           'follow-link t)
          (insert "\n"))))
    (insert "\n  ")
    (insert-text-button
     (propertize "View all" 'face 'spofy-muted)
     'action (let ((tr time-range))
               (lambda (_btn)
                 (require 'spofy-library)
                 (spofy-list-top-artists tr)))
     'follow-link t)
    (insert "\n")))

;;;;; Dashboard: new releases section

(defun spofy--dashboard-insert-new-releases (items)
  "Insert the new-releases section from ITEMS into the current buffer."
  (insert "\n" (propertize "New releases" 'face 'spofy-header) "\n\n")
  (if (or (null items) (= (length items) 0))
      (insert (propertize "  No releases" 'face 'spofy-muted) "\n")
    (seq-doseq (item items)
      (let* ((name (or (alist-get 'name item) ""))
             (artists (alist-get 'artists item))
             (artist-str (if artists (spofy-ui-format-artists artists) ""))
             (album-id (alist-get 'id item)))
        (insert "  ")
        (insert-text-button
         (spofy--dashboard-truncate
          (concat (propertize (concat name " ") 'face 'spofy-album-name)
                  (propertize artist-str 'face 'spofy-artist-name)))
         'action (lambda (_btn)
                   (require 'spofy-browse)
                   (spofy-view-album album-id))
         'follow-link t)
        (insert "\n"))))
  (insert "\n  ")
  (insert-text-button
   (propertize "View all" 'face 'spofy-muted)
   'action (lambda (_btn)
             (require 'spofy-library)
             (spofy-list-new-releases))
   'follow-link t)
  (insert "\n"))

;;;;; Dashboard: section registry

(defun spofy--dashboard-section-spec (section)
  "Return (ENDPOINT PARAMS INSERTER) for SECTION symbol."
  (pcase section
    ('recently-played
     (list "me/player/recently-played" '(("limit" . "10"))
           #'spofy--dashboard-insert-recently-played))
    ('playlists
     (list "me/playlists" '(("limit" . "10"))
           #'spofy--dashboard-insert-playlists))
    ('top-tracks-short
     (list "me/top/tracks" '(("limit" . "10") ("time_range" . "short_term"))
           (lambda (items) (spofy--dashboard-insert-top-tracks items "short_term"))))
    ('top-tracks-medium
     (list "me/top/tracks" '(("limit" . "10") ("time_range" . "medium_term"))
           (lambda (items) (spofy--dashboard-insert-top-tracks items "medium_term"))))
    ('top-tracks-long
     (list "me/top/tracks" '(("limit" . "10") ("time_range" . "long_term"))
           (lambda (items) (spofy--dashboard-insert-top-tracks items "long_term"))))
    ('top-artists-short
     (list "me/top/artists" '(("limit" . "10") ("time_range" . "short_term"))
           (lambda (items) (spofy--dashboard-insert-top-artists items "short_term"))))
    ('top-artists-medium
     (list "me/top/artists" '(("limit" . "10") ("time_range" . "medium_term"))
           (lambda (items) (spofy--dashboard-insert-top-artists items "medium_term"))))
    ('top-artists-long
     (list "me/top/artists" '(("limit" . "10") ("time_range" . "long_term"))
           (lambda (items) (spofy--dashboard-insert-top-artists items "long_term"))))
    ('new-releases
     (list "browse/new-releases" '(("limit" . "10"))
           #'spofy--dashboard-insert-new-releases))))

(defun spofy--dashboard-section-extract-items (section response)
  "Extract the items array from RESPONSE for SECTION."
  (pcase section
    ('playlists (alist-get 'items response))
    ('new-releases (let ((albums (alist-get 'albums response)))
                     (and albums (alist-get 'items albums))))
    (_ (alist-get 'items response))))

(defun spofy--dashboard-section-loading-text (section)
  "Return the loading placeholder text for SECTION."
  (pcase section
    ('recently-played "Loading recently played...")
    ('playlists "Loading playlists...")
    ('top-tracks-short "Loading top tracks (4 weeks)...")
    ('top-tracks-medium "Loading top tracks (6 months)...")
    ('top-tracks-long "Loading top tracks (1 year)...")
    ('top-artists-short "Loading top artists (4 weeks)...")
    ('top-artists-medium "Loading top artists (6 months)...")
    ('top-artists-long "Loading top artists (1 year)...")
    ('new-releases "Loading new releases...")))

;;;;; Dashboard: now-playing auto-refresh

(defvar spofy--dashboard-now-playing-marker nil
  "Marker for the start of the now-playing section in the dashboard.")

(defvar spofy--dashboard-now-playing-end-marker nil
  "Marker for the end of the now-playing section in the dashboard.")

(defvar spofy--dashboard-section-markers nil
  "Alist mapping section index to start marker.
The last entry is a sentinel marking the end of the last section.")

(defvar spofy--dashboard-progress-timer nil
  "Timer for updating the progress bar every second.")

(defun spofy--dashboard-refresh-now-playing ()
  "Refresh only the now-playing section in the *Spofy* buffer.
Called from `spofy-player-state-changed-hook' and the progress timer."
  (when-let* ((buf (get-buffer "*Spofy*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (and (markerp spofy--dashboard-now-playing-marker)
                   (markerp spofy--dashboard-now-playing-end-marker)
                   (marker-position spofy--dashboard-now-playing-marker)
                   (marker-position spofy--dashboard-now-playing-end-marker))
          (let ((inhibit-read-only t)
                (pos (point)))
            (delete-region spofy--dashboard-now-playing-marker
                           spofy--dashboard-now-playing-end-marker)
            (goto-char spofy--dashboard-now-playing-marker)
            (spofy--dashboard-insert-now-playing)
            (let ((end (point)))
              (set-marker spofy--dashboard-now-playing-end-marker end)
              ;; Keep first section marker in sync after re-render
              (when-let* ((first (alist-get 0 spofy--dashboard-section-markers)))
                (set-marker first end)))
            (spofy-ui--apply-buffer-fades)
            (goto-char (min pos (point-max)))
            (when-let* ((win (get-buffer-window buf)))
              (force-window-update win))))))))

(defun spofy--dashboard-start-progress-timer ()
  "Start a 1-second timer to refresh the progress bar."
  (spofy--dashboard-stop-progress-timer)
  (setq spofy--dashboard-progress-timer
        (run-with-timer 1 1 #'spofy--dashboard-refresh-now-playing)))

(defun spofy--dashboard-stop-progress-timer ()
  "Stop the progress bar refresh timer."
  (when spofy--dashboard-progress-timer
    (cancel-timer spofy--dashboard-progress-timer)
    (setq spofy--dashboard-progress-timer nil)))

;;;;; Dashboard: section refresh

(defun spofy--dashboard-refresh-section (section)
  "Re-fetch and re-render a single dashboard SECTION by name."
  (when-let* ((buf (get-buffer "*Spofy*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when-let* ((idx (cl-position section spofy-dashboard-sections))
                    (spec (spofy--dashboard-section-spec section)))
          (let ((sec section)
                (i idx)
                (endpoint (nth 0 spec))
                (params (nth 1 spec))
                (inserter (nth 2 spec)))
            (spofy-api-get
             endpoint params
             (lambda (response)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (let* ((inhibit-read-only t)
                          (pos (point))
                          (items (spofy--dashboard-section-extract-items sec response))
                          (start-marker (alist-get i spofy--dashboard-section-markers))
                          (end-marker (alist-get (1+ i) spofy--dashboard-section-markers)))
                     (when (and start-marker end-marker
                                (marker-position start-marker)
                                (marker-position end-marker))
                       (delete-region start-marker end-marker)
                       (goto-char start-marker)
                       (funcall inserter items)
                       (set-marker end-marker (point))
                       (spofy-ui--apply-buffer-fades))
                     (goto-char (min pos (point-max))))))))))))))

(defun spofy--dashboard-refresh-recently-played ()
  "Refresh the recently-played dashboard section after a track change.
The refresh is delayed by `spofy-poll-interval' seconds because the
Spotify API takes time to update the recently-played endpoint after
a track finishes."
  (spofy--dashboard-cancel-recently-played-timer)
  (setq spofy--dashboard-recently-played-timer
        (run-with-timer spofy-poll-interval nil
                        #'spofy--dashboard-refresh-recently-played-now)))

(defun spofy--dashboard-refresh-recently-played-now ()
  "Perform the actual recently-played section refresh."
  (setq spofy--dashboard-recently-played-timer nil)
  (spofy--dashboard-refresh-section 'recently-played))

(defun spofy--dashboard-cancel-recently-played-timer ()
  "Cancel any pending recently-played refresh timer."
  (when (timerp spofy--dashboard-recently-played-timer)
    (cancel-timer spofy--dashboard-recently-played-timer)
    (setq spofy--dashboard-recently-played-timer nil)))

;;;;; Dashboard: full render

(defun spofy--dashboard-render ()
  "Render the full dashboard content in the current buffer.
Assumes the current buffer is in `spofy-dashboard-mode'."
  (let ((inhibit-read-only t)
        (sections spofy-dashboard-sections))
    (erase-buffer)
    ;; Now playing section (with markers for partial refresh)
    (setq-local spofy--dashboard-now-playing-marker (point-marker))
    (spofy--dashboard-insert-now-playing)
    (setq-local spofy--dashboard-now-playing-end-marker (point-marker))
    ;; Create markers for each async section + a sentinel
    (setq-local spofy--dashboard-section-markers nil)
    (let ((idx 0))
      (dolist (section sections)
        (let ((marker (point-marker)))
          (push (cons idx marker) spofy--dashboard-section-markers)
          (insert (propertize (concat "\n" (spofy--dashboard-section-loading-text section))
                              'face 'spofy-muted)
                  "\n")
          (setq idx (1+ idx))))
      ;; Sentinel marker for hints
      (push (cons idx (point-marker)) spofy--dashboard-section-markers))
    (setq spofy--dashboard-section-markers
          (nreverse spofy--dashboard-section-markers))
    (goto-char (point-min))
    ;; Fire async requests for each section
    (let ((buf (current-buffer)))
      (cl-loop
       for section in sections
       for idx from 0
       for spec = (spofy--dashboard-section-spec section)
       when spec do
       (let ((sec section)
             (i idx)
             (endpoint (nth 0 spec))
             (params (nth 1 spec))
             (inserter (nth 2 spec)))
         (spofy-api-get
          endpoint params
          (lambda (response)
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (let* ((inhibit-read-only t)
                       (pos (point))
                       (items (spofy--dashboard-section-extract-items sec response))
                       (start-marker (alist-get i spofy--dashboard-section-markers))
                       (end-marker (alist-get (1+ i) spofy--dashboard-section-markers)))
                  (when (and start-marker end-marker
                             (marker-position start-marker)
                             (marker-position end-marker))
                    (delete-region start-marker end-marker)
                    (goto-char start-marker)
                    (funcall inserter items)
                    (set-marker end-marker (point))
                    (spofy-ui--apply-buffer-fades))
                  (goto-char (min pos (point-max)))))))))))))


;;;;; Dashboard: interactive commands

;;;###autoload
(cl-defun spofy ()
  "Open the Spofy dashboard.
The main entry point for the Spofy Spotify client.  Ensures
authentication, starts polling, and displays the dashboard buffer."
  (interactive)
  ;; Ensure authenticated (with interactive prompt on first use)
  (require 'spofy-auth)
  (spofy-auth--load-tokens)
  (unless (spofy-auth-access-token)
    (if (y-or-n-p "Spofy: not authenticated.  Authenticate now? ")
        (progn
          (spofy-authenticate)
          (message "Spofy: complete authentication in your browser, then run `spofy' again.")
          (cl-return-from spofy))
      (user-error "Spofy: authentication required")))
  ;; Enable global mode (starts polling, mode-line, tab-bar)
  (spofy--ensure-global-mode)
  ;; Hook up now-playing refresh on state changes
  (add-hook 'spofy-player-state-changed-hook #'spofy--dashboard-refresh-now-playing)
  ;; Hook up recently-played refresh on track changes
  (add-hook 'spofy-player-track-changed-hook #'spofy--dashboard-refresh-recently-played)
  ;; Start progress interpolation timer
  (spofy--dashboard-start-progress-timer)
  ;; Create and display the dashboard
  (let ((buf (get-buffer-create "*Spofy*")))
    (with-current-buffer buf
      (spofy-dashboard-mode)
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (spofy--dashboard-stop-progress-timer)
                  (spofy--dashboard-cancel-recently-played-timer)
                  (remove-hook 'spofy-player-state-changed-hook
                               #'spofy--dashboard-refresh-now-playing)
                  (remove-hook 'spofy-player-track-changed-hook
                               #'spofy--dashboard-refresh-recently-played))
                nil t)
      (spofy--dashboard-render))
    (pop-to-buffer buf)))

(defun spofy-dashboard-refresh ()
  "Refresh the Spofy dashboard."
  (interactive)
  (require 'spofy-api)
  (require 'spofy-player)
  (when (derived-mode-p 'spofy-dashboard-mode)
    (spofy--dashboard-render)))

;;;; Initialization helper

(defun spofy--ensure-global-mode ()
  "Ensure `spofy-global-mode' is active.
Loads required modules, checks authentication, and enables the mode.
If no tokens are available, prompts the user to authenticate."
  (require 'spofy-auth)
  (require 'spofy-api)
  (require 'spofy-player)
  (spofy-auth--load-tokens)
  (unless (spofy-auth-access-token)
    (if (y-or-n-p "Spofy: not authenticated.  Authenticate now? ")
        (progn
          (spofy-authenticate)
          (user-error "Spofy: complete authentication in your browser, then try again"))
      (user-error "Spofy: authentication required")))
  (unless spofy-global-mode
    (spofy-global-mode 1))
  (add-hook 'spofy-player-track-changed-hook
            #'spofy-ui--refresh-track-highlights)
  (add-hook 'spofy-player-state-changed-hook
            #'spofy-ui--update-mode-lines)
  (require 'spofy-library)
  (spofy-library-warm-cache))

;;;; Transient popup

(defun spofy--transient-description ()
  "Return the description string for the transient popup header."
  (require 'spofy-player)
  (let ((track-info (spofy-player-current-track)))
    (if track-info
        (let ((icon (if (spofy-player-playing-p) "⏸" "▶")))
          (format "Spofy: %s %s — %s" icon (car track-info) (cdr track-info)))
      "Spofy: no track playing")))

(defun spofy--repeat-description ()
  "Return a description string for the current repeat state."
  (let ((state (or (alist-get 'repeat spofy-player--current-state) "off")))
    (if (equal state "off")
        "Repeat off"
      (concat "Repeat " (propertize state 'face 'transient-value)))))

(defun spofy--cursor-follows-description ()
  "Return a description for the cursor-follows-playback state."
  (if (bound-and-true-p spofy-cursor-follows-playback-mode)
      (concat "Follow playback "
              (propertize "on" 'face 'transient-value))
    "Follow playback off"))

(defun spofy--shuffle-description ()
  "Return a description string for the current shuffle state."
  (if (alist-get 'shuffle spofy-player--current-state)
      (concat "Shuffle " (propertize "on" 'face 'transient-value))
    "Shuffle off"))

(transient-define-prefix spofy--menu ()
  "Spofy command popup (internal transient)."
  [:description spofy--transient-description]
  [["Playback"
    ("SPC" "Play/Pause"     spofy-play-pause)
    ("n" "Next"             spofy-next)
    ("p" "Previous"         spofy-previous)
    ("f" "Seek forward   "  spofy-seek-forward :transient t)
    ("r" "Seek backward  "  spofy-seek-backward :transient t)
    ("k" "Seek to"          spofy-seek-to)
    ""
    "Now playing"
    ("w s" "Save track"     spofy-save-current-track)
    ("w u" "Unsave track"   spofy-unsave-current-track)
    ("w j" "Jump to track"  spofy-jump-to-playing-track)
    ("w w" "Track info"     spofy-wikipedia)
    ("w t" "Timeline"       spofy-view-timeline)]
   ["Search Spotify"
    ("s t" "Tracks"         spofy-search-tracks)
    ("s a" "Albums"         spofy-search-albums)
    ("s p" "Playlists"      spofy-search-playlists)
    ("s r" "Artists"        spofy-search-artists)
    ""
    "Search library"
    ("l t" "Tracks"         spofy-library-search-tracks)
    ("l a" "Albums"         spofy-library-search-albums)
    ("l p" "Playlists"      spofy-library-search-playlists)
    ("l c" "Context"        spofy-library-search-context)
    ""
    "Browse library"
    ("b t" "Tracks"         spofy-library-browse-tracks)
    ("b a" "Albums"         spofy-library-browse-albums)
    ("b p" "Playlists"      spofy-library-browse-playlists)
    ("b c" "Context"        spofy-library-browse-context)]
   ["Volume"
    ("+" "Volume up"        spofy-volume-up :transient t)
    ("-" "Volume down"      spofy-volume-down :transient t)
    ("v" "Set volume"       spofy-volume-set)
    ""
    "Mode"
    ("F"                     spofy-cursor-follows-playback-mode
     :description spofy--cursor-follows-description :transient t)
    ("R"                    spofy-toggle-repeat
     :description spofy--repeat-description :transient t)
    ("S"                    spofy-toggle-shuffle
     :description spofy--shuffle-description :transient t)
    """"""""""
    ("/" "Dashboard"        spofy)
    ("d" "Devices"          spofy-select-device)
    ("q" "Quit"             transient-quit-one)]])

;;;###autoload (autoload 'spofy-menu "spofy" nil t)
(defun spofy-menu ()
  "Open the Spofy command popup.
Ensures Spofy is initialized (polling, tab-bar, etc.) before
showing the transient menu."
  (interactive)
  (spofy--ensure-global-mode)
  (call-interactively #'spofy--menu))


;;;; Context browsing

;;;###autoload
(defun spofy-library-browse-context ()
  "Browse the tracks of the current playback context.
Opens the album or playlist that is currently playing in a
standard browse buffer.  Signals an error when the context is
unsupported (e.g., Spotify-generated radio or daily mixes)."
  (interactive)
  (require 'spofy-player)
  (spofy-player--ensure-device)
  (unless spofy-player--current-state
    (spofy-player--poll-sync))
  (let ((context-uri (alist-get 'context-uri spofy-player--current-state))
        (context-type (alist-get 'context-type spofy-player--current-state)))
    (unless (and context-uri (member context-type '("album" "playlist")))
      (user-error "Spofy: cannot browse this context"))
    (let ((context-id (car (last (split-string context-uri ":")))))
      (pcase context-type
        ("album" (spofy-view-album context-id))
        ("playlist" (spofy-view-playlist context-id))))))

;;;; Convenience commands

;;;###autoload
(defun spofy-stop ()
  "Stop Spofy: disable polling and the global minor mode."
  (interactive)
  (require 'spofy-player)
  (spofy-player-stop-polling)
  (spofy--dashboard-stop-progress-timer)
  (remove-hook 'spofy-player-state-changed-hook
               #'spofy--dashboard-refresh-now-playing)
  (remove-hook 'spofy-player-track-changed-hook
               #'spofy--dashboard-refresh-recently-played)
  (remove-hook 'spofy-player-track-changed-hook
               #'spofy-ui--refresh-track-highlights)
  (remove-hook 'spofy-player-state-changed-hook
               #'spofy-ui--update-mode-lines)
  (when spofy-global-mode
    (spofy-global-mode -1))
  (message "Spofy: stopped."))

;;;; Global minor mode

(defvar spofy-global-mode-map (make-sparse-keymap)
  "Keymap for `spofy-global-mode'.
The actual binding is set up when the mode is enabled.")

;;;###autoload
(define-minor-mode spofy-global-mode
  "Global minor mode for Spofy.
When enabled, binds `spofy-global-key' to `spofy-menu', starts
polling, and optionally enables the mode-line display."
  :global t
  :group 'spofy
  :keymap spofy-global-mode-map
  (if spofy-global-mode
      (progn
        ;; Set up keybinding
        (define-key spofy-global-mode-map
                    (kbd spofy-global-key) #'spofy-menu)
        ;; Start polling
        (require 'spofy-player)
        (unless (and (boundp 'spofy-player--timer) spofy-player--timer)
          (spofy-player-start-polling))
        ;; Enable display: tab-bar takes precedence over mode-line to
        ;; avoid duplication (mode-line strings appear in the tab bar
        ;; via `tab-bar-format-global').
        (cond
         (spofy-enable-tab-bar
          (require 'spofy-tab-bar)
          (spofy-tab-bar-mode 1))
         (spofy-enable-mode-line
          (require 'spofy-mode-line)
          (spofy-mode-line-mode 1)))
        (require 'spofy-org))
    ;; Disable
    (define-key spofy-global-mode-map (kbd spofy-global-key) nil)
    (require 'spofy-player)
    (spofy-player-stop-polling)
    (when (fboundp 'spofy-mode-line-mode)
      (spofy-mode-line-mode -1))
    (when (fboundp 'spofy-tab-bar-mode)
      (spofy-tab-bar-mode -1))))

;;;; Auto-start

;; When display features (mode-line or tab-bar) are configured and a
;; non-expired access token is already on disk, enable `spofy-global-mode'
;; automatically on load.  This is deliberately passive: it loads tokens
;; from disk but never makes network requests (no token refresh, no API
;; calls).  If the token has expired, auto-start is silently skipped and
;; the user can start Spofy manually via `spofy-menu' or `spofy'.
(when (and (or spofy-enable-mode-line spofy-enable-tab-bar)
           (not spofy-global-mode))
  (require 'spofy-auth)
  (spofy-auth--load-tokens)
  (when (and spofy-auth--access-token
             (not (spofy-auth--token-expired-p)))
    (spofy-global-mode 1)))

(provide 'spofy)
;;; spofy.el ends here
