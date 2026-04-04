;;; spofy.el --- Spotify player for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;; Author: Pablo Stafforini
;; URL: https://github.com/pablostafforini/spofy
;; Version: 0.1.0
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
(declare-function spofy-list-playlists "spofy-playlist" ())

;; spofy-library
(declare-function spofy-library-saved-tracks "spofy-library" ())
(declare-function spofy-library-saved-albums "spofy-library" ())
(declare-function spofy-list-recently-played "spofy-library" ())
(declare-function spofy-list-top-tracks "spofy-library" (&optional time-range))
(declare-function spofy-list-top-artists "spofy-library" (&optional time-range))
(declare-function spofy-list-new-releases "spofy-library" ())

;; spofy-mode-line
(declare-function spofy-mode-line-mode "spofy-mode-line" (&optional arg))

;; spofy-tab-bar
(declare-function spofy-tab-bar-mode "spofy-tab-bar" (&optional arg))

;; spofy-wikipedia
(declare-function spofy-wikipedia "spofy-wikipedia" ())

;;;; Variables from other modules (for byte-compiler)

(defvar spofy-auth--access-token)
(defvar spofy-player--current-state)
(defvar spofy-player-state-changed-hook)
(defvar spofy-player--timer)
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
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `spofy-dashboard-mode'.")

(define-derived-mode spofy-dashboard-mode special-mode "Spofy"
  "Major mode for the Spofy dashboard buffer."
  :group 'spofy)

;;;;; Dashboard: now-playing section

(defun spofy--dashboard-insert-now-playing ()
  "Insert the now-playing section into the current buffer."
  (let* ((state (and (boundp 'spofy-player--current-state)
                     spofy-player--current-state))
         (track (and state (alist-get 'track state)))
         (artist (and state (alist-get 'artist state)))
         (album (and state (alist-get 'album state)))
         (progress (and state (spofy-player-interpolated-progress)))
         (duration (and state (alist-get 'duration state)))
         (is-playing (and state (alist-get 'is-playing state)))
         (shuffle (and state (alist-get 'shuffle state)))
         (repeat-state (and state (alist-get 'repeat state))))
    (insert (propertize "Now playing" 'face 'spofy-header) "\n\n")
    (if (not track)
        (insert (propertize "  No track playing" 'face 'spofy-muted) "\n")
      (insert "  "
              (propertize track 'face 'spofy-track-name)
              " "
              (propertize (or artist "") 'face 'spofy-artist-name)
              "\n")
      (insert "  " (propertize (or album "") 'face 'spofy-album-name) "\n")
      (insert "  "
              (spofy-ui-progress-bar progress (or duration 0) 20)
              "\n")
      (insert "  "
              (if is-playing "⏸" "▶")
              "  "
              (format "shuffle: %s" (if shuffle "on" "off"))
              "  "
              (format "repeat: %s" (or repeat-state "off"))
              "\n"))))

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
           (concat (propertize (concat name " ") 'face 'spofy-track-name)
                   (propertize artist-str 'face 'spofy-artist-name))
           'action (lambda (_btn)
                     (require 'spofy-player)
                     (spofy-play-track uri))
           'spofy-uri uri
           'follow-link t)
          (insert "\n")))))
  (insert "\n  ")
  (insert-text-button
   (propertize "View all recently played..." 'face 'spofy-muted)
   'action (lambda (_btn)
             (require 'spofy-library)
             (spofy-list-recently-played))
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
         (concat (propertize (concat name " ") 'face 'spofy-track-name)
                 (propertize (format "(%d tracks)" total) 'face 'spofy-muted))
         'action (lambda (_btn)
                   (require 'spofy-browse)
                   (spofy-view-playlist playlist-id))
         'spofy-playlist-id playlist-id
         'follow-link t)
        (insert "\n"))))
  (insert "\n  ")
  (insert-text-button
   (propertize "View all playlists..." 'face 'spofy-muted)
   'action (lambda (_btn)
             (require 'spofy-playlist)
             (spofy-list-playlists))
   'follow-link t)
  (insert "\n"))

;;;;; Dashboard: top tracks section

(defun spofy--dashboard-insert-top-tracks (items time-range)
  "Insert a top-tracks section from ITEMS into the current buffer.
TIME-RANGE is \"short_term\", \"medium_term\", or \"long_term\"."
  (let ((label (pcase time-range
                 ("short_term" "Top tracks (4 weeks)")
                 ("medium_term" "Top tracks (6 months)")
                 ("long_term" "Top tracks (1 year)"))))
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
           (concat (propertize (concat name " ") 'face 'spofy-track-name)
                   (propertize artist-str 'face 'spofy-artist-name))
           'action (lambda (_btn)
                     (require 'spofy-player)
                     (spofy-play-track uri))
           'spofy-uri uri
           'follow-link t)
          (insert "\n"))))
    (insert "\n  ")
    (insert-text-button
     (propertize "View all..." 'face 'spofy-muted)
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
                 ("long_term" "Top artists (1 year)"))))
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
           (concat (propertize name 'face 'spofy-track-name)
                   (unless (string-empty-p genre-str)
                     (propertize (concat " " genre-str) 'face 'spofy-muted)))
           'action (lambda (_btn)
                     (require 'spofy-browse)
                     (spofy-view-artist id))
           'follow-link t)
          (insert "\n"))))
    (insert "\n  ")
    (insert-text-button
     (propertize "View all..." 'face 'spofy-muted)
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
         (concat (propertize (concat name " ") 'face 'spofy-album-name)
                 (propertize artist-str 'face 'spofy-artist-name))
         'action (lambda (_btn)
                   (require 'spofy-browse)
                   (spofy-view-album album-id))
         'follow-link t)
        (insert "\n"))))
  (insert "\n  ")
  (insert-text-button
   (propertize "View all..." 'face 'spofy-muted)
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

;;;;; Dashboard: keybinding hints

(defun spofy--dashboard-insert-hints ()
  "Insert keybinding hints at the bottom of the dashboard."
  (insert "\n"
          (propertize
           "SPC Play/Pause  n Next  p Previous  / Search  q Quit"
           'face 'spofy-muted)
          "\n"))

;;;;; Dashboard: now-playing auto-refresh

(defvar spofy--dashboard-now-playing-marker nil
  "Marker for the start of the now-playing section in the dashboard.")

(defvar spofy--dashboard-now-playing-end-marker nil
  "Marker for the end of the now-playing section in the dashboard.")

(defvar spofy--dashboard-section-markers nil
  "Alist mapping section index to start marker.
The last entry is a sentinel marking the start of the hints section.")

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
    ;; Create markers for each async section + a sentinel for hints
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
    (spofy--dashboard-insert-hints)
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
                    (set-marker end-marker (point)))
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
  ;; Start progress interpolation timer
  (spofy--dashboard-start-progress-timer)
  ;; Create and display the dashboard
  (let ((buf (get-buffer-create "*Spofy*")))
    (with-current-buffer buf
      (spofy-dashboard-mode)
      (spofy--dashboard-render))
    (pop-to-buffer buf)))

(defun spofy-dashboard-refresh ()
  "Refresh the Spofy dashboard."
  (interactive)
  (require 'spofy-api)
  (require 'spofy-player)
  (when (string= (buffer-name) "*Spofy*")
    (spofy--dashboard-render)))

;;;; Initialization helper

(defun spofy--ensure-global-mode ()
  "Ensure `spofy-global-mode' is active.
Loads required modules, checks authentication, and enables the mode.
Signals `user-error' if authentication tokens are not available."
  (require 'spofy-auth)
  (require 'spofy-api)
  (require 'spofy-player)
  (spofy-auth--load-tokens)
  (unless (spofy-auth-access-token)
    (user-error "Spofy: not authenticated; run `spofy' to authenticate"))
  (unless spofy-global-mode
    (spofy-global-mode 1)))

;;;; Transient popup

(defun spofy--transient-description ()
  "Return the description string for the transient popup header."
  (require 'spofy-player)
  (let ((track-info (spofy-player-current-track)))
    (if track-info
        (let ((icon (if (spofy-player-playing-p) "⏸" "▶")))
          (format "Spofy: %s %s — %s" icon (car track-info) (cdr track-info)))
      "Spofy: no track playing")))

(transient-define-prefix spofy--menu ()
  "Spofy command popup (internal transient)."
  [:description spofy--transient-description]
  [["Playback"
    ("SPC" "Play/Pause"  spofy-play-pause)
    ("n" "Next"          spofy-next)
    ("p" "Previous"      spofy-previous)
    ("f" "Seek forward"  spofy-seek-forward :transient t)
    ("r" "Seek backward" spofy-seek-backward :transient t)
    ""
    "Volume"
    ("+" "Volume up"   spofy-volume-up :transient t)
    ("-" "Volume down" spofy-volume-down :transient t)
    ("v" "Set volume"  spofy-volume-set)
    ""
    "Mode"
    ("S" spofy-toggle-shuffle
     :description (lambda ()
                    (if (alist-get 'shuffle spofy-player--current-state)
                        (concat "Shuffle " (propertize "on" 'face 'transient-value))
                      "Shuffle off"))
     :transient t)
    ("R" spofy-toggle-repeat
     :description (lambda ()
                    (let ((state (or (alist-get 'repeat spofy-player--current-state) "off")))
                      (if (equal state "off")
                          "Repeat off"
                        (concat "Repeat " (propertize state 'face 'transient-value)))))
     :transient t)]
   ["Search"
    ("s t" "Tracks"    spofy-search-tracks)
    ("s l" "Albums"    spofy-search-albums)
    ("s a" "Artists"   spofy-search-artists)
    ("s p" "Playlists" spofy-search-playlists)
    ""
    "Browse"
    ("l"  "Library"   spofy-library-menu)
    ("P"  "Playlists" spofy-list-playlists)
    ("d"  "Devices"   spofy-select-device)
    ""
    "Other"
    ("w" "Wikipedia" spofy-wikipedia)
    ("D" "Dashboard" spofy)
    ("q" "Quit"      transient-quit-one)]])

;;;###autoload (autoload 'spofy-menu "spofy" nil t)
(defun spofy-menu ()
  "Open the Spofy command popup.
Ensures Spofy is initialized (polling, tab-bar, etc.) before
showing the transient menu."
  (interactive)
  (spofy--ensure-global-mode)
  (call-interactively #'spofy--menu))

;;;###autoload (autoload 'spofy-library-menu "spofy" nil t)
(transient-define-prefix spofy-library-menu ()
  "Spofy library sub-menu."
  ["Library"
   ("t" "Saved tracks" spofy-library-saved-tracks)
   ("a" "Saved albums" spofy-library-saved-albums)
   ("q" "Back"         transient-quit-one)])

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
          (spofy-mode-line-mode 1))))
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
