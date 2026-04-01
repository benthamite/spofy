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

;; spofy-playlist
(declare-function spofy-list-playlists "spofy-playlist" ())

;; spofy-library
(declare-function spofy-library-saved-tracks "spofy-library" ())
(declare-function spofy-library-saved-albums "spofy-library" ())

;; spofy-mode-line
(declare-function spofy-mode-line-mode "spofy-mode-line" (&optional arg))

;; spofy-tab-bar
(declare-function spofy-tab-bar-mode "spofy-tab-bar" (&optional arg))

;;;; Variables from other modules (for byte-compiler)

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

;;;; Dashboard buffer

(defvar spofy-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p")   #'spofy-play-pause)
    (define-key map (kbd "n")   #'spofy-next)
    (define-key map (kbd "b")   #'spofy-previous)
    (define-key map (kbd "/")   #'spofy-menu)
    (define-key map (kbd "g")   #'spofy-dashboard-refresh)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `spofy-dashboard-mode'.")

(define-derived-mode spofy-dashboard-mode special-mode "Spofy"
  "Major mode for the Spofy dashboard buffer."
  :group 'spofy)

;;;;; Dashboard: now-playing section

(defun spofy--dashboard-progress-bar (progress-ms duration-ms width)
  "Build a text progress bar for PROGRESS-MS out of DURATION-MS.
WIDTH is the total character width of the bar (excluding brackets)."
  (let* ((progress (or progress-ms 0))
         (duration (max 1 (or duration-ms 1)))
         (fraction (min 1.0 (/ (float progress) duration)))
         (filled (round (* fraction width)))
         (empty (- width filled))
         (progress-str (spofy-ui-format-duration-ms progress))
         (duration-str (spofy-ui-format-duration-ms duration)))
    (format "[%s%s%s] %s / %s"
            (make-string (max 0 (1- filled)) ?=)
            (if (> filled 0) ">" "")
            (make-string (max 0 empty) ? )
            progress-str duration-str)))

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
              (propertize " — " 'face 'spofy-muted)
              (propertize (or artist "") 'face 'spofy-artist-name)
              "\n")
      (insert "  " (propertize (or album "") 'face 'spofy-album-name) "\n")
      (insert "  "
              (propertize (spofy--dashboard-progress-bar
                           progress (or duration 0) 20)
                          'face 'spofy-muted)
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
           (concat (propertize name 'face 'spofy-track-name)
                   (propertize " — " 'face 'spofy-muted)
                   (propertize artist-str 'face 'spofy-artist-name))
           'action (lambda (_btn)
                     (require 'spofy-player)
                     (spofy-play-track uri))
           'spofy-uri uri
           'follow-link t)
          (insert "\n"))))))

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
         (format "%s (%d tracks)" name total)
         'action (lambda (_btn)
                   (require 'spofy-browse)
                   (spofy-view-playlist playlist-id))
         'spofy-playlist-id playlist-id
         'follow-link t)
        (insert "\n"))))
  ;; "View all playlists..." link
  (insert "\n  ")
  (insert-text-button
   "View all playlists..."
   'action (lambda (_btn)
             (require 'spofy-playlist)
             (spofy-list-playlists))
   'follow-link t)
  (insert "\n"))

;;;;; Dashboard: keybinding hints

(defun spofy--dashboard-insert-hints ()
  "Insert keybinding hints at the bottom of the dashboard."
  (insert "\n"
          (propertize
           "p Play/Pause  n Next  b Previous  / Search  q Quit"
           'face 'spofy-muted)
          "\n"))

;;;;; Dashboard: now-playing auto-refresh

(defvar spofy--dashboard-now-playing-marker nil
  "Marker for the start of the now-playing section in the dashboard.")

(defvar spofy--dashboard-now-playing-end-marker nil
  "Marker for the end of the now-playing section in the dashboard.")

(defvar spofy--dashboard-recent-marker nil
  "Marker for the start of the recently-played section.")

(defvar spofy--dashboard-playlist-marker nil
  "Marker for the start of the playlists section.")

(defvar spofy--dashboard-hint-marker nil
  "Marker for the start of the hints section.")

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
              ;; Keep section markers in sync after re-render
              (when (markerp spofy--dashboard-recent-marker)
                (set-marker spofy--dashboard-recent-marker end)))
            (goto-char (min pos (point-max)))))))))

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
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; Now playing section (with markers for partial refresh)
    (setq-local spofy--dashboard-now-playing-marker (point-marker))
    (spofy--dashboard-insert-now-playing)
    (setq-local spofy--dashboard-now-playing-end-marker (point-marker))
    ;; Fetch recently played and playlists asynchronously.
    (setq-local spofy--dashboard-recent-marker (point-marker))
    (insert (propertize "\nLoading recently played..." 'face 'spofy-muted) "\n")
    (setq-local spofy--dashboard-playlist-marker (point-marker))
    (insert (propertize "\nLoading playlists..." 'face 'spofy-muted) "\n")
    (setq-local spofy--dashboard-hint-marker (point-marker))
    (spofy--dashboard-insert-hints)
    (let ((buf (current-buffer)))
      (spofy-api-get
       "me/player/recently-played"
       '(("limit" . "10"))
       (lambda (response)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (let ((inhibit-read-only t)
                   (items (alist-get 'items response)))
               (delete-region spofy--dashboard-recent-marker
                              spofy--dashboard-playlist-marker)
               (goto-char spofy--dashboard-recent-marker)
               (spofy--dashboard-insert-recently-played items)
               (set-marker spofy--dashboard-playlist-marker (point)))))))
      (spofy-api-get
       "me/playlists"
       '(("limit" . "10"))
       (lambda (response)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (let ((inhibit-read-only t)
                   (items (alist-get 'items response)))
               (delete-region spofy--dashboard-playlist-marker
                              spofy--dashboard-hint-marker)
               (goto-char spofy--dashboard-playlist-marker)
               (spofy--dashboard-insert-playlists items)))))))))

;;;;; Dashboard: interactive commands

;;;###autoload
(cl-defun spofy ()
  "Open the Spofy dashboard.
The main entry point for the Spofy Spotify client.  Ensures
authentication, starts polling, and displays the dashboard buffer."
  (interactive)
  ;; Lazy-load modules
  (require 'spofy-auth)
  (require 'spofy-api)
  (require 'spofy-player)
  ;; Try to load persisted tokens
  (spofy-auth--load-tokens)
  ;; Ensure we are authenticated
  (unless (spofy-auth-access-token)
    (if (y-or-n-p "Spofy: not authenticated.  Authenticate now? ")
        (progn
          (spofy-authenticate)
          (message "Spofy: complete authentication in your browser, then run `spofy' again.")
          (cl-return-from spofy))
      (user-error "Spofy: authentication required")))
  ;; Start polling if not already running
  (unless (and (boundp 'spofy-player--timer) spofy-player--timer)
    (spofy-player-start-polling))
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

;;;; Transient popup

(defun spofy--transient-description ()
  "Return the description string for the transient popup header."
  (require 'spofy-player)
  (let ((track-info (spofy-player-current-track)))
    (if track-info
        (let ((icon (if (spofy-player-playing-p) "⏸" "▶")))
          (format "Spofy: %s %s — %s" icon (car track-info) (cdr track-info)))
      "Spofy: no track playing")))

;;;###autoload (autoload 'spofy-menu "spofy" nil t)
(transient-define-prefix spofy-menu ()
  "Spofy command popup."
  [:description spofy--transient-description]
  [["Playback"
    ("p" "Play/Pause"    spofy-play-pause)
    ("n" "Next"          spofy-next)
    ("b" "Previous"      spofy-previous)
    ("f" "Seek forward"  spofy-seek-forward)
    ("r" "Seek backward" spofy-seek-backward)]
   ["Search"
    ("s t" "Tracks"    spofy-search-tracks)
    ("s l" "Albums"    spofy-search-albums)
    ("s a" "Artists"   spofy-search-artists)
    ("s p" "Playlists" spofy-search-playlists)]
   ["Browse"
    ("l"  "Library"   spofy--transient-library)
    ("P"  "Playlists" spofy-list-playlists)
    ("d"  "Devices"   spofy-select-device)]
   ["Volume"
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
     :transient t)
    ""
    "Other"
    ("D" "Dashboard" spofy)
    ("q" "Quit"      transient-quit-one)]])

;;;###autoload (autoload 'spofy--transient-library "spofy" nil t)
(transient-define-prefix spofy--transient-library ()
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
        ;; Enable mode-line if configured
        (when spofy-enable-mode-line
          (require 'spofy-mode-line)
          (spofy-mode-line-mode 1))
        ;; Enable tab-bar if configured
        (when spofy-enable-tab-bar
          (require 'spofy-tab-bar)
          (spofy-tab-bar-mode 1)))
    ;; Disable
    (define-key spofy-global-mode-map (kbd spofy-global-key) nil)
    (require 'spofy-player)
    (spofy-player-stop-polling)
    (when (and spofy-enable-mode-line
               (fboundp 'spofy-mode-line-mode))
      (spofy-mode-line-mode -1))
    (when (and spofy-enable-tab-bar
               (fboundp 'spofy-tab-bar-mode))
      (spofy-tab-bar-mode -1))))

(provide 'spofy)
;;; spofy.el ends here
