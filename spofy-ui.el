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

(defface spofy-now-playing-track
  '((t :inherit spofy-track-name :height 1.15))
  "Face for the track name in the now-playing dashboard section."
  :group 'spofy-faces)

(defface spofy-now-playing-artist
  '((t :inherit spofy-artist-name :height 1.15))
  "Face for the artist name in the now-playing dashboard section."
  :group 'spofy-faces)

(defface spofy-now-playing-album
  '((t :inherit spofy-album-name :height 1.15))
  "Face for the album name in the now-playing dashboard section."
  :group 'spofy-faces)

(defface spofy-playing
  '((t :inherit bold :foreground "#1DB954")) ; Spotify brand green
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

(defface spofy-progress-filled
  '((t :inherit success))
  "Face for the filled portion of the progress bar."
  :group 'spofy-faces)

(defface spofy-progress-empty
  '((t :inherit shadow))
  "Face for the empty portion of the progress bar."
  :group 'spofy-faces)

(defface spofy-muted
  '((t :inherit shadow))
  "Face for secondary text such as duration and dates."
  :group 'spofy-faces)

;;;; Column widths

(defcustom spofy-columns
  '((library-track    . (35 25 25))
    (library-album    . (35 25))
    (search-track     . (35 25 25))
    (search-album     . (35 25))
    (search-artist    . (35 30))
    (search-playlist  . (35 20))
    (album-track      . (40 25))
    (artist-album     . (40))
    (artist-top-track . (35 25))
    (playlist-track   . (30 20 20))
    (playlist-list    . (35 20)))
  "Proportional column weights for tabulated-list buffers.
Each entry is (VIEW . (W1 W2 ...)).  The values are proportional
weights for variable-width columns; they are scaled so that all
columns together fill the window width.  Fixed-width columns
\(indicators, duration, year, counts) are not listed here.

The variable-width columns per view are:

  library-track:    Name, Artist(s), Album
  library-album:    Name, Artist(s)
  search-track:     Name, Artist(s), Album
  search-album:     Name, Artist(s)
  search-artist:    Name, Genres
  search-playlist:  Name, Owner
  album-track:      Name, Artist(s)
  artist-album:     Name
  artist-top-track: Name, Album
  playlist-track:   Name, Artist(s), Album
  playlist-list:    Name, Owner"
  :type '(alist :key-type symbol
                :value-type (repeat integer))
  :group 'spofy)

(defvar-local spofy-ui--format-view nil
  "View symbol used to compute the current buffer's column format.")

(defvar-local spofy-ui--format-columns nil
  "Column recipe used to compute the current buffer's column format.
A list of specs (NAME WIDTH-OR-FLEX SORT . PROPS).  When WIDTH-OR-FLEX
is the keyword `:flex', the column width is computed proportionally.")

(defvar-local spofy-ui--computed-widths nil
  "Computed flex column widths for the current buffer.
An alist (VIEW . (W0 W1 ...)) set by `spofy-ui-compute-format'.")

(defun spofy-ui-col (view n)
  "Return the Nth flex column width for VIEW.
Uses the dynamically computed width when available, falling back
to the raw weight from `spofy-columns'."
  (or (nth n (alist-get view spofy-ui--computed-widths))
      (nth n (alist-get view spofy-columns))))

(defun spofy-ui-compute-format (view columns)
  "Compute a `tabulated-list-format' vector for VIEW with COLUMNS.
COLUMNS is a list of column specs, each (NAME WIDTH SORT . PROPS).
When WIDTH is `:flex', the column width is computed proportionally
from the VIEW entry in `spofy-columns' so that all columns together
fill the window.  Flex columns consume weights in order of appearance."
  (let ((fixed-total 0)
        (flex-count 0)
        (num-cols (length columns))
        (padding (or tabulated-list-padding 0)))
    (dolist (col columns)
      (if (eq (cadr col) :flex)
          (cl-incf flex-count)
        (cl-incf fixed-total (cadr col))))
    (let* ((weights (seq-take (alist-get view spofy-columns) flex-count))
           (total-weight (max 1 (apply #'+ weights)))
           ;; Floor of 20 chars prevents degenerate layouts in very narrow windows.
           (available (max 20 (- (window-body-width) padding fixed-total num-cols)))
           (computed nil)
           (flex-idx 0))
      (prog1
          (apply #'vector
                 (mapcar (lambda (col)
                           (if (eq (cadr col) :flex)
                               ;; Floor of 8 chars keeps columns readable
                               ;; even when the window is extremely narrow.
                               (let ((w (max 8 (floor (* available
                                                         (/ (float (nth flex-idx weights))
                                                            total-weight))))))
                                 (push w computed)
                                 (cl-incf flex-idx)
                                 `(,(car col) ,w ,@(cddr col)))
                             col))
                         columns))
        (setq spofy-ui--computed-widths
              (list (cons view (nreverse computed))))))))

(defun spofy-ui-set-format (view columns)
  "Set `tabulated-list-format' for VIEW with dynamic COLUMNS.
Stores the recipe for resize recomputation, sets the format,
and suppresses `tabulated-list-mode'\\='s built-in ellipsis so that
only the fade effect from `spofy-ui-truncate' indicates truncation."
  (setq-local spofy-ui--format-view view)
  (setq-local spofy-ui--format-columns columns)
  (setq-local truncate-string-ellipsis "")
  (setq tabulated-list-format (spofy-ui-compute-format view columns)))

(defun spofy-ui--recompute-columns ()
  "Recompute column widths for the current Spofy buffer after resize."
  (when (and spofy-ui--format-view spofy-ui--format-columns)
    (let ((new-format (spofy-ui-compute-format
                       spofy-ui--format-view spofy-ui--format-columns)))
      (unless (equal new-format tabulated-list-format)
        (setq tabulated-list-format new-format)
        (tabulated-list-init-header)
        (tabulated-list-print t)))))

(defun spofy-ui--handle-window-resize (frame)
  "Recompute column widths in Spofy buffers displayed in FRAME."
  (dolist (window (window-list frame 'no-minibuf))
    (when (buffer-local-value 'spofy-ui--format-view (window-buffer window))
      (with-selected-window window
        (spofy-ui--recompute-columns)))))

(add-hook 'window-size-change-functions #'spofy-ui--handle-window-resize)

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

(defun spofy-ui--blend-color (fg bg ratio)
  "Blend FG toward BG by RATIO (0.0 = pure FG, 1.0 = pure BG).
Return a hex color string, or nil if either color is unresolvable."
  (let ((fv (color-values fg))
        (bv (color-values bg)))
    (when (and fv bv)
      ;; `color-values' returns 16-bit components (0-65535); shift right
      ;; by 8 to convert to 8-bit hex (0-255) for the #RRGGBB format.
      (format "#%02x%02x%02x"
              (ash (round (+ (* (- 1.0 ratio) (nth 0 fv))
                             (* ratio (nth 0 bv))))
                   -8)
              (ash (round (+ (* (- 1.0 ratio) (nth 1 fv))
                             (* ratio (nth 1 bv))))
                   -8)
              (ash (round (+ (* (- 1.0 ratio) (nth 2 fv))
                             (* ratio (nth 2 bv))))
                   -8)))))

(defun spofy-ui-truncate (string max-width &optional face)
  "Truncate STRING to MAX-WIDTH display columns with optional FACE.
When FACE is non-nil, apply it to the result.  If the string was
truncated, mark the last three characters with the `spofy-fade'
text property for post-rendering color blending."
  (let* ((truncated (> (string-width string) max-width))
         (result (if truncated
                     (truncate-string-to-width string max-width)
                   string)))
    (when face
      (setq result (propertize result 'face face))
      (when truncated
        (let ((len (length result)))
          (when (>= len 3)
            (dotimes (i 3)
              (put-text-property (+ (- len 3) i) (+ (- len 3) i 1)
                                 'spofy-fade (1+ i) result))))))
    result))

(defvar-local spofy-ui--fade-overlays nil
  "Overlays for the truncation fade effect in the current buffer.")

(defun spofy-ui--apply-buffer-fades ()
  "Create fade overlays for characters marked with `spofy-fade'.
Each marked character gets an overlay whose foreground is the base
face's foreground blended toward the default background.  The last
three characters of a truncated string are faded progressively:
level 1 at 25%, level 2 at 50%, level 3 at 75% — producing a
gradient that visually communicates \"text continues beyond here\"
without an abrupt ellipsis."
  (mapc #'delete-overlay spofy-ui--fade-overlays)
  (setq spofy-ui--fade-overlays nil)
  (let ((bg (face-background 'default nil t)))
    (when bg
      (save-excursion
        (goto-char (point-min))
        (let ((pos (point-min)))
          (while (< pos (point-max))
            (let ((level (get-text-property pos 'spofy-fade)))
              (if (not level)
                  (setq pos (or (next-single-property-change
                                 pos 'spofy-fade nil (point-max))
                                (point-max)))
                (let* ((face-val (get-text-property pos 'face))
                       (base-face (cond
                                   ((symbolp face-val) face-val)
                                   ((consp face-val)
                                    (seq-find #'symbolp face-val))
                                   (t 'default)))
                       (fg (or (face-foreground base-face nil t)
                               (face-foreground 'default nil t)))
                       (ratio (* 0.25 level))
                       (blended (when fg
                                  (spofy-ui--blend-color fg bg ratio))))
                  (when blended
                    (let ((ov (make-overlay pos (1+ pos))))
                      (overlay-put ov 'face (list :foreground blended))
                      (overlay-put ov 'spofy-fade t)
                      (push ov spofy-ui--fade-overlays))))
                (setq pos (1+ pos))))))))))

(defun spofy-ui--refresh-fades (&rest _)
  "Recompute fade overlays in all Spofy buffers.
Intended for `enable-theme-functions'."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (string-prefix-p "*Spofy" (buffer-name))
        (spofy-ui--apply-buffer-fades)))))

(add-hook 'enable-theme-functions #'spofy-ui--refresh-fades)


(defun spofy-ui-progress-bar-only (progress-ms duration-ms width)
  "Build a text progress bar for PROGRESS-MS out of DURATION-MS.
WIDTH is the total character width of the bar.  Uses full-block
characters (U+2588) for the filled portion and light-shade
characters (U+2591) for the empty portion."
  (let* ((progress (or progress-ms 0))
         (duration (max 1 (or duration-ms 1)))
         (fraction (min 1.0 (/ (float progress) duration)))
         (filled (round (* fraction width)))
         (empty (- width filled)))
    (concat (propertize (make-string (max 0 filled) ?\u2588)
                        'face 'spofy-progress-filled)
            (propertize (make-string (max 0 empty) ?\u2591)
                        'face 'spofy-progress-empty))))

(defun spofy-ui-progress-time (progress-ms duration-ms)
  "Build a time display string for PROGRESS-MS out of DURATION-MS.
Returns a string like \"1:23 / 3:45\"."
  (let ((progress-str (spofy-ui-format-duration-ms (or progress-ms 0)))
        (duration-str (spofy-ui-format-duration-ms (max 1 (or duration-ms 1)))))
    (format "%s / %s" progress-str duration-str)))

(defvar-local spofy-ui--next-page-url nil
  "URL for the next page of results in this buffer.")

(defvar-local spofy-ui--entity-type nil
  "The type of Spotify entity displayed in this buffer (track, album, etc.).")

(defvar-local spofy-ui--load-more-handler nil
  "Function to handle loading the next page of results.
When non-nil, called with the API response for the next page.
Must return a cons cell (NEW-ENTRIES . NEXT-URL) where NEW-ENTRIES
is a list of `tabulated-list-entries' items and NEXT-URL is the URL
for the following page, or nil.")

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

(declare-function spofy-api--request "spofy-api"
                  (method url &optional params data callback))

(defvar-local spofy-ui--loading-more nil
  "Non-nil while an auto-pagination request is in flight.")

(defun spofy-ui--maybe-load-more (window _start)
  "Auto-load the next page when WINDOW scrolls near the end of the buffer."
  (with-current-buffer (window-buffer window)
    (when (and spofy-ui--load-more-handler
               (stringp spofy-ui--next-page-url)
               (not spofy-ui--loading-more)
               ;; Trigger when fewer than 3 characters remain below the
               ;; visible window — i.e., the user has scrolled to the bottom.
               (<= (- (point-max) (window-end window t)) 3))
      (setq spofy-ui--loading-more t)
      (require 'spofy-api)
      (let ((buf (current-buffer))
            (handler spofy-ui--load-more-handler))
        (spofy-api--request
         "GET" spofy-ui--next-page-url nil nil
         (lambda (response)
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (setq spofy-ui--loading-more nil)
               (pcase-let ((`(,new-entries . ,next-url)
                            (funcall handler response)))
                 (setq spofy-ui--next-page-url
                       (unless (eq next-url :null) next-url))
                 (setq tabulated-list-entries
                       (append tabulated-list-entries new-entries))
                 (let ((saved-point (point))
                       (saved-start (window-start window)))
                   (tabulated-list-print t)
                   (goto-char saved-point)
                   (set-window-start window saved-start t)))))))))))

(add-hook 'window-scroll-functions #'spofy-ui--maybe-load-more)

(defun spofy-ui--maybe-load-more-post-command ()
  "Check for auto-pagination after each command.
This complements `window-scroll-functions', which can miss scroll
events under `pixel-scroll-precision-mode'."
  (when-let* ((window (selected-window))
              (buf (window-buffer window)))
    (when (and (buffer-local-value 'spofy-ui--load-more-handler buf)
               (string-prefix-p "*Spofy" (buffer-name buf)))
      (spofy-ui--maybe-load-more window nil))))

(add-hook 'post-command-hook #'spofy-ui--maybe-load-more-post-command)

(defun spofy-ui--after-tabulated-list-print (&rest _)
  "Post-process Spofy tabulated-list buffers after printing.
Apply truncation fade overlays.
Added as :after advice on `tabulated-list-print'."
  (when (string-prefix-p "*Spofy" (buffer-name))
    (spofy-ui--apply-buffer-fades)))

(advice-add 'tabulated-list-print :after #'spofy-ui--after-tabulated-list-print)

;;;; Entity storage

(defvar-local spofy-ui--entities nil
  "Hash table mapping entity URIs/IDs to their full API alists.
Buffer-local in every Spofy list buffer.")

(defun spofy-ui-store-entity (key entity)
  "Store ENTITY alist under KEY in the buffer-local entity table."
  (unless spofy-ui--entities
    (setq spofy-ui--entities (make-hash-table :test #'equal)))
  (puthash key entity spofy-ui--entities))

(defun spofy-ui-entity-at-point ()
  "Return the full alist for the entity on the current tabulated-list row.
The row ID is used as the key into `spofy-ui--entities'."
  (when-let* ((id (tabulated-list-get-id)))
    (and spofy-ui--entities
         (gethash id spofy-ui--entities))))

;;;; URI and metadata helpers

(declare-function spofy-player-current-track-id "spofy-player" ())

(defun spofy-ui-extract-id (uri)
  "Extract the Spotify ID from URI (e.g. \"spotify:track:xyz\" -> \"xyz\")."
  (if (string-match "spotify:[^:]+:\\(.+\\)" uri)
      (match-string 1 uri)
    uri))

(defun spofy-ui-album-year (album)
  "Extract the release year from ALBUM alist."
  (let ((date (or (alist-get 'release_date album) "")))
    (if (string-match "\\`\\([0-9]\\{4\\}\\)" date)
        (match-string 1 date)
      date)))

(defun spofy-ui-playing-indicator (track-uri)
  "Return a now-playing indicator string for TRACK-URI.
Shows a play icon if TRACK-URI matches the currently playing track."
  (let* ((current-id (and (fboundp 'spofy-player-current-track-id)
                          (spofy-player-current-track-id)))
         (playing-p (and current-id track-uri
                         (string-match-p (regexp-quote current-id) track-uri))))
    (if playing-p
        (propertize "\u25B6" 'face 'spofy-playing-icon)
      "")))

;;;; Player state indicators

(defun spofy-ui-play-pause-icon (state)
  "Return the play/pause icon based on player STATE."
  (if (alist-get 'is-playing state) "⏸" "▶"))

(defun spofy-ui-shuffle-indicator (state)
  "Return the shuffle indicator based on player STATE."
  (if (alist-get 'shuffle state) "⇌" ""))

(defun spofy-ui-repeat-indicator (state)
  "Return the repeat indicator based on player STATE."
  (pcase (alist-get 'repeat state)
    ("context" "↻")
    ("track"   "↻₁")
    (_         "")))

;;;; List buffer rendering

(defun spofy-ui-render-list (buf-name mode-fn entries next-url
                                      &optional load-more-handler display-fn)
  "Render ENTRIES in BUF-NAME using MODE-FN with pagination.
ENTRIES may be a list of `tabulated-list-entries' items, or a
function of no arguments that returns such a list.  When a
function, it is called after the mode has been initialized so
that `spofy-ui-col' can access dynamically computed column widths.
NEXT-URL is the URL for the next page of results, or nil.
LOAD-MORE-HANDLER, when non-nil, is set as `spofy-ui--load-more-handler'.
DISPLAY-FN controls how the buffer is displayed: defaults to `pop-to-buffer';
pass `switch-to-buffer' for browse views."
  (let ((buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer))
      (funcall mode-fn)
      (setq-local spofy-ui--next-page-url next-url)
      (when load-more-handler
        (setq-local spofy-ui--load-more-handler load-more-handler))
      (setq tabulated-list-entries
            (if (functionp entries) (funcall entries) entries))
      (tabulated-list-print t)
      (goto-char (point-min)))
    (funcall (or display-fn #'pop-to-buffer) buf)))

(provide 'spofy-ui)
;;; spofy-ui.el ends here
