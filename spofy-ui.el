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

(require 'cl-lib)

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
  '((t :weight ultra-bold))
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
only the fade effect from `spofy-ui-truncate' indicates truncation.
Also installs the Spofy buffer mode-line and common keybindings."
  (setq-local spofy-ui--format-view view)
  (setq-local spofy-ui--format-columns columns)
  (setq-local truncate-string-ellipsis "")
  (setq-local cursor-type nil)
  (setq tabulated-list-format (spofy-ui-compute-format view columns))
  (setq-local mode-line-format spofy-ui-mode-line-format)
  (local-set-key (kbd ".") #'spofy-jump-to-playing-track))

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

(defun spofy-ui--resolve-foreground (face-val)
  "Resolve the effective foreground color from FACE-VAL.
FACE-VAL may be a symbol or a list of face symbols."
  (cond
   ((symbolp face-val)
    (face-foreground face-val nil t))
   ((consp face-val)
    (cl-some (lambda (f)
               (and (facep f) (face-foreground f nil t)))
             face-val))))

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
                       (fg (or (spofy-ui--resolve-foreground face-val)
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
Intended for `enable-theme-functions'.  The playing-line border
also updates its color, though vertical borders may not render
correctly until the buffer is refreshed with `g'."
  (spofy-ui--refresh-track-highlights)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (string-prefix-p "*Spofy" (buffer-name))
                 (not tabulated-list-entries))
        (spofy-ui--apply-buffer-fades)))))

(add-hook 'enable-theme-functions #'spofy-ui--refresh-fades)

;;;; Playing-line border overlay

(defun spofy-ui--remove-playing-line-padding ()
  "Remove any padding spaces inserted by the playing-line overlay."
  (with-silent-modifications
    (let ((pos (point-min)))
      (while (setq pos (text-property-any pos (point-max) 'spofy-padding t))
        (delete-region pos (or (next-single-property-change pos 'spofy-padding)
                               (point-max)))))))

(defvar-local spofy-ui--playing-line-overlay nil
  "Overlay highlighting the full line of the currently playing track.")

(defun spofy-ui--apply-playing-line-overlay ()
  "Place a border overlay on the line of the currently playing track."
  (when spofy-ui--playing-line-overlay
    (delete-overlay spofy-ui--playing-line-overlay)
    (setq spofy-ui--playing-line-overlay nil))
  (spofy-ui--remove-playing-line-padding)
  (let ((current-id (and (fboundp 'spofy-player-current-track-id)
                         (spofy-player-current-track-id))))
    (when current-id
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let ((entry-id (tabulated-list-get-id)))
            (when (and entry-id (string-match-p (regexp-quote current-id) entry-id))
              (let* ((color (face-foreground 'default nil t))
                     (end-col (save-excursion (end-of-line) (current-column)))
                     (padding (max 0 (- (window-body-width) end-col -2))))
                (with-silent-modifications
                  (save-excursion
                    (end-of-line)
                    (insert (propertize (make-string padding ?\s) 'spofy-padding t))))
                (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
                  (overlay-put ov 'face
                               `(:box (:line-width (-1 . -1) :color ,color)))
                  (setq spofy-ui--playing-line-overlay ov))
                (goto-char (point-max))))
            (forward-line 1)))))))

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

(defvar-local spofy-ui--buffer-entity-name nil
  "Name of the entity displayed in this buffer, shown in the mode line.")

(defvar-local spofy-ui--entry-formatter nil
  "Function to re-format entries when the current track changes.
Called with (ENTITY INDEX) for each entry whose URI maps to an
entity in `spofy-ui--entities'.  Must return a
`tabulated-list-entries' item (URI VECTOR).")

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
Apply truncation fade overlays, playing-line border, and pending
jump-to-track navigation.  Added as :after advice on
`tabulated-list-print'."
  (when (string-prefix-p "*Spofy" (buffer-name))
    (spofy-ui--apply-buffer-fades)
    (spofy-ui--apply-playing-line-overlay)
    (spofy-ui--maybe-jump-to-pending-track)))

(defun spofy-ui--maybe-jump-to-pending-track ()
  "Schedule a jump to the pending track after the render completes.
Deferred to the next event loop tick so that the render
function's `goto-char' and `switch-to-buffer' finish first."
  (when spofy-ui--pending-jump-track-id
    (let ((id spofy-ui--pending-jump-track-id)
          (buf (current-buffer)))
      (setq spofy-ui--pending-jump-track-id nil)
      (run-with-timer 0 nil
                      #'spofy-ui--execute-pending-jump buf id))))

(defun spofy-ui--execute-pending-jump (buf track-id)
  "Jump to TRACK-ID in BUF if the buffer is still live."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (spofy-ui--goto-playing-track track-id))))

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

(defun spofy-ui-row-position ()
  "Return the 0-indexed position of the tabulated-list row at point.
Position is the row's index in `tabulated-list-entries', which matches
the offset Spotify expects for \"offset.position\" in a context-based
play request.  Returns nil when point is not on a row."
  (when-let* ((id (tabulated-list-get-id)))
    (cl-position id tabulated-list-entries :key #'car :test #'equal)))

;;;; URI and metadata helpers

(defvar spofy-player--current-state)
(declare-function spofy-player-current-track-id "spofy-player" ())
(declare-function spofy-jump-to-playing-track "spofy-player" ())

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

(defun spofy-ui-playing-face (base-face playing-p)
  "Return BASE-FACE with bold added when PLAYING-P is non-nil."
  (if playing-p (list 'spofy-playing base-face) base-face))

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

;;;; Playing track navigation

(defvar spofy-ui--pending-jump-track-id nil
  "Track ID to jump to after the next buffer render.")

(defun spofy-ui--find-track-position (track-id)
  "Return buffer position of the row matching TRACK-ID, or nil."
  (save-excursion
    (goto-char (point-min))
    (catch 'found
      (while (not (eobp))
        (when-let* ((entry-id (tabulated-list-get-id)))
          (when (string-match-p (regexp-quote track-id) entry-id)
            (throw 'found (point))))
        (forward-line 1))
      nil)))

(defun spofy-ui--goto-playing-track (&optional track-id)
  "Move point to the row of the currently playing track.
If TRACK-ID is non-nil, use it instead of the current player
state.  Return non-nil if found."
  (when-let* ((id (or track-id
                      (and (fboundp 'spofy-player-current-track-id)
                           (spofy-player-current-track-id))))
              (pos (spofy-ui--find-track-position id)))
    (goto-char pos)
    (when (get-buffer-window (current-buffer))
      (recenter))
    t))

(defun spofy-ui--playing-track-position ()
  "Return (POSITION . TOTAL) for the playing track in this buffer.
POSITION is 1-based.  Return nil if the track is not here."
  (when-let* ((current-id (and (fboundp 'spofy-player-current-track-id)
                                (spofy-player-current-track-id)))
              (entries (bound-and-true-p tabulated-list-entries)))
    (let ((pos nil)
          (total (length entries))
          (idx 0))
      (dolist (entry entries)
        (when (and (not pos)
                   (car entry)
                   (string-match-p (regexp-quote current-id) (car entry)))
          (setq pos (1+ idx)))
        (cl-incf idx))
      (when pos (cons pos total)))))

(defun spofy-ui--find-buffer-with-track (track-id)
  "Return a Spofy buffer containing TRACK-ID, or nil."
  (cl-find-if
   (lambda (buf)
     (with-current-buffer buf
       (and spofy-ui--entry-formatter
            (bound-and-true-p tabulated-list-entries)
            (spofy-ui--find-track-position track-id))))
   (buffer-list)))

;;;; Buffer mode-line

(defconst spofy-ui-mode-line-format
  '(" "
    mode-name
    (:eval (spofy-ui--mode-line-entity-name))
    (:eval (spofy-ui--mode-line-info))
    "  "
    mode-line-misc-info
    mode-line-end-spaces)
  "Mode-line format for Spofy tabulated-list buffers.")

(defun spofy-ui--mode-line-entity-name ()
  "Return the entity name for the mode line, or empty string."
  (if spofy-ui--buffer-entity-name
      (concat ": " spofy-ui--buffer-entity-name)
    ""))

(defun spofy-ui--mode-line-info ()
  "Return mode-line string with playback state and track position."
  (let ((state-str (spofy-ui--mode-line-playback-state))
        (pos-str (spofy-ui--mode-line-track-position)))
    (cond
     ((and (string-empty-p state-str) (string-empty-p pos-str))
      "")
     ((string-empty-p pos-str)
      (concat "  " state-str))
     ((string-empty-p state-str)
      (concat "  " pos-str))
     (t (concat "  " state-str "  " pos-str)))))

(defun spofy-ui--mode-line-playback-state ()
  "Return a string with playback state icons.
In `spofy-dashboard-mode' buffers, return an empty string because
the dashboard already displays shuffle and repeat state in its
content."
  (if (or (derived-mode-p 'spofy-dashboard-mode)
          (not (and (boundp 'spofy-player--current-state)
                    spofy-player--current-state)))
      ""
    (string-join
     (seq-remove #'string-empty-p
                 (list (spofy-ui-shuffle-indicator spofy-player--current-state)
                       (spofy-ui-repeat-indicator spofy-player--current-state)))
     " ")))

(defun spofy-ui--mode-line-track-position ()
  "Return a string like \"47/213\" for the mode-line."
  (if-let* ((pos-info (spofy-ui--playing-track-position)))
      (propertize (format "%d/%d" (car pos-info) (cdr pos-info))
                  'face 'spofy-muted)
    ""))

(defun spofy-ui--update-mode-lines ()
  "Force mode-line update in all Spofy buffers."
  (dolist (buf (buffer-list))
    (when (buffer-local-value 'spofy-ui--format-view buf)
      (with-current-buffer buf
        (force-mode-line-update)))))

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

;;;; Track highlight auto-refresh

(defun spofy-ui--refresh-track-highlights ()
  "Re-render playing indicators in all Spofy list buffers.
Intended for `spofy-player-track-changed-hook'.  Saves and
restores window points around the reprint, since
`tabulated-list-print' erases the buffer and invalidates
window-point markers."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (and spofy-ui--entry-formatter
                   spofy-ui--entities
                   tabulated-list-entries)
          (let ((saved (spofy-ui--save-window-entries buf)))
            (spofy-ui--reformat-entries)
            (tabulated-list-print t)
            (spofy-ui--restore-window-entries buf saved))))))
  (spofy-ui--update-mode-lines))

(defun spofy-ui--save-window-entries (buf)
  "Return an alist of (WINDOW . ENTRY-ID) for windows showing BUF."
  (mapcar (lambda (win)
            (cons win (save-excursion
                        (goto-char (window-point win))
                        (tabulated-list-get-id))))
          (get-buffer-window-list buf nil t)))

(defun spofy-ui--restore-window-entries (buf saved)
  "Restore window points in BUF from SAVED (WINDOW . ENTRY-ID) pairs."
  (ignore buf)
  (dolist (entry saved)
    (let ((win (car entry))
          (id (cdr entry)))
      (when (and (window-live-p win) id)
        (when-let* ((pos (spofy-ui--find-track-position id)))
          (set-window-point win pos))))))

(defun spofy-ui--reformat-entries ()
  "Regenerate `tabulated-list-entries' using `spofy-ui--entry-formatter'."
  (let ((new-entries nil)
        (idx 0))
    (dolist (entry tabulated-list-entries)
      (let ((entity (gethash (car entry) spofy-ui--entities)))
        (push (if entity
                  (funcall spofy-ui--entry-formatter entity idx)
                entry)
              new-entries))
      (setq idx (1+ idx)))
    (setq tabulated-list-entries (nreverse new-entries))))

;;;; Cursor follows playback

;;;###autoload
(define-minor-mode spofy-cursor-follows-playback-mode
  "When enabled, scroll to the playing track on track changes.
Applies to all visible Spofy track-list windows.  Off by default."
  :global t
  :group 'spofy
  (if spofy-cursor-follows-playback-mode
      (add-hook 'spofy-player-track-changed-hook
                #'spofy-ui--follow-playing-track 10)
    (remove-hook 'spofy-player-track-changed-hook
                 #'spofy-ui--follow-playing-track)))

(defun spofy-ui--follow-playing-track ()
  "Scroll to the playing track in all visible Spofy windows."
  (when-let* ((current-id (and (fboundp 'spofy-player-current-track-id)
                                (spofy-player-current-track-id))))
    (dolist (window (window-list nil 'no-minibuf))
      (spofy-ui--maybe-follow-in-window window current-id))))

(defun spofy-ui--maybe-follow-in-window (window track-id)
  "In WINDOW, scroll to TRACK-ID if the buffer is a Spofy track list."
  (with-current-buffer (window-buffer window)
    (when (and spofy-ui--entry-formatter
               (bound-and-true-p tabulated-list-entries))
      (when-let* ((pos (spofy-ui--find-track-position track-id)))
        (set-window-point window pos)))))

(provide 'spofy-ui)
;;; spofy-ui.el ends here
