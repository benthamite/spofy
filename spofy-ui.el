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
    (playlist-track   . (30 20 20 15))
    (playlist-list    . (35 20)))
  "Column widths for tabulated-list buffers.
Each entry is (VIEW . (COL1 COL2 ...)).  Only variable-width columns
are listed; fixed-width columns (indicators, duration, year, counts)
are not configurable.  The columns per view are:

  library-track:    Name, Artist(s), Album
  library-album:    Name, Artist(s)
  search-track:     Name, Artist(s), Album
  search-album:     Name, Artist(s)
  search-artist:    Name, Genres
  search-playlist:  Name, Owner
  album-track:      Name, Artist(s)
  artist-album:     Name
  artist-top-track: Name, Album
  playlist-track:   Name, Artist(s), Album, Added By
  playlist-list:    Name, Owner"
  :type '(alist :key-type symbol
                :value-type (repeat integer))
  :group 'spofy)

(defun spofy-ui-col (view n)
  "Return the Nth column width for VIEW from `spofy-columns'."
  (nth n (alist-get view spofy-columns)))

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
face's foreground blended toward the default background.  Levels
1, 2, 3 blend at 25%, 50%, 75% respectively."
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


(defun spofy-ui-progress-bar (progress-ms duration-ms width)
  "Build a text progress bar for PROGRESS-MS out of DURATION-MS.
WIDTH is the total character width of the bar.  Uses Unicode block
characters for sub-character precision: each character position is
divided into 8 slices using U+2588..U+258F.

The time display (e.g. \"1:23 / 3:45\") is appended after the bar."
  (let* ((progress (or progress-ms 0))
         (duration (max 1 (or duration-ms 1)))
         (fraction (min 1.0 (/ (float progress) duration)))
         (total-eighths (round (* fraction width 8)))
         (full (/ total-eighths 8))
         (remainder (% total-eighths 8))
         (empty (- width full (if (> remainder 0) 1 0)))
         ;; U+2588 (full) down to U+258F (1/8), indexed 0..7
         (partial-chars [?\u2588 ?\u2589 ?\u258A ?\u258B ?\u258C ?\u258D ?\u258E ?\u258F])
         (filled-str (propertize (make-string (max 0 full) ?\u2588)
                                 'face 'spofy-progress-filled))
         (partial-str (if (> remainder 0)
                          (propertize (string (aref partial-chars (- 8 remainder)))
                                     'face 'spofy-progress-filled)
                        ""))
         (empty-str (propertize (make-string (max 0 empty) ?\u2591)
                                'face 'spofy-progress-empty))
         (progress-str (spofy-ui-format-duration-ms progress))
         (duration-str (spofy-ui-format-duration-ms duration)))
    (format "%s%s%s %s / %s"
            filled-str partial-str empty-str
            progress-str duration-str)))

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

(defun spofy-ui-insert-pagination-footer ()
  "Insert a pagination hint at the end of the current buffer.
When `spofy-ui--next-page-url' is non-nil, append a line telling the
user they can press \\`m' to load more results."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-max))
      ;; Remove any existing footer first.
      (when (re-search-backward "^\\[m\\] Load more" nil t)
        (delete-region (line-beginning-position) (point-max)))
      (when spofy-ui--next-page-url
        (goto-char (point-max))
        (insert (propertize "\n[m] Load more" 'face 'spofy-muted))))))

(defun spofy-ui--after-tabulated-list-print (&rest _)
  "Post-process Spofy tabulated-list buffers after printing.
Insert the pagination footer and apply truncation fade overlays.
Added as :after advice on `tabulated-list-print'."
  (when (local-variable-p 'spofy-ui--next-page-url)
    (spofy-ui-insert-pagination-footer))
  (when (string-prefix-p "*Spofy" (buffer-name))
    (spofy-ui--apply-buffer-fades)))

(advice-add 'tabulated-list-print :after #'spofy-ui--after-tabulated-list-print)

(provide 'spofy-ui)
;;; spofy-ui.el ends here
