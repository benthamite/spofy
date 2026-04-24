;;; spofy-timeline.el --- Unified history/now-playing/queue view  -*- lexical-binding: t; -*-

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

;; `spofy-view-timeline' opens a single buffer that combines three
;; chronologically-ordered sections: recently played tracks at the top,
;; the currently playing track in the middle, and upcoming queue
;; entries at the bottom.  The buffer auto-refreshes on every track
;; change while it is visible, and recenters on the now-playing row.
;;
;; Row actions (RET, Q, a, A, s, g) work on history and queue rows as
;; well as on the now-playing row.  Playing a track from the timeline
;; preserves the current playback context when one is active.

;;; Code:

(require 'spofy-api)
(require 'spofy-browse)
(require 'spofy-ui)
(require 'cl-lib)

(defvar spofy-player--current-state)
(declare-function spofy-play-pause "spofy-player" ())
(declare-function spofy-play-track "spofy-player"
                  (uri &optional context-uri position))
(declare-function spofy-view-album "spofy-browse" (album-id))
(declare-function spofy-view-artist "spofy-browse" (artist-id))
(declare-function spofy-library-save "spofy-library" (uri-or-type &optional id))
(declare-function spofy-add-to-queue "spofy-queue" (uri))
(declare-function spofy-browse--cache-get "spofy-browse" (context-uri))
(declare-function spofy-browse--cache-put "spofy-browse" (context-uri items))
(declare-function spofy-browse--fetch-all-pages
                  "spofy-browse" (collected next-url callback))

;;;; User options

(defcustom spofy-timeline-history-limit 20
  "Number of past tracks to display in the timeline's history section."
  :type 'integer
  :group 'spofy)

(defconst spofy-timeline--buffer-name "*spofy Timeline*"
  "Name of the buffer that displays the unified timeline.")

(defconst spofy-timeline--section-titles
  '((":sep:history" . "Recently played")
    (":sep:now"     . "Now playing")
    (":sep:queue"   . "Up next"))
  "Titles for section-separator entry IDs.")

(defvar-local spofy-timeline--history-contexts nil
  "Hash mapping history-row URIs to their original play context URI.
Kept separate from `spofy-ui--entities' because the same URI often
appears in the queue section too, and the queue store would
overwrite any context annotation attached to the history entity.")

;;;; Mode

(defvar spofy-timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'spofy-timeline-play-at-point)
    (define-key map (kbd "a")   #'spofy-timeline-view-album-at-point)
    (define-key map (kbd "A")   #'spofy-timeline-view-artist-at-point)
    (define-key map (kbd "s")   #'spofy-timeline-save-at-point)
    (define-key map (kbd "Q")   #'spofy-timeline-queue-at-point)
    (define-key map (kbd "SPC") #'spofy-play-pause)
    (define-key map (kbd "g")   #'spofy-view-timeline)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `spofy-timeline-mode'.")

;; Install remaps outside the `defvar', which skips reinitialization on
;; reload, so the bindings reach a running Emacs when the file changes.
(define-key spofy-timeline-mode-map [remap next-line]
            #'spofy-timeline-next-line)
(define-key spofy-timeline-mode-map [remap previous-line]
            #'spofy-timeline-previous-line)

(define-derived-mode spofy-timeline-mode tabulated-list-mode "spofy Timeline"
  "Major mode for the combined history/now-playing/queue view."
  :group 'spofy
  (setq tabulated-list-padding 2)
  (spofy-ui-set-format
   'library-track
   '((" "         2 nil)
     ("Name"      :flex nil)
     ("Artist(s)" :flex nil)
     ("Album"     :flex nil)
     ("Duration"  6 nil :right-align t)))
  (setq-local spofy-ui--entity-type 'track)
  (setq-local spofy-ui--hide-header-line t)
  (setq-local tabulated-list-printer #'spofy-timeline--printer)
  (setq-local spofy-ui--entry-formatter #'spofy-timeline--reformat-entry)
  (tabulated-list-init-header)
  (setq header-line-format nil)
  (spofy-ui-enable-line-highlight))

;;;; Interactive commands

;;;###autoload
(defun spofy-view-timeline ()
  "Open the combined history / now-playing / queue timeline buffer."
  (interactive)
  (let ((buf (get-buffer-create spofy-timeline--buffer-name)))
    (with-current-buffer buf
      (spofy-timeline-mode))
    (spofy-timeline--fetch
     (lambda (history queue currently)
       (when (buffer-live-p buf)
         (spofy-timeline--render buf history queue currently))))
    (pop-to-buffer buf)))

(defcustom spofy-timeline-play-debounce 1.5
  "Seconds to ignore repeated `spofy-timeline-play-at-point' presses.
Spotify returns HTTP 403 \"Restriction violated\" when a new play
request arrives before the player has finished transitioning from
the previous one.  Swallowing presses inside this window keeps
rapid RET taps from stacking failed requests on top of a
just-issued successful play."
  :type 'number
  :group 'spofy)

(defvar spofy-timeline--last-play-time nil
  "Time of the most recent `spofy-timeline-play-at-point' call, or nil.")

(defun spofy-timeline-play-at-point ()
  "Play the track at point.
For \"Up next\" and \"Now playing\" rows, preserves the current
playback context so the remainder of the queue stays intact.
For history rows, replays the track within the same context it
was originally played in (captured from the recently-played
API), so hitting RET on history doesn't collapse the queue into
a single repeated URI.  Album and playlist contexts are resolved
to a track position first, since Spotify rejects `offset.uri'
with HTTP 403 whenever the track has been relinked.  Presses
within `spofy-timeline-play-debounce' seconds of a previous play
are ignored, since Spotify also rejects rapid successive play
requests with 403 while the previous one is still transitioning."
  (interactive)
  (if (spofy-timeline--play-debounced-p)
      (message "spofy: please wait before triggering another play")
    (when-let* ((entity (spofy-timeline--entity-at-point)))
      (setq spofy-timeline--last-play-time (current-time))
      (spofy-timeline--play-entity
       entity (spofy-timeline--play-context entity)))))

(defun spofy-timeline--play-debounced-p ()
  "Return non-nil when a play request should be swallowed as too soon."
  (and spofy-timeline--last-play-time
       (< (float-time (time-since spofy-timeline--last-play-time))
          spofy-timeline-play-debounce)))

(defun spofy-timeline--play-entity (entity context-uri)
  "Play ENTITY within CONTEXT-URI, resolving album/playlist positions first.
When CONTEXT-URI is nil, falls back to the track's own album so
Spotify always has a queue to draw from and we never issue a
bare URI play (which would scramble \"Up next\" with repeats)."
  (let* ((uri (alist-get 'uri entity))
         (resolved (or context-uri
                       (alist-get 'uri (alist-get 'album entity)))))
    (cond
     ((spofy-timeline--album-context-p resolved)
      (spofy-timeline--play-in-context entity resolved "albums"
                                       #'identity))
     ((spofy-timeline--playlist-context-p resolved)
      (spofy-timeline--play-in-context entity resolved "playlists"
                                       (lambda (item)
                                         (alist-get 'track item))))
     (resolved
      (spofy-play-track uri resolved))
     (t
      (spofy-play-track uri)))))

(defun spofy-timeline--album-context-p (context-uri)
  "Return non-nil when CONTEXT-URI is an album URI."
  (and context-uri (string-prefix-p "spotify:album:" context-uri)))

(defun spofy-timeline--playlist-context-p (context-uri)
  "Return non-nil when CONTEXT-URI is a playlist URI."
  (and context-uri (string-prefix-p "spotify:playlist:" context-uri)))

(defun spofy-timeline--play-in-context (entity context-uri endpoint track-fn)
  "Play ENTITY's URI within CONTEXT-URI, resolving 0-indexed position first.
ENDPOINT is the API subpath (`albums' or `playlists') whose
`.../tracks' route returns the context's items.  TRACK-FN extracts
the track alist from each item (identity for albums; `(alist-get
\\='track item)' for playlists).  Tries URI match first; falls back
to disc/track-number match for relinked URIs; passes nil position
(i.e. `offset.uri') when neither matches, so the play still issues
against the context instead of collapsing to a bare URI."
  (let ((uri (alist-get 'uri entity)))
    (spofy-timeline--fetch-context-tracks
     context-uri endpoint
     (lambda (tracks)
       (spofy-play-track
        uri context-uri
        (or (spofy-timeline--position-of uri tracks track-fn)
            (spofy-timeline--position-by-track-number
             entity tracks track-fn)))))))

(defun spofy-timeline--fetch-context-tracks (context-uri endpoint callback)
  "Fetch all tracks for CONTEXT-URI from ENDPOINT, then call CALLBACK with them.
Uses `spofy-browse' track cache to avoid refetching on repeated plays."
  (if-let* ((cached (spofy-browse--cache-get context-uri)))
      (funcall callback cached)
    (let* ((id (spofy-timeline--uri-id context-uri))
           (path (format "%s/%s/tracks" endpoint id)))
      (spofy-api-get
       path '(("limit" . "50"))
       (lambda (response)
         (let* ((first (append (alist-get 'items response) nil))
                (next (let ((n (alist-get 'next response)))
                        (and (stringp n) n))))
           (if next
               (spofy-browse--fetch-all-pages
                first next
                (lambda (all)
                  (spofy-browse--cache-put context-uri all)
                  (funcall callback all)))
             (spofy-browse--cache-put context-uri first)
             (funcall callback first))))))))

(defun spofy-timeline--uri-id (context-uri)
  "Return the ID part of CONTEXT-URI (everything after the last colon)."
  (substring context-uri (1+ (or (cl-position ?: context-uri :from-end t) -1))))

(defun spofy-timeline--position-by-track-number (entity tracks track-fn)
  "Return the 0-indexed position in TRACKS of the item matching ENTITY.
Compares `track_number' and `disc_number' rather than URI, so a
relinked track URI still resolves to the right album row.  TRACK-FN
extracts the track alist from each item in TRACKS.  Returns nil
when the entity lacks those numbers or no item matches."
  (let ((track-num (alist-get 'track_number entity))
        (disc-num (alist-get 'disc_number entity)))
    (when (and track-num disc-num)
      (cl-loop for item in tracks
               for index from 0
               for track = (funcall track-fn item)
               when (and track
                         (equal (alist-get 'track_number track) track-num)
                         (equal (alist-get 'disc_number track) disc-num))
               return index))))

(defun spofy-timeline--position-of (uri tracks track-fn)
  "Return the 0-indexed position of URI in TRACKS, or nil when absent.
TRACK-FN pulls the track alist out of each item in TRACKS."
  (cl-loop for item in tracks
           for index from 0
           for track = (funcall track-fn item)
           when (and track (equal (alist-get 'uri track) uri))
           return index))

(defun spofy-timeline--play-context (entity)
  "Return the context URI to play ENTITY in, based on its section."
  (pcase (spofy-timeline--section-at-point)
    ((or 'queue 'now)
     (alist-get 'context-uri spofy-player--current-state))
    ('history
     (and spofy-timeline--history-contexts
          (gethash (alist-get 'uri entity)
                   spofy-timeline--history-contexts)))))

(defun spofy-timeline-view-album-at-point ()
  "View the album for the track at point."
  (interactive)
  (when-let* ((entity (spofy-timeline--entity-at-point))
              (album (alist-get 'album entity))
              (album-id (alist-get 'id album)))
    (spofy-view-album album-id)))

(defun spofy-timeline-view-artist-at-point ()
  "View the first artist for the track at point."
  (interactive)
  (when-let* ((entity (spofy-timeline--entity-at-point))
              (artists (alist-get 'artists entity))
              ((> (length artists) 0))
              (artist (aref artists 0))
              (artist-id (alist-get 'id artist)))
    (spofy-view-artist artist-id)))

(defun spofy-timeline-save-at-point ()
  "Save the track at point to the user's library."
  (interactive)
  (when-let* ((entity (spofy-timeline--entity-at-point))
              (uri (alist-get 'uri entity)))
    (spofy-library-save uri)))

(defun spofy-timeline-queue-at-point ()
  "Add the track at point to the playback queue."
  (interactive)
  (when-let* ((entity (spofy-timeline--entity-at-point))
              (uri (alist-get 'uri entity)))
    (spofy-add-to-queue uri)))

;;;; Track-change hook

(defun spofy-timeline--refresh-if-visible ()
  "Silently refresh the timeline buffer if it is visible."
  (when-let* ((buf (get-buffer spofy-timeline--buffer-name))
              ((get-buffer-window buf 'visible)))
    (spofy-timeline--fetch
     (lambda (history queue currently)
       (when (buffer-live-p buf)
         (spofy-timeline--render buf history queue currently))))))

(add-hook 'spofy-player-track-changed-hook
          #'spofy-timeline--refresh-if-visible)

;;;; Helpers

(defun spofy-timeline--entity-at-point ()
  "Return the track alist on the current line, or nil."
  (spofy-ui-entity-at-point))

(defun spofy-timeline--section-at-point ()
  "Return the section symbol (`history', `now', `queue') for the row at point.
Returns nil when point is not under any section."
  (save-excursion
    (when (text-property-search-backward 'spofy-sep-section)
      (get-text-property (point) 'spofy-sep-section))))

(defun spofy-timeline--fetch (callback)
  "Fetch history and queue data, then call CALLBACK.
CALLBACK is called with three arguments: the history items
vector, the queue items vector, and the currently-playing track
alist (or nil)."
  (let ((history nil) (queue nil) (currently nil) (done 0))
    (cl-labels
        ((maybe-finish ()
           (setq done (1+ done))
           (when (= done 2)
             (funcall callback history queue currently))))
      (spofy-api-get
       "me/player/recently-played"
       `(("limit" . ,(number-to-string spofy-timeline-history-limit)))
       (lambda (response)
         (setq history (or (alist-get 'items response) []))
         (maybe-finish)))
      (spofy-api-get
       "me/player/queue" nil
       (lambda (response)
         (setq queue (or (alist-get 'queue response) []))
         (setq currently (alist-get 'currently_playing response))
         (maybe-finish))))))

(defun spofy-timeline--render (buf history queue currently)
  "Render HISTORY, CURRENTLY, and QUEUE into BUF."
  (with-current-buffer buf
    (unless (derived-mode-p 'spofy-timeline-mode)
      (spofy-timeline-mode))
    (setq tabulated-list-entries
          (spofy-timeline--build-entries history queue currently))
    (tabulated-list-print t)
    (spofy-timeline--goto-now-playing)))

(defun spofy-timeline--build-entries (history queue currently)
  "Return a list of tabulated-list entries from HISTORY, QUEUE, and CURRENTLY."
  (spofy-timeline--reset-history-contexts)
  (let ((entries (list (spofy-timeline--separator-entry ":sep:history"))))
    (dolist (item (spofy-timeline--history-items history))
      (let ((track (car item))
            (context-uri (cdr item)))
        (when-let* ((uri (alist-get 'uri track)))
          (puthash uri context-uri spofy-timeline--history-contexts))
        (push (spofy-timeline--format-track track 'history) entries)))
    (push (spofy-timeline--separator-entry ":sep:now") entries)
    (when currently
      (push (spofy-timeline--format-track currently 'now-playing) entries))
    (push (spofy-timeline--separator-entry ":sep:queue") entries)
    (cl-loop for track across queue
             do (push (spofy-timeline--format-track track 'queue) entries))
    (nreverse entries)))

(defun spofy-timeline--reset-history-contexts ()
  "Clear `spofy-timeline--history-contexts' before a fresh render."
  (unless spofy-timeline--history-contexts
    (setq-local spofy-timeline--history-contexts
                (make-hash-table :test #'equal)))
  (clrhash spofy-timeline--history-contexts))

(defun spofy-timeline--history-items (history)
  "Return HISTORY items as (TRACK . CONTEXT-URI) pairs, oldest first.
CONTEXT-URI is the URI of the context (album, playlist, etc.) the
track was originally played in, falling back to the track's album
URI when the play had no context.  A context is essential when
replaying: a bare URI play leaves Spotify with nothing to fill the
queue from, so it repeats the played track and visibly scrambles
\"Up next\"."
  (let ((pairs (cl-loop for item across history
                        for track = (alist-get 'track item)
                        for context = (alist-get 'context item)
                        for original-uri = (and (listp context)
                                                (alist-get 'uri context))
                        for album-uri = (alist-get 'uri
                                                   (alist-get 'album track))
                        for context-uri = (or original-uri album-uri)
                        when track
                        collect (cons track context-uri))))
    (nreverse pairs)))

(defun spofy-timeline--separator-entry (id)
  "Return a tabulated-list entry representing a section separator ID."
  (list id (vector "" "" "" "" "")))

(defun spofy-timeline--format-track (track section)
  "Format TRACK alist as a `tabulated-list-entries' entry.
SECTION is `history', `now-playing', or `queue' and fully
determines whether the row gets the now-playing face.  A track
that also appears in the history section is not highlighted just
because its URI happens to match the currently playing one — only
the authoritative `now-playing' row carries the bold styling."
  (let* ((uri (alist-get 'uri track))
         (name (or (alist-get 'name track) ""))
         (artists (or (alist-get 'artists track) []))
         (album (alist-get 'album track))
         (album-name (if album (or (alist-get 'name album) "") ""))
         (duration-ms (or (alist-get 'duration_ms track) 0))
         (artist-str (spofy-ui-format-artists artists))
         (duration-str (spofy-ui-format-duration-ms duration-ms))
         (playing (eq section 'now-playing))
         (indicator " "))
    (spofy-ui-store-entity uri track)
    (list uri
          (vector indicator
                  (spofy-ui-truncate
                   name (spofy-ui-col 'library-track 0)
                   (spofy-ui-playing-face 'spofy-track-name playing))
                  (spofy-ui-truncate
                   artist-str (spofy-ui-col 'library-track 1)
                   (spofy-ui-playing-face 'spofy-artist-name playing))
                  (spofy-ui-truncate
                   album-name (spofy-ui-col 'library-track 2)
                   (spofy-ui-playing-face 'spofy-album-name playing))
                  (propertize duration-str 'face
                              (spofy-ui-playing-face 'spofy-muted playing))))))

(defun spofy-timeline--reformat-entry (entity _idx)
  "Re-format ENTITY as a track row on track change.
Called before the async refresh has returned fresh data, so rows
are still in their old sections.  Highlight whichever row's URI
matches the just-updated current track; the full rerender via
`spofy-timeline--refresh-if-visible' follows shortly afterwards
and replaces this with the authoritative section layout."
  (let ((section (if (spofy-timeline--current-uri-p
                      (alist-get 'uri entity))
                     'now-playing
                   'queue)))
    (spofy-timeline--format-track entity section)))

(defun spofy-timeline--current-uri-p (uri)
  "Return non-nil when URI belongs to the currently playing track."
  (and uri (not (string-empty-p (spofy-ui-playing-indicator uri)))))

(defun spofy-timeline--printer (id cols)
  "Custom tabulated-list printer.
Render section separators specially; delegate track rows to
`tabulated-list-print-entry'.  ID is the row ID, COLS its columns."
  (if (spofy-timeline--separator-id-p id)
      (spofy-timeline--insert-separator id)
    (tabulated-list-print-entry id cols)))

(defun spofy-timeline--separator-id-p (id)
  "Return non-nil when ID is a section-separator entry ID."
  (and (stringp id) (string-prefix-p ":sep:" id)))

(defun spofy-timeline--insert-separator (id)
  "Insert a section separator block for the separator ID."
  (unless (bobp) (insert "\n"))
  (let* ((start (point))
         (title (alist-get id spofy-timeline--section-titles
                           nil nil #'equal))
         (pad (make-string (or tabulated-list-padding 0) ?\s)))
    (insert pad (propertize title 'face 'spofy-header) "\n\n")
    (put-text-property start (point) 'spofy-sep-section
                       (intern (substring id 5)))))

(defun spofy-timeline--goto-now-playing ()
  "Move point to the now-playing row and center it in every visible window.
Centering every window (not only the selected one) keeps the
current track anchored visually as the history and queue sections
grow or shrink around it, so the row never drifts into the
\"Up next\" area."
  (goto-char (point-min))
  (when-let* ((match (text-property-search-forward
                      'spofy-sep-section 'now t)))
    (let ((now-pt (prop-match-end match)))
      (goto-char now-pt)
      (dolist (win (get-buffer-window-list (current-buffer) nil 'visible))
        (spofy-timeline--recenter-window-on win now-pt)))))

(defun spofy-timeline--recenter-window-on (win pt)
  "Move WIN's point to PT and recenter WIN on that line."
  (set-window-point win pt)
  (with-selected-window win
    (recenter)))

;;;; Track-row navigation

(defun spofy-timeline-next-line (&optional n)
  "Move to the Nth next track row, skipping section separators.
N defaults to 1.  Negative N moves backwards.  Bound to remap
`next-line' so every binding that normally calls `next-line'
(C-n, <down>, etc.) also skips.  Non-motion commands, mouse
clicks, and scroll commands keep their usual behavior — if they
strand point on a separator, press the motion key once to skip
off."
  (interactive "^p")
  (spofy-ui-move-by-matching-lines #'spofy-timeline--track-row-p
                                   (or n 1)))

(defun spofy-timeline-previous-line (&optional n)
  "Move to the Nth previous track row, skipping section separators.
N defaults to 1.  Negative N moves forwards."
  (interactive "^p")
  (spofy-ui-move-by-matching-lines #'spofy-timeline--track-row-p
                                   (- (or n 1))))

(defun spofy-timeline--track-row-p ()
  "Return non-nil when the current row is a track, not a separator."
  (not (null (tabulated-list-get-id))))

(provide 'spofy-timeline)
;;; spofy-timeline.el ends here
