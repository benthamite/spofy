;;; spofy-player.el --- Playback control for Spofy  -*- lexical-binding: t; -*-

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

;; Playback control, polling, device management, and volume control for
;; the Spofy Spotify client.  All commands communicate with the Spotify
;; Web API via `spofy-api.el'.

;;; Code:

(require 'spofy-api)
(require 'spofy-ui)
(require 'cl-lib)

(declare-function spofy-view-album "spofy-browse" (album-id))
(declare-function spofy-view-playlist "spofy-browse" (playlist-id))

;;;; Customization

(defcustom spofy-poll-interval 5
  "Seconds between player state polls."
  :type 'number
  :group 'spofy)

(defcustom spofy-seek-seconds 10
  "Number of seconds to seek forward or backward."
  :type 'integer
  :group 'spofy)

(defcustom spofy-volume-step 5
  "Volume increment/decrement step (0-100)."
  :type 'integer
  :group 'spofy)


;;;; Hooks

(defvar spofy-player-state-changed-hook nil
  "Hook run when any player state changes (track, play/pause, etc.).")

(defvar spofy-player-track-changed-hook nil
  "Hook run when the current track changes.")

;;;; Internal state

(defvar spofy-player--current-state nil
  "Alist holding the last polled player state.
Keys: track, artist, artist-id, artists, album, album-id, album-date,
album-image-url, progress, duration, is-playing, shuffle, repeat,
device, volume, track-id, context-uri, context-type.")

(defvar spofy-player--timer nil
  "Timer object for player state polling.")

(defvar spofy-player--volume-set-time nil
  "Timestamp of the last optimistic volume update, or nil.")

(defvar spofy-player--poll-generation 0
  "Generation counter for the current polling loop.
Incremented on each `spofy-player-start-polling' call so that
stale in-flight callbacks do not reschedule a stopped loop.")

;;;; State extraction

(defun spofy-player--best-image-url (images)
  "Return the URL of the image closest to 300px from IMAGES.
IMAGES is the `images' array from a Spotify album object."
  (when (and images (> (length images) 0))
    (let ((best nil)
          (best-diff most-positive-fixnum))
      (seq-doseq (img images)
        (let* ((h (or (alist-get 'height img) 0))
               (diff (abs (- h 300))))
          (when (< diff best-diff)
            (setq best img best-diff diff))))
      (alist-get 'url best))))

(defun spofy-player--extract-state (data)
  "Extract a normalized state alist from the Spotify player DATA response."
  (when data
    (let* ((item (let ((i (alist-get 'item data)))
                   (and (listp i) i)))
           (artists (alist-get 'artists item))
           (album (alist-get 'album item))
           (device (alist-get 'device data))
           (context (let ((c (alist-get 'context data)))
                      (and (listp c) c)))
           (album-id (alist-get 'id album))
           (context-uri (spofy-player--resolve-context-uri context album-id))
           (context-type (alist-get 'type context)))
      `((track     . ,(alist-get 'name item))
        (artist    . ,(when (and artists (> (length artists) 0))
                       (spofy-ui-format-artists artists)))
        (artist-id . ,(when (and artists (> (length artists) 0))
                       (alist-get 'id (aref artists 0))))
        (artists   . ,(when (and artists (> (length artists) 0))
                       (mapcar (lambda (a)
                                 (cons (alist-get 'name a)
                                       (alist-get 'id a)))
                               (append artists nil))))
        (album     . ,(alist-get 'name album))
        (album-date . ,(alist-get 'release_date album))
        (album-id  . ,album-id)
        (album-image-url . ,(spofy-player--best-image-url
                              (alist-get 'images album)))
        (progress  . ,(alist-get 'progress_ms data))
        (duration  . ,(alist-get 'duration_ms item))
        (is-playing . ,(eq (alist-get 'is_playing data) t))
        (shuffle   . ,(eq (alist-get 'shuffle_state data) t))
        (repeat    . ,(alist-get 'repeat_state data))
        (device    . ,(alist-get 'name device))
        (volume    . ,(alist-get 'volume_percent device))
        (track-id  . ,(alist-get 'id item))
        (context-uri  . ,context-uri)
        (context-type . ,context-type)))))

(defun spofy-player--resolve-context-uri (context album-id)
  "Return the correct context URI from CONTEXT.
When the context is an album whose ID does not match ALBUM-ID,
Spotify autoplay has moved past the original album; return the
track's actual album URI instead."
  (let ((uri (alist-get 'uri context)))
    (if (and uri
             (equal (alist-get 'type context) "album")
             album-id
             (not (equal (car (last (split-string uri ":")))
                         album-id)))
        (concat "spotify:album:" album-id)
      uri)))

(defun spofy-player--update-state (key value)
  "Set KEY to VALUE in the current player state and run hooks."
  (when spofy-player--current-state
    (setf (alist-get key spofy-player--current-state) value)
    (run-hooks 'spofy-player-state-changed-hook)))

;;;; Polling

(defun spofy-player--poll ()
  "Poll the Spotify player state and reschedule when done.
Uses a one-shot timer that only reschedules after the response
callback completes, so requests never pile up under network
latency."
  (let ((gen spofy-player--poll-generation))
    (condition-case err
        (spofy-api-get "me/player" (spofy-api-with-market nil)
                       (lambda (data)
                         (unwind-protect
                             (spofy-player--handle-poll-response data)
                           (spofy-player--reschedule-poll gen))))
      (error
       (message "Spofy: poll error: %S" err)
       (spofy-player--reschedule-poll gen)))))

(defun spofy-player--handle-poll-response (data)
  "Handle the poll response DATA.
Compare to previous state and run appropriate hooks."
  (let ((new-state (spofy-player--extract-state data)))
    (if new-state
        (progn
          ;; Record the wall-clock time of this poll for progress interpolation
          (setf (alist-get 'poll-time new-state) (float-time))
          ;; Preserve optimistic volume during the grace period after a
          ;; user-initiated volume change, so the poll doesn't overwrite it
          ;; with a stale value from Spotify.
          (when spofy-player--volume-set-time
            (if (< (- (float-time) spofy-player--volume-set-time)
                   spofy-poll-interval)
                (when spofy-player--current-state
                  (setf (alist-get 'volume new-state)
                        (alist-get 'volume spofy-player--current-state)))
              (setq spofy-player--volume-set-time nil)))
          (let ((old-state spofy-player--current-state))
            (setq spofy-player--current-state new-state)
            (unless (equal old-state new-state)
              (run-hooks 'spofy-player-state-changed-hook)
              (unless (equal (alist-get 'track-id old-state)
                             (alist-get 'track-id new-state))
                (run-hooks 'spofy-player-track-changed-hook)))))
      ;; No active player session (API returned 204) — clear stale state
      (when spofy-player--current-state
        (setq spofy-player--current-state nil)
        (run-hooks 'spofy-player-state-changed-hook)))))

(defun spofy-player--reschedule-poll (generation)
  "Schedule the next poll after `spofy-poll-interval' seconds.
Only reschedules if GENERATION matches the current loop and
polling has not been stopped."
  (when (and spofy-player--timer
             (= generation spofy-player--poll-generation))
    (setq spofy-player--timer
          (run-with-timer spofy-poll-interval nil #'spofy-player--poll))))

(defun spofy-player--poll-sync ()
  "Synchronously fetch and update the player state."
  (let ((data (spofy-api-get-sync "me/player" (spofy-api-with-market nil))))
    (spofy-player--handle-poll-response data)))

;;;###autoload
(defun spofy-player-start-polling ()
  "Start polling the Spotify player state.
Polls at `spofy-poll-interval' second intervals."
  (interactive)
  (spofy-player-stop-polling)
  (cl-incf spofy-player--poll-generation)
  (setq spofy-player--timer
        (run-with-timer 0 nil #'spofy-player--poll)))

;;;###autoload
(defun spofy-player-stop-polling ()
  "Stop polling the Spotify player state."
  (interactive)
  (when spofy-player--timer
    (cancel-timer spofy-player--timer)
    (setq spofy-player--timer nil)))

;;;; Helper accessors

(defun spofy-player-current-track ()
  "Return the current track as (NAME . ARTIST), or nil."
  (when spofy-player--current-state
    (cons (alist-get 'track spofy-player--current-state)
          (alist-get 'artist spofy-player--current-state))))

(defun spofy-player-playing-p ()
  "Return non-nil if Spotify is currently playing."
  (alist-get 'is-playing spofy-player--current-state))

(defun spofy-player-current-track-id ()
  "Return the Spotify track ID of the currently playing track, or nil."
  (alist-get 'track-id spofy-player--current-state))

(defun spofy-player-interpolated-progress ()
  "Return the estimated current progress in milliseconds.
Adds elapsed wall-clock time since the last poll if the track is playing."
  (when spofy-player--current-state
    (let ((progress (or (alist-get 'progress spofy-player--current-state) 0))
          (duration (or (alist-get 'duration spofy-player--current-state) 0))
          (poll-time (alist-get 'poll-time spofy-player--current-state))
          (playing (alist-get 'is-playing spofy-player--current-state)))
      (if (and playing poll-time)
          (let ((elapsed-ms (* (- (float-time) poll-time) 1000)))
            (min (round (+ progress elapsed-ms)) duration))
        progress))))

;;;; Device management

(defun spofy-player--ensure-device ()
  "Ensure an active playback device is available.
If no device is found, calls `spofy-select-device' so the user can
pick one.  Refreshes state once before prompting, since cold sessions
may have no cached state even when a device is already active."
  (unless (alist-get 'device spofy-player--current-state)
    (spofy-player--poll-sync))
  (unless (alist-get 'device spofy-player--current-state)
    (spofy-select-device)))

;;;###autoload
(defun spofy-select-device ()
  "List available Spotify devices and transfer playback to the selected one."
  (interactive)
  (let* ((data (spofy-api-get-sync "me/player/devices"))
         (devices (and data (alist-get 'devices data)))
         (names (mapcar (lambda (d) (alist-get 'name d)) devices)))
    (unless devices
      (user-error "Spofy: no devices found"))
    (let ((choice (completing-read "Spofy device: " names nil t)))
      (when-let* ((device (cl-find choice devices
                                   :key (lambda (d) (alist-get 'name d))
                                   :test #'equal))
                  (device-id (alist-get 'id device)))
        (spofy-api-put "me/player"
                       `((device_ids . [,device-id])
                         (play . t))
                       (lambda (_)
                         (message "Spofy: transferred playback to %s" choice)))))))

;;;; Playback commands

;;;###autoload
(defun spofy-play-pause ()
  "Toggle play/pause."
  (interactive)
  (spofy-player--ensure-device)
  (unless spofy-player--current-state
    (spofy-player--poll-sync))
  (let ((pausing (spofy-player-playing-p)))
    (if pausing
        ;; Snapshot interpolated progress so display doesn't jump backwards
        (progn
          (setf (alist-get 'progress spofy-player--current-state)
                (spofy-player-interpolated-progress))
          (spofy-player--update-state 'is-playing nil)
          (spofy-api-put "me/player/pause" nil
                         (lambda (_) (message "Spofy: paused"))))
      ;; Reset poll-time so interpolation starts fresh from stored progress
      (setf (alist-get 'poll-time spofy-player--current-state) (float-time))
      (spofy-player--update-state 'is-playing t)
      (spofy-api-put "me/player/play" nil
                     (lambda (_)
                       (message "Spofy: playing")
                       ;; Re-poll to pick up any track change Spotify made on resume
                       (spofy-player--poll))))))

;;;###autoload
(defun spofy-next ()
  "Skip to the next track."
  (interactive)
  (spofy-player--ensure-device)
  (let ((was-playing (spofy-player-playing-p)))
    (spofy-api-post "me/player/next" nil
                    (lambda (_)
                      (message "Spofy: next track")
                      (spofy-player--poll-after-skip was-playing)))))

;;;###autoload
(defun spofy-previous ()
  "Skip to the previous track."
  (interactive)
  (spofy-player--ensure-device)
  (let ((was-playing (spofy-player-playing-p)))
    (spofy-api-post "me/player/previous" nil
                    (lambda (_)
                      (message "Spofy: previous track")
                      (spofy-player--poll-after-skip was-playing)))))

(defun spofy-player--poll-after-skip (was-playing)
  "Poll player state after a skip command.
Delay slightly so Spotify has time to transition to the new track.
If WAS-PLAYING is non-nil, preserve the playing state in case the
poll catches a brief transitional pause."
  (run-with-timer
   0.5 nil
   (lambda ()
     (spofy-api-get
      "me/player" (spofy-api-with-market nil)
      (lambda (data)
        (when (and was-playing data)
          ;; Ensure Spotify's transitional pause doesn't stop our playback
          (setf (alist-get 'is_playing data) t))
        (spofy-player--handle-poll-response data))))))

;;;###autoload
(defun spofy-seek-forward ()
  "Seek forward by `spofy-seek-seconds' seconds."
  (interactive)
  (spofy-player--ensure-device)
  (let* ((progress (or (spofy-player-interpolated-progress) 0))
         (duration (or (alist-get 'duration spofy-player--current-state) 0))
         (new-pos (min duration (+ progress (* spofy-seek-seconds 1000)))))
    (spofy-api-put (format "me/player/seek?position_ms=%d" new-pos) nil
                   (lambda (_) (message "Spofy: seeked forward %ds" spofy-seek-seconds)))))

;;;###autoload
(defun spofy-seek-backward ()
  "Seek backward by `spofy-seek-seconds' seconds."
  (interactive)
  (spofy-player--ensure-device)
  (let* ((progress (or (spofy-player-interpolated-progress) 0))
         (new-pos (max 0 (- progress (* spofy-seek-seconds 1000)))))
    (spofy-api-put (format "me/player/seek?position_ms=%d" new-pos) nil
                   (lambda (_) (message "Spofy: seeked backward %ds" spofy-seek-seconds)))))

;;;###autoload
(defun spofy-volume-up ()
  "Increase volume by `spofy-volume-step' percent."
  (interactive)
  (spofy-player--ensure-device)
  (let* ((current (or (alist-get 'volume spofy-player--current-state) 50))
         (new-vol (min 100 (+ current spofy-volume-step))))
    (spofy-player--update-state 'volume new-vol)
    (setq spofy-player--volume-set-time (float-time))
    (spofy-api-put (format "me/player/volume?volume_percent=%d" new-vol) nil
                   (lambda (_) (message "Spofy: volume %d%%" new-vol)))))

;;;###autoload
(defun spofy-volume-down ()
  "Decrease volume by `spofy-volume-step' percent."
  (interactive)
  (spofy-player--ensure-device)
  (let* ((current (or (alist-get 'volume spofy-player--current-state) 50))
         (new-vol (max 0 (- current spofy-volume-step))))
    (spofy-player--update-state 'volume new-vol)
    (setq spofy-player--volume-set-time (float-time))
    (spofy-api-put (format "me/player/volume?volume_percent=%d" new-vol) nil
                   (lambda (_) (message "Spofy: volume %d%%" new-vol)))))

;;;###autoload
(defun spofy-volume-set (volume)
  "Set playback volume to VOLUME (0-100)."
  (interactive "nVolume (0-100): ")
  (spofy-player--ensure-device)
  (let ((vol (max 0 (min 100 volume))))
    (spofy-player--update-state 'volume vol)
    (setq spofy-player--volume-set-time (float-time))
    (spofy-api-put (format "me/player/volume?volume_percent=%d" vol) nil
                   (lambda (_) (message "Spofy: volume set to %d%%" vol)))))

;;;###autoload
(defun spofy-toggle-shuffle ()
  "Toggle shuffle mode."
  (interactive)
  (spofy-player--ensure-device)
  (let ((new-state (not (alist-get 'shuffle spofy-player--current-state))))
    (spofy-player--update-state 'shuffle new-state)
    (spofy-api-put (format "me/player/shuffle?state=%s"
                           (if new-state "true" "false"))
                   nil
                   (lambda (_)
                     (message "Spofy: shuffle %s" (if new-state "on" "off"))))))

;;;###autoload
(defun spofy-toggle-repeat ()
  "Cycle repeat mode: off -> context -> track."
  (interactive)
  (spofy-player--ensure-device)
  (let* ((current (or (alist-get 'repeat spofy-player--current-state) "off"))
         (next (pcase current
                 ("off"     "context")
                 ("context" "track")
                 ("track"   "off")
                 (_         "off"))))
    (spofy-player--update-state 'repeat next)
    (spofy-api-put (format "me/player/repeat?state=%s" next) nil
                   (lambda (_)
                     (message "Spofy: repeat %s" next)))))

;;;; Playing context

;;;###autoload
(defun spofy-play-track (uri &optional context-uri position)
  "Play the track at URI.
If CONTEXT-URI is non-nil, play within that context (album, playlist,
etc.) starting at the given track.  POSITION, when an integer, is the
0-indexed offset of URI within the context; it is preferred over the
URI-based offset because Spotify rejects URI offsets for tracks that
have been relinked (common in compilation albums)."
  (interactive "sSpotify track URI: ")
  (spofy-player--ensure-device)
  (spofy-api-put "me/player/play"
                 (spofy-player--play-body uri context-uri position)
                 (lambda (_)
                   (message "Spofy: playing track")
                   (spofy-player--poll-after-skip t))))

(defun spofy-player--play-body (uri context-uri position)
  "Return the JSON body for a play request of URI.
CONTEXT-URI, when non-nil, frames playback within an album or playlist
context.  POSITION, when an integer, uses \"offset.position\" instead of
\"offset.uri\" for reliable starting-track selection."
  (cond
   ((and context-uri (integerp position))
    `((context_uri . ,context-uri)
      (offset . ((position . ,position)))))
   (context-uri
    `((context_uri . ,context-uri)
      (offset . ((uri . ,uri)))))
   (t `((uris . [,uri])))))

;;;###autoload
(defun spofy-play-context (context-uri)
  "Play the context at CONTEXT-URI (album, playlist, or artist)."
  (interactive "sSpotify context URI: ")
  (spofy-player--ensure-device)
  (spofy-api-put "me/player/play"
                 `((context_uri . ,context-uri))
                 (lambda (_)
                   (message "Spofy: playing context")
                   (spofy-player--poll-after-skip t))))

;;;; Seek to timestamp

(defun spofy-player--parse-timestamp (input)
  "Parse INPUT as a timestamp and return milliseconds.
Accepts seconds (\"90\"), M:SS (\"1:30\"), or H:MM:SS (\"1:30:00\")."
  (let ((parts (split-string (string-trim input) ":")))
    (pcase (length parts)
      (1 (* (truncate (string-to-number (nth 0 parts))) 1000))
      (2 (* (+ (* (string-to-number (nth 0 parts)) 60)
               (string-to-number (nth 1 parts)))
            1000))
      (3 (* (+ (* (string-to-number (nth 0 parts)) 3600)
               (* (string-to-number (nth 1 parts)) 60)
               (string-to-number (nth 2 parts)))
            1000))
      (_ (user-error "Spofy: invalid timestamp format: %s" input)))))

;;;###autoload
(defun spofy-seek-to (timestamp)
  "Seek to TIMESTAMP in the current track.
TIMESTAMP is a string in M:SS, H:MM:SS, or plain seconds format."
  (interactive
   (list (let ((duration (or (alist-get 'duration spofy-player--current-state) 0)))
           (read-string (format "Seek to (max %s): "
                                (spofy-ui-format-duration-ms duration))))))
  (spofy-player--ensure-device)
  (let* ((pos-ms (spofy-player--parse-timestamp timestamp))
         (duration (or (alist-get 'duration spofy-player--current-state) 0))
         (clamped (max 0 (min duration pos-ms))))
    (spofy-player--update-state 'progress clamped)
    (setf (alist-get 'poll-time spofy-player--current-state) (float-time))
    (spofy-api-put (format "me/player/seek?position_ms=%d" clamped) nil
                   (lambda (_)
                     (message "Spofy: seeked to %s"
                              (spofy-ui-format-duration-ms clamped))))))

(provide 'spofy-player)
;;; spofy-player.el ends here
