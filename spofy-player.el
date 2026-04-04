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
Keys: track, artist, album, progress, is-playing, shuffle, repeat,
device, volume, track-id.")

(defvar spofy-player--timer nil
  "Timer object for player state polling.")

(defvar spofy-player--volume-set-time nil
  "Timestamp of the last optimistic volume update, or nil.")

;;;; State extraction

(defun spofy-player--extract-state (data)
  "Extract a normalized state alist from the Spotify player DATA response."
  (when data
    (let* ((item (alist-get 'item data))
           (artists (alist-get 'artists item))
           (album (alist-get 'album item))
           (device (alist-get 'device data)))
      `((track     . ,(alist-get 'name item))
        (artist    . ,(when (and artists (> (length artists) 0))
                       (spofy-ui-format-artists artists)))
        (artist-id . ,(when (and artists (> (length artists) 0))
                       (alist-get 'id (aref artists 0))))
        (album     . ,(alist-get 'name album))
        (album-id  . ,(alist-get 'id album))
        (progress  . ,(alist-get 'progress_ms data))
        (duration  . ,(alist-get 'duration_ms item))
        (is-playing . ,(eq (alist-get 'is_playing data) t))
        (shuffle   . ,(eq (alist-get 'shuffle_state data) t))
        (repeat    . ,(alist-get 'repeat_state data))
        (device    . ,(alist-get 'name device))
        (volume    . ,(alist-get 'volume_percent device))
        (track-id  . ,(alist-get 'id item))))))

(defun spofy-player--update-state (key value)
  "Set KEY to VALUE in the current player state and run hooks."
  (when spofy-player--current-state
    (setf (alist-get key spofy-player--current-state) value)
    (run-hooks 'spofy-player-state-changed-hook)))

;;;; Polling

(defun spofy-player--poll ()
  "Poll the Spotify player state and run hooks on changes.
Errors are caught unconditionally so that `debug-on-error' does not
prevent the repeat timer from being rescheduled."
  (condition-case err
      (spofy-api-get "me/player" nil #'spofy-player--handle-poll-response)
    (error (message "Spofy: poll error: %S" err))))

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

(defun spofy-player--poll-sync ()
  "Synchronously fetch and update the player state."
  (let ((data (spofy-api-get-sync "me/player")))
    (spofy-player--handle-poll-response data)))

;;;###autoload
(defun spofy-player-start-polling ()
  "Start polling the Spotify player state.
Polls at `spofy-poll-interval' second intervals."
  (interactive)
  (spofy-player-stop-polling)
  (setq spofy-player--timer
        (run-with-timer 0 spofy-poll-interval #'spofy-player--poll)))

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
  (spofy-api-post "me/player/next" nil
                  (lambda (_) (message "Spofy: next track"))))

;;;###autoload
(defun spofy-previous ()
  "Skip to the previous track."
  (interactive)
  (spofy-player--ensure-device)
  (spofy-api-post "me/player/previous" nil
                  (lambda (_) (message "Spofy: previous track"))))

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
(defun spofy-play-track (uri &optional context-uri)
  "Play the track at URI.
If CONTEXT-URI is non-nil, play within that context (album, playlist,
etc.) starting at the given track."
  (interactive "sSpotify track URI: ")
  (spofy-player--ensure-device)
  (let ((body (if context-uri
                  `((context_uri . ,context-uri)
                    (offset . ((uri . ,uri))))
                `((uris . [,uri])))))
    (spofy-api-put "me/player/play" body
                   (lambda (_) (message "Spofy: playing track")))))

;;;###autoload
(defun spofy-play-context (context-uri)
  "Play the context at CONTEXT-URI (album, playlist, or artist)."
  (interactive "sSpotify context URI: ")
  (spofy-player--ensure-device)
  (spofy-api-put "me/player/play"
                 `((context_uri . ,context-uri))
                 (lambda (_) (message "Spofy: playing context"))))

(provide 'spofy-player)
;;; spofy-player.el ends here
