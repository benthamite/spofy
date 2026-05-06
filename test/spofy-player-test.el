;;; spofy-player-test.el --- Tests for spofy-player  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;;; Commentary:

;; ERT tests for pure helpers in `spofy-player': player state
;; extraction, timestamp parsing, context URI resolution, and image
;; selection.

;;; Code:

(require 'ert)
(require 'spofy-player)

;;;; Timestamp parsing

(ert-deftest spofy-player-test-parse-timestamp-seconds ()
  "A plain seconds input returns milliseconds."
  (should (= 90000 (spofy-player--parse-timestamp "90")))
  (should (= 0 (spofy-player--parse-timestamp "0"))))

(ert-deftest spofy-player-test-parse-timestamp-m-ss ()
  "A M:SS input is parsed as minutes and seconds."
  (should (= 90000 (spofy-player--parse-timestamp "1:30")))
  (should (= 125000 (spofy-player--parse-timestamp "2:05"))))

(ert-deftest spofy-player-test-parse-timestamp-h-mm-ss ()
  "A H:MM:SS input is parsed as hours, minutes, and seconds."
  (should (= 5400000 (spofy-player--parse-timestamp "1:30:00")))
  (should (= 3723000 (spofy-player--parse-timestamp "1:02:03"))))

(ert-deftest spofy-player-test-parse-timestamp-trims-whitespace ()
  "Leading or trailing whitespace is ignored."
  (should (= 60000 (spofy-player--parse-timestamp "  1:00  "))))

(ert-deftest spofy-player-test-parse-timestamp-invalid-signals ()
  "An input with too many colons signals a user error."
  (should-error (spofy-player--parse-timestamp "1:2:3:4") :type 'user-error))

;;;; Best image URL

(ert-deftest spofy-player-test-best-image-url-closest-to-300 ()
  "The image whose height is closest to 300 pixels wins."
  (let ((images [((height . 64)  (url . "small.jpg"))
                 ((height . 300) (url . "medium.jpg"))
                 ((height . 640) (url . "large.jpg"))]))
    (should (equal "medium.jpg" (spofy-player--best-image-url images)))))

(ert-deftest spofy-player-test-best-image-url-empty ()
  "Empty or nil images return nil."
  (should-not (spofy-player--best-image-url []))
  (should-not (spofy-player--best-image-url nil)))

(ert-deftest spofy-player-test-best-image-url-missing-height ()
  "Missing height is treated as zero when selecting."
  (let ((images [((url . "no-height.jpg"))
                 ((height . 300) (url . "match.jpg"))]))
    (should (equal "match.jpg" (spofy-player--best-image-url images)))))

;;;; Context URI resolution

(ert-deftest spofy-player-test-resolve-context-uri-passthrough ()
  "When context is a non-album type, pass its URI through unchanged."
  (let ((context '((uri . "spotify:playlist:abc") (type . "playlist"))))
    (should (equal "spotify:playlist:abc"
                   (spofy-player--resolve-context-uri context "xyz")))))

(ert-deftest spofy-player-test-resolve-context-uri-matching-album ()
  "When context is an album matching the track's album, pass it through."
  (let ((context '((uri . "spotify:album:xyz") (type . "album"))))
    (should (equal "spotify:album:xyz"
                   (spofy-player--resolve-context-uri context "xyz")))))

(ert-deftest spofy-player-test-resolve-context-uri-autoplay-mismatch ()
  "When autoplay moved past the original album, return the track album URI."
  (let ((context '((uri . "spotify:album:original") (type . "album"))))
    (should (equal "spotify:album:current"
                   (spofy-player--resolve-context-uri context "current")))))

(ert-deftest spofy-player-test-resolve-context-uri-nil-context ()
  "A nil context URI returns nil."
  (should-not (spofy-player--resolve-context-uri nil "xyz"))
  (should-not (spofy-player--resolve-context-uri '((uri) (type . "album"))
                                                 "xyz")))

;;;; Player state extraction

(defun spofy-player-test--sample-data ()
  "Return a representative Spotify player state response."
  '((item . ((id . "track-id")
             (name . "Track Name")
             (duration_ms . 240000)
             (artists . [((id . "artist-id") (name . "Artist"))])
             (album . ((id . "album-id")
                       (name . "Album Name")
                       (release_date . "2024-05-01")
                       (images . [((height . 300) (url . "cover.jpg"))])))))
    (context . ((uri . "spotify:album:album-id") (type . "album")))
    (device . ((name . "Speaker") (volume_percent . 70)))
    (progress_ms . 60000)
    (is_playing . t)
    (shuffle_state . :false)
    (repeat_state . "off")))

(ert-deftest spofy-player-test-extract-state-nil-input ()
  "Nil input returns nil."
  (should-not (spofy-player--extract-state nil)))

(ert-deftest spofy-player-test-extract-state-track-fields ()
  "Track name, id, artist, and album come out of the state alist."
  (let ((state (spofy-player--extract-state (spofy-player-test--sample-data))))
    (should (equal "Track Name" (alist-get 'track state)))
    (should (equal "track-id" (alist-get 'track-id state)))
    (should (equal "Artist" (alist-get 'artist state)))
    (should (equal "artist-id" (alist-get 'artist-id state)))
    (should (equal "Album Name" (alist-get 'album state)))
    (should (equal "album-id" (alist-get 'album-id state)))
    (should (equal "cover.jpg" (alist-get 'album-image-url state)))))

(ert-deftest spofy-player-test-extract-state-progress-and-duration ()
  "Progress and duration are copied over."
  (let ((state (spofy-player--extract-state (spofy-player-test--sample-data))))
    (should (= 60000 (alist-get 'progress state)))
    (should (= 240000 (alist-get 'duration state)))))

(ert-deftest spofy-player-test-extract-state-playback-flags ()
  "Is-playing, shuffle, and repeat become canonical Lisp booleans."
  (let ((state (spofy-player--extract-state (spofy-player-test--sample-data))))
    (should (eq t (alist-get 'is-playing state)))
    (should-not (alist-get 'shuffle state))
    (should (equal "off" (alist-get 'repeat state)))))

(ert-deftest spofy-player-test-extract-state-device ()
  "Device name and volume are copied over."
  (let ((state (spofy-player--extract-state (spofy-player-test--sample-data))))
    (should (equal "Speaker" (alist-get 'device state)))
    (should (= 70 (alist-get 'volume state)))))

(ert-deftest spofy-player-test-extract-state-missing-item ()
  "A response without an `item' key yields nils for track fields."
  (let ((state (spofy-player--extract-state '((is_playing . :false)))))
    (should state)
    (should-not (alist-get 'track state))
    (should-not (alist-get 'track-id state))
    (should-not (alist-get 'album-id state))))

;;;; Device management

(ert-deftest spofy-player-test-poll-sync-preserves-state-during-rate-limit ()
  "Synchronous polling does not clear state while rate-limited."
  (let ((spofy-player--current-state '((track . "Track")
                                       (device . "Speaker"))))
    (cl-letf (((symbol-function 'spofy-api-get-sync)
               (lambda (&rest _) nil))
              ((symbol-function 'spofy-api-rate-limit-remaining)
               (lambda () 7)))
      (spofy-player--poll-sync)
      (should (equal spofy-player--current-state
                     '((track . "Track") (device . "Speaker")))))))

(ert-deftest spofy-player-test-ensure-device-stops-during-rate-limit ()
  "Device checks signal before polling while a cooldown is active."
  (let ((spofy-player--current-state nil)
        polled
        selected)
    (cl-letf (((symbol-function 'spofy-api-rate-limit-remaining)
               (lambda () 7))
              ((symbol-function 'spofy-player--poll-sync)
               (lambda () (setq polled t)))
              ((symbol-function 'spofy-select-device)
               (lambda () (setq selected t))))
      (let ((err (should-error (spofy-player--ensure-device)
                               :type 'user-error)))
        (should (string-match-p "rate limit exceeded"
                                (error-message-string err)))
        (should (string-match-p "7 seconds"
                                (error-message-string err))))
      (should-not polled)
      (should-not selected))))

(ert-deftest spofy-player-test-select-device-signals-on-empty-device-list ()
  "An empty device array reports that no devices are available."
  (cl-letf (((symbol-function 'spofy-api-get-sync-or-error)
             (lambda (&rest _) '((devices . [])))))
    (let ((err (should-error (spofy-select-device) :type 'user-error)))
      (should (string= "spofy: No devices found"
                       (error-message-string err))))))

(ert-deftest spofy-player-test-select-device-transfers-selected-device ()
  "Selecting a device transfers playback to its ID."
  (let (request)
    (cl-letf (((symbol-function 'spofy-api-get-sync-or-error)
               (lambda (&rest _)
                 '((devices . [((id . "device-1")
                                (name . "Speaker"))]))))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (should (equal '("Speaker") collection))
                 "Speaker"))
              ((symbol-function 'spofy-api-put)
               (lambda (endpoint data &optional callback)
                 (setq request (list endpoint data))
                 (when callback
                   (funcall callback nil)))))
      (spofy-select-device)
      (should (equal "me/player" (car request)))
      (should (equal '((device_ids . ["device-1"]) (play . t))
                     (cadr request))))))

(provide 'spofy-player-test)
;;; spofy-player-test.el ends here
