;;; spofy-integration-test.el --- Regression tests for Spofy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;;; Commentary:

;; Focused regression tests for command wiring and optional integrations.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'spofy-browse)
(require 'spofy-consult)
(require 'spofy-library)
(require 'spofy-player)
(require 'spofy-playlist)
(require 'spofy-search)

(ert-deftest spofy-library-test-save-accepts-legacy-type-and-id ()
  "Library save accepts the older TYPE + ID calling convention."
  (let (captured-endpoint captured-data)
    (cl-letf (((symbol-function 'spofy-api-put)
               (lambda (endpoint data &optional _callback)
                 (setq captured-endpoint endpoint
                       captured-data data))))
      (spofy-library-save "albums" "album-123"))
    (should (equal captured-endpoint "me/albums"))
    (should (equal captured-data '((ids . ["album-123"]))))))

(ert-deftest spofy-library-test-save-accepts-uri ()
  "Library save still accepts a full Spotify URI."
  (let (captured-endpoint captured-data)
    (cl-letf (((symbol-function 'spofy-api-put)
               (lambda (endpoint data &optional _callback)
                 (setq captured-endpoint endpoint
                       captured-data data))))
      (spofy-library-save "spotify:track:track-123"))
    (should (equal captured-endpoint "me/tracks"))
    (should (equal captured-data '((ids . ["track-123"]))))))

(ert-deftest spofy-playlist-test-remove-track-accepts-explicit-args ()
  "Playlist track removal accepts explicit playlist and track identifiers."
  (let (captured-endpoint captured-data)
    (cl-letf (((symbol-function 'spofy-api-delete)
               (lambda (endpoint data &optional _callback)
                 (setq captured-endpoint endpoint
                       captured-data data))))
      (spofy-playlist-remove-track "playlist-123" "spotify:track:track-123"))
    (should (equal captured-endpoint "playlists/playlist-123/tracks"))
    (should (equal captured-data
                   '((tracks . [((uri . "spotify:track:track-123"))]))))))

(ert-deftest spofy-browse-test-album-save-uses-album-uri ()
  "Album save uses a single album URI for the library command."
  (let ((spofy-ui--buffer-context '((album-uri . "spotify:album:album-123")
                                    (album-id . "album-123")))
        captured-args)
    (cl-letf (((symbol-function 'spofy-library-save)
               (lambda (&rest args)
                 (setq captured-args args))))
      (spofy-album-save))
    (should (equal captured-args '("spotify:album:album-123")))))

(ert-deftest spofy-search-test-missing-consult-signals-user-error ()
  "Search commands fail cleanly when Consult is unavailable."
  (cl-letf (((symbol-function 'spofy-search--consult-available-p)
             (lambda () nil)))
    (should-error (spofy-search-tracks nil) :type 'user-error)))

(ert-deftest spofy-consult-test-missing-consult-signals-user-error ()
  "Consult commands fail cleanly when Consult is unavailable."
  (cl-letf (((symbol-function 'spofy-consult--available-p)
             (lambda () nil)))
    (should-error (consult-spofy-track) :type 'user-error)))

(ert-deftest spofy-player-test-ensure-device-refreshes-on-empty-state ()
  "Device checks refresh the player state before failing."
  (let ((spofy-player--current-state nil)
        (spofy-no-device-action 'prompt)
        polled)
    (cl-letf (((symbol-function 'spofy-player--poll-sync)
               (lambda ()
                 (setq polled t
                       spofy-player--current-state '((device . "MacBook"))))))
      (spofy-player--ensure-device))
    (should polled)
    (should (equal (alist-get 'device spofy-player--current-state) "MacBook"))))

(provide 'spofy-integration-test)
;;; spofy-integration-test.el ends here
