;;; spofy-queue-test.el --- Tests for spofy-queue  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;;; Commentary:

;; ERT tests for pure helpers in `spofy-queue': URI validation and
;; header formatting.

;;; Code:

(require 'ert)
(require 'spofy-queue)

;;;; Queueable URI predicate

(ert-deftest spofy-queue-test-queueable-uri-track ()
  "A track URI is queueable."
  (should (spofy-queue--queueable-uri-p "spotify:track:abc123")))

(ert-deftest spofy-queue-test-queueable-uri-episode ()
  "An episode URI is queueable."
  (should (spofy-queue--queueable-uri-p "spotify:episode:xyz")))

(ert-deftest spofy-queue-test-queueable-uri-album ()
  "An album URI is NOT queueable (only tracks and episodes are)."
  (should-not (spofy-queue--queueable-uri-p "spotify:album:xyz")))

(ert-deftest spofy-queue-test-queueable-uri-artist ()
  "An artist URI is NOT queueable."
  (should-not (spofy-queue--queueable-uri-p "spotify:artist:xyz")))

(ert-deftest spofy-queue-test-queueable-uri-playlist ()
  "A playlist URI is NOT queueable."
  (should-not (spofy-queue--queueable-uri-p "spotify:playlist:xyz")))

(ert-deftest spofy-queue-test-queueable-uri-malformed ()
  "A malformed URI, nil, or non-string input is not queueable."
  (should-not (spofy-queue--queueable-uri-p "not-a-uri"))
  (should-not (spofy-queue--queueable-uri-p ""))
  (should-not (spofy-queue--queueable-uri-p nil)))

;;;; Header formatting

(ert-deftest spofy-queue-test-header-with-current-track ()
  "Header shows currently-playing track and upcoming count."
  (let* ((current '((name . "Track A")
                    (artists . [((name . "Artist 1"))])))
         (queue [((name . "T1")) ((name . "T2"))])
         (lines (spofy-queue--header current queue)))
    (should (= 2 (length lines)))
    (should (string-match-p "Currently playing: Track A — Artist 1"
                            (nth 0 lines)))
    (should (string-match-p "Upcoming: 2 tracks" (nth 1 lines)))))

(ert-deftest spofy-queue-test-header-no-current-track ()
  "Header omits the current-track line when nothing is playing."
  (let* ((queue [((name . "T1"))])
         (lines (spofy-queue--header nil queue)))
    (should (= 1 (length lines)))
    (should (string-match-p "Upcoming: 1 track\\'" (nth 0 lines)))))

(ert-deftest spofy-queue-test-header-empty-queue ()
  "Header shows \"0 tracks\" when the queue is empty."
  (let ((lines (spofy-queue--header nil [])))
    (should (string-match-p "Upcoming: 0 tracks" (nth 0 lines)))))

(provide 'spofy-queue-test)
;;; spofy-queue-test.el ends here
