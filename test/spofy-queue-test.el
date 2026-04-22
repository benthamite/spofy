;;; spofy-queue-test.el --- Tests for spofy-queue  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;;; Commentary:

;; ERT tests for `spofy-queue--queueable-uri-p'.

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

(provide 'spofy-queue-test)
;;; spofy-queue-test.el ends here
