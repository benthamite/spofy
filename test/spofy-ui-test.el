;;; spofy-ui-test.el --- Tests for spofy-ui  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;;; Commentary:

;; ERT tests for pure helpers in `spofy-ui': ID and URL extraction,
;; formatting utilities, and release-year parsing.

;;; Code:

(require 'ert)
(require 'spofy-ui)

;;;; Extract ID from URI

(ert-deftest spofy-ui-test-extract-id-track ()
  "The ID portion of a track URI is returned."
  (should (equal "abc123" (spofy-ui-extract-id "spotify:track:abc123"))))

(ert-deftest spofy-ui-test-extract-id-album ()
  "The ID portion of an album URI is returned."
  (should (equal "xyz" (spofy-ui-extract-id "spotify:album:xyz"))))

(ert-deftest spofy-ui-test-extract-id-plain-id ()
  "A plain ID (no prefix) is returned unchanged."
  (should (equal "abc123" (spofy-ui-extract-id "abc123"))))

;;;; URI to open.spotify.com URL

(ert-deftest spofy-ui-test-uri-to-url-track ()
  "A track URI is mapped to the canonical open.spotify.com URL."
  (should (equal "https://open.spotify.com/track/abc123"
                 (spofy-ui-uri-to-url "spotify:track:abc123"))))

(ert-deftest spofy-ui-test-uri-to-url-album ()
  "An album URI is mapped to the canonical open.spotify.com URL."
  (should (equal "https://open.spotify.com/album/xyz"
                 (spofy-ui-uri-to-url "spotify:album:xyz"))))

(ert-deftest spofy-ui-test-uri-to-url-artist ()
  "An artist URI is mapped to the canonical open.spotify.com URL."
  (should (equal "https://open.spotify.com/artist/abc"
                 (spofy-ui-uri-to-url "spotify:artist:abc"))))

(ert-deftest spofy-ui-test-uri-to-url-playlist ()
  "A playlist URI is mapped to the canonical open.spotify.com URL."
  (should (equal "https://open.spotify.com/playlist/pid"
                 (spofy-ui-uri-to-url "spotify:playlist:pid"))))

(ert-deftest spofy-ui-test-uri-to-url-malformed ()
  "A malformed URI returns nil."
  (should-not (spofy-ui-uri-to-url "not-a-uri"))
  (should-not (spofy-ui-uri-to-url ""))
  (should-not (spofy-ui-uri-to-url nil))
  (should-not (spofy-ui-uri-to-url "spotify:track:")))

;;;; Album year parsing

(ert-deftest spofy-ui-test-album-year-full-date ()
  "A YYYY-MM-DD release date yields just the year."
  (should (equal "2024"
                 (spofy-ui-album-year '((release_date . "2024-05-01"))))))

(ert-deftest spofy-ui-test-album-year-year-only ()
  "A year-only release date returns the year."
  (should (equal "1999" (spofy-ui-album-year '((release_date . "1999"))))))

(ert-deftest spofy-ui-test-album-year-missing ()
  "A missing release date returns an empty string."
  (should (equal "" (spofy-ui-album-year '()))))

(ert-deftest spofy-ui-test-album-year-malformed ()
  "A non-date release date is passed through verbatim."
  (should (equal "unknown"
                 (spofy-ui-album-year '((release_date . "unknown"))))))

;;;; Duration formatting

(ert-deftest spofy-ui-test-format-duration-ms-short ()
  "Durations under a minute format with a zero-padded seconds field."
  (should (equal "0:05" (spofy-ui-format-duration-ms 5000))))

(ert-deftest spofy-ui-test-format-duration-ms-minute ()
  "Durations over a minute format as M:SS."
  (should (equal "1:30" (spofy-ui-format-duration-ms 90000))))

(ert-deftest spofy-ui-test-format-duration-ms-long ()
  "Multi-minute durations keep growing the minutes field."
  (should (equal "12:34" (spofy-ui-format-duration-ms 754000))))

;;;; Artist formatting

(ert-deftest spofy-ui-test-format-artists-single ()
  "A single artist returns its name."
  (should (equal "Artist" (spofy-ui-format-artists '(((name . "Artist")))))))

(ert-deftest spofy-ui-test-format-artists-multiple ()
  "Multiple artists are joined with \", \"."
  (should (equal "A, B, C"
                 (spofy-ui-format-artists '(((name . "A"))
                                            ((name . "B"))
                                            ((name . "C")))))))

(ert-deftest spofy-ui-test-format-artists-empty ()
  "An empty artist list returns an empty string."
  (should (equal "" (spofy-ui-format-artists '()))))

(provide 'spofy-ui-test)
;;; spofy-ui-test.el ends here
