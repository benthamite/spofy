;;; spofy-library-test.el --- Tests for spofy-library  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;;; Commentary:

;; ERT tests for pure helpers in `spofy-library': URI type extraction,
;; type normalization, and entity resolution.

;;; Code:

(require 'ert)
(require 'spofy-library)

;;;; Type extraction from URIs

(ert-deftest spofy-library-test-extract-type-track ()
  "A track URI yields the \"track\" type."
  (should (equal "track" (spofy-library--extract-type "spotify:track:abc123"))))

(ert-deftest spofy-library-test-extract-type-album ()
  "An album URI yields the \"album\" type."
  (should (equal "album" (spofy-library--extract-type "spotify:album:xyz"))))

(ert-deftest spofy-library-test-extract-type-artist ()
  "An artist URI yields the \"artist\" type."
  (should (equal "artist" (spofy-library--extract-type "spotify:artist:abc"))))

(ert-deftest spofy-library-test-extract-type-playlist ()
  "A playlist URI yields the \"playlist\" type."
  (should (equal "playlist"
                 (spofy-library--extract-type "spotify:playlist:abc"))))

(ert-deftest spofy-library-test-extract-type-malformed ()
  "A malformed URI yields nil."
  (should-not (spofy-library--extract-type "not-a-uri"))
  (should-not (spofy-library--extract-type "")))

;;;; Type normalization

(ert-deftest spofy-library-test-normalize-type-singulars ()
  "Singular names are returned unchanged."
  (should (equal "track" (spofy-library--normalize-type "track")))
  (should (equal "album" (spofy-library--normalize-type "album"))))

(ert-deftest spofy-library-test-normalize-type-plurals ()
  "Plural names are singularized."
  (should (equal "track" (spofy-library--normalize-type "tracks")))
  (should (equal "album" (spofy-library--normalize-type "albums"))))

(ert-deftest spofy-library-test-normalize-type-symbols ()
  "Symbols are accepted and singularized."
  (should (equal "track" (spofy-library--normalize-type 'track)))
  (should (equal "album" (spofy-library--normalize-type 'albums))))

(ert-deftest spofy-library-test-normalize-type-unknown ()
  "Unknown types return nil."
  (should-not (spofy-library--normalize-type "playlist"))
  (should-not (spofy-library--normalize-type "foo")))

;;;; Entity resolution

(ert-deftest spofy-library-test-resolve-entity-from-uri ()
  "A URI alone yields a (TYPE . ID) pair."
  (should (equal '("track" . "abc")
                 (spofy-library--resolve-entity "spotify:track:abc"))))

(ert-deftest spofy-library-test-resolve-entity-from-type-and-id ()
  "An explicit type and ID pair works for both strings and symbols."
  (should (equal '("album" . "xyz")
                 (spofy-library--resolve-entity "album" "xyz")))
  (should (equal '("track" . "xyz")
                 (spofy-library--resolve-entity 'track "xyz"))))

(ert-deftest spofy-library-test-resolve-entity-normalizes-plural ()
  "Plural type names are normalized to singular."
  (should (equal '("album" . "abc")
                 (spofy-library--resolve-entity "albums" "abc"))))

(provide 'spofy-library-test)
;;; spofy-library-test.el ends here
