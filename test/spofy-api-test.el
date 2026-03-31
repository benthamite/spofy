;;; spofy-api-test.el --- Tests for spofy-api.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;;; Commentary:

;; ERT tests for the Spofy API client module.

;;; Code:

(require 'ert)
(require 'spofy-api)
(require 'cl-lib)

;;;; Test helpers

(defun spofy-api-test--mock-response (status-code &optional body headers)
  "Create a fake HTTP response buffer.
STATUS-CODE is the HTTP status code (integer).
BODY is the response body string (default empty).
HEADERS is an alist of extra headers to include."
  (let ((buf (generate-new-buffer " *spofy-test-response*")))
    (with-current-buffer buf
      (insert (format "HTTP/1.1 %d OK\r\n" status-code))
      (insert "Content-Type: application/json\r\n")
      (dolist (header headers)
        (insert (format "%s: %s\r\n" (car header) (cdr header))))
      (insert "\r\n")
      (when body
        (insert body)))
    buf))

;;;; Cache tests

(ert-deftest spofy-api-test-cache-put-get ()
  "Cache put stores a value that can be retrieved with get."
  (let ((spofy-api--cache (make-hash-table :test #'equal))
        (key '("GET" "https://api.spotify.com/v1/me/playlists" nil)))
    (spofy-api--cache-put key '((items . [])) 300)
    (should (equal '((items . [])) (spofy-api--cache-get key)))))

(ert-deftest spofy-api-test-cache-ttl-expiry ()
  "Cache entries expire after their TTL."
  (let ((spofy-api--cache (make-hash-table :test #'equal))
        (key '("GET" "https://api.spotify.com/v1/search" (("q" . "test")))))
    (spofy-api--cache-put key '((items . [])) 300)
    ;; Should be valid now
    (should (spofy-api--cache-valid-p key))
    ;; Manually expire: overwrite the timestamp to be in the past
    (let* ((entry (gethash key spofy-api--cache))
           (expired-time (- (float-time) 400)))
      (puthash key (list (car entry) expired-time (nth 2 entry)) spofy-api--cache))
    ;; Should be invalid now
    (should-not (spofy-api--cache-valid-p key))
    ;; get should return nil for expired entries
    (should-not (spofy-api--cache-get key))))

(ert-deftest spofy-api-test-cache-key-same-params ()
  "Same method + URL + params produce the same cache key."
  (let ((spofy-api--cache (make-hash-table :test #'equal))
        (key1 (spofy-api--cache-key "GET" "https://api.spotify.com/v1/search"
                                    '(("q" . "radiohead") ("type" . "track"))))
        (key2 (spofy-api--cache-key "GET" "https://api.spotify.com/v1/search"
                                    '(("q" . "radiohead") ("type" . "track")))))
    (should (equal key1 key2))))

(ert-deftest spofy-api-test-cache-key-different-params ()
  "Different params produce different cache keys."
  (let ((key1 (spofy-api--cache-key "GET" "https://api.spotify.com/v1/search"
                                    '(("q" . "radiohead"))))
        (key2 (spofy-api--cache-key "GET" "https://api.spotify.com/v1/search"
                                    '(("q" . "beatles")))))
    (should-not (equal key1 key2))))

(ert-deftest spofy-api-test-cache-key-different-methods ()
  "Different HTTP methods produce different cache keys."
  (let ((key1 (spofy-api--cache-key "GET" "https://api.spotify.com/v1/tracks/123" nil))
        (key2 (spofy-api--cache-key "PUT" "https://api.spotify.com/v1/tracks/123" nil)))
    (should-not (equal key1 key2))))

(ert-deftest spofy-api-test-cache-clear ()
  "Clearing the cache removes all entries."
  (let ((spofy-api--cache (make-hash-table :test #'equal)))
    (spofy-api--cache-put '("GET" "url1" nil) '((a . 1)) 300)
    (spofy-api--cache-put '("GET" "url2" nil) '((b . 2)) 300)
    (should (= 2 (hash-table-count spofy-api--cache)))
    (spofy-clear-cache)
    (should (= 0 (hash-table-count spofy-api--cache)))))

(ert-deftest spofy-api-test-player-state-bypasses-cache ()
  "Player state endpoint is never cached."
  (let ((spofy-api--cache (make-hash-table :test #'equal)))
    ;; Manually put a player state entry in the cache
    (let ((key (spofy-api--cache-key "GET" "https://api.spotify.com/v1/me/player" nil)))
      (spofy-api--cache-put key '((is_playing . t)) 300)
      ;; The skip-cache check should identify this as a player state endpoint
      (should (spofy-api--skip-cache-p "https://api.spotify.com/v1/me/player"))
      ;; Also check sub-paths
      (should (spofy-api--skip-cache-p "https://api.spotify.com/v1/me/player/currently-playing")))))

;;;; Retry-After header parsing

(ert-deftest spofy-api-test-retry-after-parsing ()
  "Retry-After header value is parsed correctly."
  (let ((buf (spofy-api-test--mock-response 429 nil '(("Retry-After" . "3")))))
    (unwind-protect
        (with-current-buffer buf
          (should (equal 3 (spofy-api--parse-retry-after))))
      (kill-buffer buf))))

(ert-deftest spofy-api-test-retry-after-missing ()
  "When Retry-After header is absent, return a default."
  (let ((buf (spofy-api-test--mock-response 429 nil)))
    (unwind-protect
        (with-current-buffer buf
          (should (numberp (spofy-api--parse-retry-after))))
      (kill-buffer buf))))

;;;; Exponential backoff

(ert-deftest spofy-api-test-exponential-backoff-delays ()
  "Exponential backoff delays follow the pattern 1, 2, 4 seconds."
  (should (= 1 (spofy-api--backoff-delay 0)))
  (should (= 2 (spofy-api--backoff-delay 1)))
  (should (= 4 (spofy-api--backoff-delay 2))))

;;;; URL construction

(ert-deftest spofy-api-test-url-construction-no-params ()
  "URL is built correctly from endpoint without params."
  (should (equal "https://api.spotify.com/v1/me/playlists"
                 (spofy-api--build-url "me/playlists" nil))))

(ert-deftest spofy-api-test-url-construction-with-params ()
  "URL is built correctly from endpoint with query params."
  (let ((url (spofy-api--build-url "search" '(("q" . "radiohead") ("type" . "track")))))
    (should (string-prefix-p "https://api.spotify.com/v1/search?" url))
    (should (string-match-p "q=radiohead" url))
    (should (string-match-p "type=track" url))))

(ert-deftest spofy-api-test-url-construction-leading-slash ()
  "Leading slash in endpoint is handled gracefully."
  (should (equal "https://api.spotify.com/v1/me/player"
                 (spofy-api--build-url "/me/player" nil))))

(ert-deftest spofy-api-test-url-construction-full-url ()
  "Full URLs (e.g. pagination next URLs) are passed through unchanged."
  (let ((full-url "https://api.spotify.com/v1/search?q=test&offset=20"))
    (should (equal full-url (spofy-api--build-url full-url nil)))))

;;;; JSON response parsing

(ert-deftest spofy-api-test-json-parsing ()
  "JSON response body is parsed correctly."
  (let ((buf (spofy-api-test--mock-response
              200
              "{\"items\":[{\"name\":\"OK Computer\"}],\"next\":null}")))
    (unwind-protect
        (with-current-buffer buf
          (let ((parsed (spofy-api--parse-response)))
            (should (listp parsed))
            ;; The parsed data should contain the items
            (let ((items (alist-get 'items parsed)))
              (should (vectorp items))
              (should (equal "OK Computer" (alist-get 'name (aref items 0)))))))
      (kill-buffer buf))))

(ert-deftest spofy-api-test-json-parsing-empty-body ()
  "Empty response body returns nil."
  (let ((buf (spofy-api-test--mock-response 204)))
    (unwind-protect
        (with-current-buffer buf
          (should-not (spofy-api--parse-response)))
      (kill-buffer buf))))

;;;; Authorization header injection

(ert-deftest spofy-api-test-authorization-header ()
  "Authorization header is injected with the access token."
  (let ((url-request-method nil)
        (url-request-extra-headers nil)
        (url-request-data nil)
        (captured-headers nil))
    ;; Mock spofy-auth-access-token to return a known token
    (cl-letf (((symbol-function 'spofy-auth-access-token)
               (lambda () "test-token-12345"))
              ((symbol-function 'url-retrieve)
               (lambda (_url callback &rest _args)
                 (setq captured-headers url-request-extra-headers)
                 ;; Create a fake response and call the callback
                 (let ((buf (spofy-api-test--mock-response 200 "{}")))
                   (with-current-buffer buf
                     (funcall callback nil))
                   buf))))
      (spofy-api--request "GET" "https://api.spotify.com/v1/me" nil nil
                          (lambda (_data) nil))
      (should (assoc "Authorization" captured-headers))
      (should (equal "Bearer test-token-12345"
                     (cdr (assoc "Authorization" captured-headers)))))))

;;;; HTTP status code extraction

(ert-deftest spofy-api-test-status-code-extraction ()
  "HTTP status code is extracted from the response buffer."
  (let ((buf (spofy-api-test--mock-response 200 "{}")))
    (unwind-protect
        (with-current-buffer buf
          (should (= 200 (spofy-api--response-status))))
      (kill-buffer buf)))
  (let ((buf (spofy-api-test--mock-response 401 "{\"error\":{\"message\":\"Unauthorized\"}}")))
    (unwind-protect
        (with-current-buffer buf
          (should (= 401 (spofy-api--response-status))))
      (kill-buffer buf)))
  (let ((buf (spofy-api-test--mock-response 429 nil '(("Retry-After" . "5")))))
    (unwind-protect
        (with-current-buffer buf
          (should (= 429 (spofy-api--response-status))))
      (kill-buffer buf))))

;;;; High-level helpers URL construction

(ert-deftest spofy-api-test-api-get-constructs-url ()
  "spofy-api-get constructs the correct URL and uses GET."
  (let ((captured-method nil)
        (captured-url nil))
    (cl-letf (((symbol-function 'spofy-api--request)
               (lambda (method url &optional _params _data _callback)
                 (setq captured-method method
                       captured-url url))))
      (spofy-api-get "me/playlists" '(("limit" . "50")) #'ignore)
      (should (equal "GET" captured-method))
      (should (equal "https://api.spotify.com/v1/me/playlists" captured-url)))))

(ert-deftest spofy-api-test-api-put-constructs-url ()
  "spofy-api-put constructs the correct URL and uses PUT."
  (let ((captured-method nil)
        (captured-url nil))
    (cl-letf (((symbol-function 'spofy-api--request)
               (lambda (method url &optional _params _data _callback)
                 (setq captured-method method
                       captured-url url))))
      (spofy-api-put "me/player/play" '((context_uri . "spotify:album:123")) #'ignore)
      (should (equal "PUT" captured-method))
      (should (equal "https://api.spotify.com/v1/me/player/play" captured-url)))))

(ert-deftest spofy-api-test-api-post-constructs-url ()
  "spofy-api-post constructs the correct URL and uses POST."
  (let ((captured-method nil))
    (cl-letf (((symbol-function 'spofy-api--request)
               (lambda (method _url &optional _params _data _callback)
                 (setq captured-method method))))
      (spofy-api-post "users/me/playlists" '((name . "Test")) #'ignore)
      (should (equal "POST" captured-method)))))

(ert-deftest spofy-api-test-api-delete-constructs-url ()
  "spofy-api-delete constructs the correct URL and uses DELETE."
  (let ((captured-method nil))
    (cl-letf (((symbol-function 'spofy-api--request)
               (lambda (method _url &optional _params _data _callback)
                 (setq captured-method method))))
      (spofy-api-delete "playlists/123/tracks" '((tracks . [])) #'ignore)
      (should (equal "DELETE" captured-method)))))

;;;; Pagination

(ert-deftest spofy-api-test-pagination-extracts-next-url ()
  "Pagination helper extracts the next URL from a response."
  (let ((response '((items . [((name . "Track 1")) ((name . "Track 2"))])
                    (next . "https://api.spotify.com/v1/search?q=test&offset=20")
                    (total . 100))))
    (let ((result (spofy-api--extract-paged-results response)))
      (should (equal (alist-get 'next result)
                     "https://api.spotify.com/v1/search?q=test&offset=20"))
      (should (vectorp (alist-get 'items result))))))

(ert-deftest spofy-api-test-pagination-null-next ()
  "When there is no next page, next is nil."
  (let ((response '((items . [((name . "Track 1"))])
                    (next)
                    (total . 1))))
    (let ((result (spofy-api--extract-paged-results response)))
      (should-not (alist-get 'next result)))))

;;;; Cache TTL lookup

(ert-deftest spofy-api-test-cache-ttl-lookup ()
  "Cache TTL is looked up based on endpoint patterns."
  ;; Search results should get 300 seconds
  (should (= 300 (spofy-api--cache-ttl "https://api.spotify.com/v1/search?q=test")))
  ;; Album details should get 600 seconds
  (should (= 600 (spofy-api--cache-ttl "https://api.spotify.com/v1/albums/123")))
  ;; Artist details should get 600 seconds
  (should (= 600 (spofy-api--cache-ttl "https://api.spotify.com/v1/artists/123")))
  ;; User playlists should get 60 seconds
  (should (= 60 (spofy-api--cache-ttl "https://api.spotify.com/v1/me/playlists")))
  ;; Playlist tracks should get 120 seconds
  (should (= 120 (spofy-api--cache-ttl "https://api.spotify.com/v1/playlists/123/tracks")))
  ;; Unknown endpoints get a default
  (should (numberp (spofy-api--cache-ttl "https://api.spotify.com/v1/something/else"))))

(provide 'spofy-api-test)
;;; spofy-api-test.el ends here
