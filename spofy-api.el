;;; spofy-api.el --- Spotify Web API client for Spofy  -*- lexical-binding: t; -*-

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

;; HTTP client for the Spotify Web API.  All API communication flows through
;; `spofy-api--request', which handles authentication, rate limiting, retries,
;; and response parsing.  An in-memory cache reduces redundant network calls.

;;; Code:

(require 'url)
(require 'url-http)
(require 'json)
(require 'cl-lib)
(require 'spofy-ui)

(declare-function spofy-auth-access-token "spofy-auth" ())
(declare-function spofy-auth-refresh-token "spofy-auth" ())

;;;; Constants

(defconst spofy-api--base-url "https://api.spotify.com/v1/"
  "Base URL for the Spotify Web API.")

(defconst spofy-api--max-retries 3
  "Maximum number of retries for transient (5xx) errors.")

(defconst spofy-api--error-cooldown 60
  "Minimum seconds between repeated error messages for the same endpoint.")

;;;; Error rate limiting

(defvar spofy-api--last-error-times (make-hash-table :test #'equal)
  "Hash table mapping endpoint URLs to the last time an error was logged.")

(defconst spofy-api--default-retry-after 1
  "Default Retry-After delay in seconds when the header is missing.")

;;;; Cache

(defvar spofy-api--cache (make-hash-table :test #'equal)
  "In-memory cache for API responses.
Keys are lists of (METHOD URL PARAMS).
Values are lists of (DATA TIMESTAMP TTL).")

(defvar spofy-api--cache-ttls
  '(("search" . 300)
    ("me/playlists" . 60)
    ("playlists/.*/tracks" . 120)
    ("albums/" . 600)
    ("artists/" . 600))
  "Alist of endpoint URL patterns to cache TTL in seconds.
Patterns are matched as regexps against the URL.")

(defconst spofy-api--default-cache-ttl 120
  "Default cache TTL in seconds for endpoints not in `spofy-api--cache-ttls'.")

(defun spofy-api--cache-key (method url params)
  "Generate a cache key from METHOD, URL, and PARAMS."
  (list method url params))

(defun spofy-api--cache-put (key data ttl)
  "Store DATA in the cache under KEY with TTL seconds."
  (puthash key (list data (float-time) ttl) spofy-api--cache))

(defun spofy-api--cache-valid-p (key)
  "Return non-nil if KEY has a valid (non-expired) cache entry."
  (when-let* ((entry (gethash key spofy-api--cache)))
    (let ((timestamp (nth 1 entry))
          (ttl (nth 2 entry)))
      (< (- (float-time) timestamp) ttl))))

(defun spofy-api--cache-get (key)
  "Return cached data for KEY, or nil if expired or absent."
  (when (spofy-api--cache-valid-p key)
    (car (gethash key spofy-api--cache))))

(defun spofy-api--cache-ttl (url)
  "Return the cache TTL for URL based on `spofy-api--cache-ttls'."
  (let ((ttl nil))
    (cl-loop for (pattern . seconds) in spofy-api--cache-ttls
             when (string-match-p pattern url)
             do (setq ttl seconds)
             and return nil)
    (or ttl spofy-api--default-cache-ttl)))

(defun spofy-api--skip-cache-p (url)
  "Return non-nil if URL should never be cached (e.g. player state)."
  (string-match-p "me/player\\(?:/\\|$\\)" url))

;;;###autoload
(defun spofy-clear-cache ()
  "Clear the Spofy API response cache."
  (interactive)
  (clrhash spofy-api--cache)
  (message "Spofy: API cache cleared."))

;;;; Response parsing

(defun spofy-api--response-status ()
  "Extract the HTTP status code from the current response buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (string-to-number (match-string 1)))))

(defun spofy-api--parse-retry-after ()
  "Parse the Retry-After header from the current response buffer.
Return the value as a number of seconds, or `spofy-api--default-retry-after'
if the header is absent."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^Retry-After: \\([0-9]+\\)" nil t)
        (string-to-number (match-string 1))
      spofy-api--default-retry-after)))

(defun spofy-api--parse-response ()
  "Parse the JSON body from the current HTTP response buffer.
Return the parsed data as an alist, or nil if the body is empty
or not valid JSON."
  (save-excursion
    (goto-char (point-min))
    ;; Skip past the HTTP headers (separated from body by a blank line)
    (when (re-search-forward "\r?\n\r?\n" nil t)
      (let ((body (buffer-substring-no-properties (point) (point-max))))
        (when (and body (not (string-empty-p (string-trim body))))
          (condition-case err
              (json-parse-string body :object-type 'alist :array-type 'array)
            (json-parse-error
             (message "Spofy: failed to parse JSON response: %s"
                      (error-message-string err))
             nil)))))))

;;;; URL construction

(defun spofy-api--build-url (endpoint params)
  "Build a full Spotify API URL from ENDPOINT and query PARAMS.
ENDPOINT is a relative path (e.g. \"me/playlists\") or a full URL
\(e.g. a pagination `next' URL).  PARAMS is an alist of query parameters."
  (let ((url (if (string-prefix-p "https://" endpoint)
                 endpoint
               (concat spofy-api--base-url
                       (string-remove-prefix "/" endpoint)))))
    (if params
        (concat url (if (string-match-p "\\?" url) "&" "?")
                (mapconcat
                 (lambda (pair)
                   (format "%s=%s"
                           (url-hexify-string (car pair))
                           (url-hexify-string (cdr pair))))
                 params "&"))
      url)))

;;;; Backoff

(defun spofy-api--backoff-delay (retry-count)
  "Return the backoff delay in seconds for RETRY-COUNT (0-indexed)."
  (expt 2 retry-count))

;;;; Core request function

(defun spofy-api--request (method url &optional params data callback)
  "Send an HTTP request to the Spotify API.

METHOD is the HTTP method string (\"GET\", \"PUT\", \"POST\", \"DELETE\").
URL is the full request URL.
PARAMS is an optional alist of query parameters (for GET requests).
DATA is an optional alist to be JSON-encoded as the request body.
CALLBACK is a function called with the parsed JSON response on success.

Authentication, rate-limit handling (429), token refresh (401), and
exponential backoff for transient errors (5xx) are handled automatically."
  (spofy-api--request-with-retries method url params data callback 0 nil))

(defun spofy-api--request-with-retries (method url params data callback retry-count refreshed-p)
  "Internal: send request with retry logic.
METHOD, URL, PARAMS, DATA, CALLBACK are as in `spofy-api--request'.
RETRY-COUNT is the current retry attempt (0-indexed).
REFRESHED-P is non-nil if we have already attempted a token refresh."
  ;; Check cache for GET requests
  (let* ((full-url (if params (spofy-api--build-url url params) url))
         (cache-key (spofy-api--cache-key method full-url nil)))
    (if-let* ((cached (and (equal method "GET")
                         (not (spofy-api--skip-cache-p full-url))
                         (spofy-api--cache-get cache-key))))
        ;; Return cached result
        (when callback
          (funcall callback cached))
      ;; Make the actual request.  Spotify requires a JSON body (even
      ;; "{}") on PUT/POST/DELETE; omitting it causes "Malformed json".
      (let* ((token (spofy-auth-access-token))
             (has-body (not (equal method "GET"))))
        (if (null token)
            ;; No valid token — cannot make request
            (let ((last (gethash full-url spofy-api--last-error-times 0)))
              (when (> (- (float-time) last) spofy-api--error-cooldown)
                (puthash full-url (float-time) spofy-api--last-error-times)
                (message "Spofy: no valid access token; try M-x spofy-authenticate")))
          (let* ((url-request-method method)
                 (url-request-extra-headers
                  `(("Authorization" . ,(concat "Bearer " token))
                    ,@(when has-body '(("Content-Type" . "application/json")))))
                 (url-request-data (cond (data (json-serialize data))
                                         (has-body "{}")))
                 (url-show-status nil))
            (url-retrieve
             full-url
             (lambda (_status)
               (let ((buf (current-buffer))
                     (status-code (spofy-api--response-status)))
                 (cond
                  ;; Success (2xx)
                  ((and status-code (>= status-code 200) (< status-code 300))
                   (let ((parsed (spofy-api--parse-response)))
                     (kill-buffer buf)
                     ;; Cache GET responses (unless skip-cache)
                     (when (and (equal method "GET")
                                (not (spofy-api--skip-cache-p full-url)))
                       (spofy-api--cache-put cache-key parsed
                                             (spofy-api--cache-ttl full-url)))
                     (when callback
                       (funcall callback parsed))))
                  ;; Unauthorized (401): refresh token and retry once
                  ((and (eql status-code 401) (not refreshed-p))
                   (kill-buffer buf)
                   (condition-case err
                       (spofy-auth-refresh-token)
                     (error
                      (message "Spofy: token refresh failed: %s"
                               (error-message-string err))))
                   (spofy-api--request-with-retries
                    method url params data callback retry-count t))
                  ;; Rate limited (429): retry after delay
                  ((and (eql status-code 429)
                        (< retry-count spofy-api--max-retries))
                   (let ((delay (spofy-api--parse-retry-after)))
                     (kill-buffer buf)
                     (run-at-time delay nil
                                  #'spofy-api--request-with-retries
                                  method url params data callback
                                  (1+ retry-count) refreshed-p)))
                  ;; Server error (5xx): exponential backoff
                  ((and status-code (>= status-code 500)
                        (< retry-count spofy-api--max-retries))
                   (let ((delay (spofy-api--backoff-delay retry-count)))
                     (kill-buffer buf)
                     (run-at-time delay nil
                                  #'spofy-api--request-with-retries
                                  method url params data callback
                                  (1+ retry-count) refreshed-p)))
                  ;; No active device (404 on player endpoints)
                  ((and (eql status-code 404)
                        (string-match-p "me/player" full-url))
                   (kill-buffer buf)
                   (let ((last (gethash full-url spofy-api--last-error-times 0)))
                     (when (> (- (float-time) last) spofy-api--error-cooldown)
                       (puthash full-url (float-time) spofy-api--last-error-times)
                       (message "Spofy: no active device found; please open Spotify on a device"))))
                  ;; All retries exhausted or unrecoverable error
                  (t
                   (kill-buffer buf)
                   (let ((last (gethash full-url spofy-api--last-error-times 0)))
                     (when (> (- (float-time) last) spofy-api--error-cooldown)
                       (puthash full-url (float-time) spofy-api--last-error-times)
                       (message "Spofy: API request failed (HTTP %s): %s %s"
                                (or status-code "?") method full-url))))))))))))))

;;;; High-level helpers

(defun spofy-api-get (endpoint &optional params callback)
  "Send a GET request to ENDPOINT with optional query PARAMS.
ENDPOINT is a relative API path (e.g. \"me/playlists\").
CALLBACK is called with the parsed JSON response."
  (spofy-api--request "GET" (spofy-api--build-url endpoint nil)
                      params nil callback))

(defun spofy-api-get-sync (endpoint &optional params)
  "Synchronously GET ENDPOINT with optional query PARAMS.
Return the parsed JSON response, or nil on error."
  (when-let* ((token (spofy-auth-access-token)))
    (let* ((url (spofy-api--build-url endpoint params))
           (url-request-method "GET")
           (url-request-extra-headers
            `(("Authorization" . ,(concat "Bearer " token))))
           (url-show-status nil)
           (buf (url-retrieve-synchronously url t nil 5)))
      (when buf
        (unwind-protect
            (with-current-buffer buf
              (let ((status (spofy-api--response-status)))
                (when (and status (>= status 200) (< status 300))
                  (spofy-api--parse-response))))
          (kill-buffer buf))))))

(defun spofy-api-put (endpoint &optional data callback)
  "Send a PUT request to ENDPOINT with optional JSON body DATA.
CALLBACK is called with the parsed JSON response."
  (spofy-api--request "PUT" (spofy-api--build-url endpoint nil)
                      nil data callback))

(defun spofy-api-post (endpoint &optional data callback)
  "Send a POST request to ENDPOINT with optional JSON body DATA.
CALLBACK is called with the parsed JSON response."
  (spofy-api--request "POST" (spofy-api--build-url endpoint nil)
                      nil data callback))

(defun spofy-api-delete (endpoint &optional data callback)
  "Send a DELETE request to ENDPOINT with optional JSON body DATA.
CALLBACK is called with the parsed JSON response."
  (spofy-api--request "DELETE" (spofy-api--build-url endpoint nil)
                      nil data callback))

;;;; Pagination

(defun spofy-api--extract-paged-results (response)
  "Extract items and pagination info from a paginated RESPONSE.
Return an alist with `items' (vector) and `next' (URL string or nil)."
  (let ((items (alist-get 'items response))
        (next (alist-get 'next response)))
    `((items . ,items)
      (next . ,(unless (eq next :null) next)))))

(provide 'spofy-api)
;;; spofy-api.el ends here
