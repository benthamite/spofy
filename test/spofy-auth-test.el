;;; spofy-auth-test.el --- Tests for spofy-auth  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;;; Commentary:

;; ERT tests for the Spofy OAuth2 authentication module.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'spofy-auth)

;;;; Scope completeness

(ert-deftest spofy-auth-test-scopes-complete ()
  "All required OAuth scopes are present."
  (let ((required '("user-read-playback-state"
                     "user-modify-playback-state"
                     "user-read-currently-playing"
                     "user-read-recently-played"
                     "user-top-read"
                     "user-library-read"
                     "user-library-modify"
                     "user-follow-read"
                     "user-follow-modify"
                     "playlist-read-private"
                     "playlist-read-collaborative"
                     "playlist-modify-public"
                     "playlist-modify-private"
                     "streaming")))
    (dolist (scope required)
      (should (member scope spofy-auth--scopes)))))

(ert-deftest spofy-auth-test-scopes-no-extras ()
  "No extra scopes beyond the required set."
  (should (= (length spofy-auth--scopes) 14)))

;;;; Redirect URI construction

(ert-deftest spofy-auth-test-redirect-uri-default-port ()
  "Redirect URI uses the default port 8080."
  (let ((spofy-redirect-port 8080))
    (should (equal (spofy-auth--redirect-uri)
                   "http://127.0.0.1:8080/spofy/callback"))))

(ert-deftest spofy-auth-test-redirect-uri-custom-port ()
  "Redirect URI respects a custom port."
  (let ((spofy-redirect-port 9999))
    (should (equal (spofy-auth--redirect-uri)
                   "http://127.0.0.1:9999/spofy/callback"))))

;;;; Authorization URL construction

(ert-deftest spofy-auth-test-authorize-url-contains-client-id ()
  "Authorization URL contains the client ID."
  (let ((spofy-client-id "test-client-id")
        (spofy-redirect-port 8080))
    (let ((url (spofy-auth--authorize-url)))
      (should (string-match-p "client_id=test-client-id" url)))))

(ert-deftest spofy-auth-test-authorize-url-contains-redirect-uri ()
  "Authorization URL contains the redirect URI."
  (let ((spofy-client-id "test-client-id")
        (spofy-redirect-port 8080))
    (let ((url (spofy-auth--authorize-url)))
      (should (string-match-p "redirect_uri=" url))
      ;; The redirect URI should contain the loopback callback path
      (should (string-match-p "127\\.0\\.0\\.1.*8080.*/spofy/callback" url)))))

(ert-deftest spofy-auth-test-authorize-url-contains-all-scopes ()
  "Authorization URL contains all required scopes."
  (let ((spofy-client-id "test-client-id")
        (spofy-redirect-port 8080))
    (let ((url (spofy-auth--authorize-url)))
      (should (string-match-p "scope=" url))
      ;; Check that at least a few key scopes appear (URL-encoded space = %20 or +)
      (should (string-match-p "user-read-playback-state" url))
      (should (string-match-p "streaming" url))
      (should (string-match-p "playlist-modify-private" url)))))

(ert-deftest spofy-auth-test-authorize-url-contains-state ()
  "Authorization URL contains a state parameter."
  (let ((spofy-client-id "test-client-id")
        (spofy-redirect-port 8080))
    (let ((url (spofy-auth--authorize-url)))
      (should (string-match-p "state=" url)))))

(ert-deftest spofy-auth-test-authorize-url-response-type-code ()
  "Authorization URL requests response_type=code."
  (let ((spofy-client-id "test-client-id")
        (spofy-redirect-port 8080))
    (let ((url (spofy-auth--authorize-url)))
      (should (string-match-p "response_type=code" url)))))

(ert-deftest spofy-auth-test-authorize-url-base ()
  "Authorization URL starts with the Spotify authorize endpoint."
  (let ((spofy-client-id "test-client-id")
        (spofy-redirect-port 8080))
    (let ((url (spofy-auth--authorize-url)))
      (should (string-prefix-p "https://accounts.spotify.com/authorize?" url)))))

;;;; Token expiry check

(ert-deftest spofy-auth-test-token-expired-no-expiry ()
  "Token is expired when no expiry time is set."
  (let ((spofy-auth--token-expiry nil))
    (should (spofy-auth--token-expired-p))))

(ert-deftest spofy-auth-test-token-expired-past ()
  "Token is expired when expiry is in the past."
  (let ((spofy-auth--token-expiry (- (float-time) 100)))
    (should (spofy-auth--token-expired-p))))

(ert-deftest spofy-auth-test-token-not-expired ()
  "Token is not expired when expiry is well in the future."
  (let ((spofy-auth--token-expiry (+ (float-time) 3600)))
    (should-not (spofy-auth--token-expired-p))))

(ert-deftest spofy-auth-test-token-expired-within-60s ()
  "Token is considered expired when it expires within 60 seconds."
  (let ((spofy-auth--token-expiry (+ (float-time) 30)))
    (should (spofy-auth--token-expired-p))))

(ert-deftest spofy-auth-test-token-not-expired-boundary ()
  "Token is not expired when expiry is exactly 61 seconds away."
  (let ((spofy-auth--token-expiry (+ (float-time) 61)))
    (should-not (spofy-auth--token-expired-p))))

;;;; Token storage and retrieval (in-memory, mocked auth-source)

(ert-deftest spofy-auth-test-store-tokens-in-memory ()
  "Storing tokens sets the in-memory variables."
  (let ((spofy-auth--access-token nil)
        (spofy-auth--refresh-token nil)
        (spofy-auth--token-expiry nil))
    (cl-letf (((symbol-function 'spofy-auth--persist-tokens)
               (lambda () nil)))
      (spofy-auth--store-tokens "my-access" "my-refresh" 3600))
    (should (equal spofy-auth--access-token "my-access"))
    (should (equal spofy-auth--refresh-token "my-refresh"))
    ;; Expiry should be approximately now + 3600
    (should (> spofy-auth--token-expiry (+ (float-time) 3500)))
    (should (< spofy-auth--token-expiry (+ (float-time) 3700)))))

(ert-deftest spofy-auth-test-access-token-returns-from-memory ()
  "Access token is returned from memory when available and not expired."
  (let ((spofy-auth--access-token "cached-token")
        (spofy-auth--token-expiry (+ (float-time) 3600)))
    (should (equal (spofy-auth-access-token) "cached-token"))))

(ert-deftest spofy-auth-test-access-token-nil-when-expired ()
  "Access token returns nil when the token is expired and refresh is not attempted."
  (let ((spofy-auth--access-token "old-token")
        (spofy-auth--refresh-token nil)
        (spofy-auth--token-expiry (- (float-time) 100)))
    (should-not (spofy-auth-access-token))))

(ert-deftest spofy-auth-test-access-token-never-refreshes-synchronously ()
  "The synchronous accessor does not contact Spotify on expired tokens."
  (let ((spofy-auth--access-token "old-token")
        (spofy-auth--refresh-token "refresh-token")
        (spofy-auth--token-expiry (- (float-time) 100))
        refreshed)
    (cl-letf (((symbol-function 'spofy-auth-refresh-token)
               (lambda (&rest _) (setq refreshed t))))
      (should-not (spofy-auth-access-token))
      (should-not refreshed))))

(ert-deftest spofy-auth-test-access-token-async-refreshes-expired-token ()
  "The async accessor refreshes expired tokens through a callback."
  (let ((spofy-auth--access-token "old-token")
        (spofy-auth--refresh-token "refresh-token")
        (spofy-auth--token-expiry (- (float-time) 100))
        result)
    (cl-letf (((symbol-function 'spofy-auth-refresh-token)
               (lambda (callback)
                 (funcall callback "new-token"))))
      (spofy-auth-access-token-async (lambda (token) (setq result token)))
      (should (equal result "new-token")))))

;;;; Token exchange request body format

(ert-deftest spofy-auth-test-exchange-code-request-body ()
  "Token exchange builds the correct POST body."
  (let ((spofy-client-id "cid")
        (spofy-client-secret "csecret")
        (spofy-redirect-port 8080)
        (captured-data nil))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url callback &rest _)
                 ;; Don't actually make a request; just capture url-request-data
                 (setq captured-data url-request-data)
                 nil)))
      (spofy-auth--exchange-code "auth-code-123" #'ignore)
      (should (stringp captured-data))
      (should (string-match-p "grant_type=authorization_code" captured-data))
      (should (string-match-p "code=auth-code-123" captured-data))
      (should (string-match-p "redirect_uri=" captured-data))
      (should (string-match-p "client_id=cid" captured-data))
      (should (string-match-p "client_secret=csecret" captured-data)))))

(ert-deftest spofy-auth-test-exchange-code-handles-missing-response ()
  "Token exchange reports failure when no HTTP response was received."
  (let ((spofy-client-id "cid")
        (spofy-client-secret "csecret")
        request-callback
        response-buffer
        (callback-count 0)
        (result 'not-called))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url callback &rest _)
                 (setq request-callback callback))))
      (spofy-auth--exchange-code
       "auth-code-123"
       (lambda (&rest tokens)
         (cl-incf callback-count)
         (setq result tokens))))
    (with-temp-buffer
      (setq response-buffer (current-buffer))
      (setq-local url-http-end-of-headers nil)
      (funcall request-callback nil))
    (should (equal result '(nil nil nil)))
    (should (= callback-count 1))
    (should-not (buffer-live-p response-buffer))))

(ert-deftest spofy-auth-test-refresh-handles-missing-response ()
  "Token refresh reports failure when no HTTP response was received."
  (let ((spofy-client-id "cid")
        (spofy-client-secret "csecret")
        (spofy-auth--access-token "old-token")
        (spofy-auth--refresh-token "refresh-token")
        (spofy-auth--token-expiry 123)
        request-callback
        response-buffer
        (callback-count 0)
        (result 'not-called))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url callback &rest _)
                 (setq request-callback callback))))
      (spofy-auth-refresh-token
       (lambda (token)
         (cl-incf callback-count)
         (setq result token))))
    (with-temp-buffer
      (setq response-buffer (current-buffer))
      (setq-local url-http-end-of-headers nil)
      (funcall request-callback nil))
    (should-not result)
    (should (= callback-count 1))
    (should-not (buffer-live-p response-buffer))
    (should (equal spofy-auth--access-token "old-token"))
    (should (equal spofy-auth--refresh-token "refresh-token"))
    (should (= spofy-auth--token-expiry 123))))

(ert-deftest spofy-auth-test-refresh-accepts-nil-success-status ()
  "Token refresh accepts nil status when an HTTP response was received."
  (let ((spofy-client-id "cid")
        (spofy-client-secret "csecret")
        (spofy-auth--refresh-token "refresh-token")
        request-callback
        result)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url callback &rest _)
                 (setq request-callback callback)))
              ((symbol-function 'spofy-auth--persist-tokens) #'ignore))
      (spofy-auth-refresh-token
       (lambda (token)
         (setq result token)))
      (with-temp-buffer
        (insert "HTTP/1.1 200 OK\r\n\r\n")
        (setq-local url-http-end-of-headers (point-marker))
        (insert "{\"access_token\":\"new-token\",\"expires_in\":3600}")
        (funcall request-callback nil)))
    (should (equal result "new-token"))))

(ert-deftest spofy-auth-test-callback-handles-token-exchange-failure ()
  "OAuth callback stops cleanly when the token exchange fails."
  (let ((spofy-auth--state "expected-state")
        exchange-callback
        response
        stored
        stopped)
    (cl-letf (((symbol-function 'spofy-auth--send-response)
               (lambda (_proc status body)
                 (setq response (list status body))))
              ((symbol-function 'spofy-auth--exchange-code)
               (lambda (_code callback)
                 (setq exchange-callback callback)))
              ((symbol-function 'spofy-auth--store-tokens)
               (lambda (&rest _)
                 (setq stored t)))
              ((symbol-function 'spofy-auth--stop-server)
               (lambda ()
                 (setq stopped t))))
      (spofy-auth--handle-callback
       nil
       "GET /spofy/callback?code=code&state=expected-state HTTP/1.1\r\n\r\n")
      (should-not response)
      (funcall exchange-callback nil nil nil))
    (should-not stored)
    (should stopped)
    (should (equal response
                   '(502 "Authentication failed while exchanging tokens. You can close this window.")))))

;;;; Callback parsing

(ert-deftest spofy-auth-test-parse-callback-extracts-code ()
  "Callback parser extracts the authorization code from a query string."
  (let ((result (spofy-auth--parse-callback
                 "GET /spofy/callback?code=abc123&state=xyz HTTP/1.1\r\nHost: localhost\r\n\r\n")))
    (should (equal (alist-get 'code result) "abc123"))))

(ert-deftest spofy-auth-test-parse-callback-extracts-state ()
  "Callback parser extracts the state parameter."
  (let ((result (spofy-auth--parse-callback
                 "GET /spofy/callback?code=abc123&state=xyz HTTP/1.1\r\nHost: localhost\r\n\r\n")))
    (should (equal (alist-get 'state result) "xyz"))))

(ert-deftest spofy-auth-test-parse-callback-handles-error ()
  "Callback parser extracts the error parameter when present."
  (let ((result (spofy-auth--parse-callback
                 "GET /spofy/callback?error=access_denied&state=xyz HTTP/1.1\r\nHost: localhost\r\n\r\n")))
    (should (equal (alist-get 'error result) "access_denied"))
    (should-not (alist-get 'code result))))

(provide 'spofy-auth-test)
;;; spofy-auth-test.el ends here
