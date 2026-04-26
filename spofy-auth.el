;;; spofy-auth.el --- OAuth2 authentication for spofy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;; Author: Pablo Stafforini
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

;; OAuth2 authentication for the spofy Spotify client.  Handles the
;; authorization code flow: starts a local HTTP server, opens the
;; browser for authorization, exchanges the code for tokens, and
;; persists them in `spofy-token-file'.

;;; Code:

(require 'spofy-ui)
(require 'epa-file)
(require 'url)
(require 'url-http)
(require 'cl-lib)

(defvar url-http-end-of-headers)

;;;; Customization

(defcustom spofy-client-id nil
  "Spotify application client ID.
Register at `https://developer.spotify.com/dashboard'."
  :type '(choice (const :tag "Not set" nil) string)
  :group 'spofy)

(defcustom spofy-client-secret nil
  "Spotify application client secret.
Register at `https://developer.spotify.com/dashboard'."
  :type '(choice (const :tag "Not set" nil) string)
  :group 'spofy)

(defcustom spofy-redirect-port 8080
  "Port for the local OAuth2 redirect server."
  :type 'integer
  :group 'spofy)

(defcustom spofy-token-file (locate-user-emacs-file "spofy-tokens.el")
  "File for storing Spotify OAuth tokens.
Tokens are stored as a Lisp alist with restrictive file permissions
\(600).  For additional security, use a .gpg extension to have Emacs
encrypt the file automatically via EasyPG (requires a configured GPG
key)."
  :type 'file
  :group 'spofy)

;;;; Hooks

(defvar spofy-authenticated-hook nil
  "Hook run after successful authentication with Spotify.")

;;;; OAuth2 scopes

(defconst spofy-auth--scopes
  '("user-read-playback-state"
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
    "streaming")
  "OAuth2 scopes required by spofy.")

;;;; Internal state

(defvar spofy-auth--access-token nil
  "Current OAuth2 access token.")

(defvar spofy-auth--refresh-token nil
  "Current OAuth2 refresh token.")

(defvar spofy-auth--token-expiry nil
  "Time (as float seconds since epoch) when the access token expires.")

(defvar spofy-auth--state nil
  "Random state parameter for the current OAuth2 flow.")

(defvar spofy-auth--server nil
  "The network process for the local OAuth2 callback server.")

;;;; Redirect URI

(defun spofy-auth--redirect-uri ()
  "Return the OAuth2 redirect URI based on `spofy-redirect-port'."
  (format "http://127.0.0.1:%d/spofy/callback" spofy-redirect-port))

;;;; Authorization URL

(defun spofy-auth--authorize-url ()
  "Build the Spotify authorization URL.
Includes client ID, redirect URI, all required scopes, response
type, and a random state parameter."
  ;; Generate 64 bits of random hex as a CSRF-prevention nonce.
  (setq spofy-auth--state (format "%08x%08x"
                                  (random (expt 16 8))
                                  (random (expt 16 8))))
  (format "https://accounts.spotify.com/authorize?%s"
          (url-build-query-string
           `(("client_id" ,spofy-client-id)
             ("response_type" "code")
             ("redirect_uri" ,(spofy-auth--redirect-uri))
             ("scope" ,(mapconcat #'identity spofy-auth--scopes " "))
             ("state" ,spofy-auth--state)))))

;;;; Token expiry

(defun spofy-auth--token-expired-p ()
  "Return non-nil if the access token is expired or expires within 60 seconds."
  ;; Treat as expired 60 seconds early to allow preemptive refresh
  ;; before the token actually becomes invalid mid-request.
  (or (null spofy-auth--token-expiry)
      (<= spofy-auth--token-expiry (+ (float-time) 60))))

;;;; Token storage

(defun spofy-auth--persist-tokens ()
  "Persist tokens and expiry to `spofy-token-file'."
  (let ((data `((access-token . ,spofy-auth--access-token)
                (refresh-token . ,spofy-auth--refresh-token)
                (token-expiry . ,spofy-auth--token-expiry)))
        (file (expand-file-name spofy-token-file))
        (epa-file-select-keys 'silent))
    (condition-case err
        (let ((saved-umask (default-file-modes)))
          (unwind-protect
              (progn
                (set-default-file-modes #o600)
                (with-temp-file file
                  (let ((print-length nil)
                        (print-level nil))
                    (prin1 data (current-buffer)))))
            (set-default-file-modes saved-umask)))
      (error
       (message "spofy: failed to persist tokens to %s: %s" file
                (error-message-string err))))))

(defun spofy-auth--store-tokens (access-token refresh-token expires-in)
  "Store ACCESS-TOKEN, REFRESH-TOKEN and compute expiry from EXPIRES-IN.
EXPIRES-IN is the number of seconds until the access token expires.
Tokens are stored both in memory and in `spofy-token-file'."
  (setq spofy-auth--access-token access-token
        spofy-auth--refresh-token refresh-token
        spofy-auth--token-expiry (+ (float-time) expires-in))
  (spofy-auth--persist-tokens))

(defun spofy-auth--load-tokens ()
  "Load tokens from `spofy-token-file' into memory.
Returns non-nil if tokens were found."
  (let ((file (expand-file-name spofy-token-file)))
    (when (file-exists-p file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents file)
            (let ((data (read (current-buffer))))
              (when (listp data)
                (when-let* ((access (alist-get 'access-token data)))
                  (setq spofy-auth--access-token access))
                (when-let* ((refresh (alist-get 'refresh-token data)))
                  (setq spofy-auth--refresh-token refresh))
                (when-let* ((expiry (alist-get 'token-expiry data)))
                  (setq spofy-auth--token-expiry expiry))
                (and spofy-auth--access-token spofy-auth--refresh-token))))
        (error
         (message "spofy: failed to read token file %s: %s"
                  file (error-message-string err))
         nil)))))

;;;; Token access

(defun spofy-auth-access-token ()
  "Return the current access token, or nil if unavailable/expired.
If the token is expired and a refresh token is available,
attempts to refresh automatically."
  (cond
   ;; Token in memory and not expired
   ((and spofy-auth--access-token
         (not (spofy-auth--token-expired-p)))
    spofy-auth--access-token)
   ;; Token expired but we have a refresh token
   (spofy-auth--refresh-token
    (spofy-auth-refresh-token)
    (and (not (spofy-auth--token-expired-p))
         spofy-auth--access-token))
   ;; Nothing available
   (t nil)))

;;;; Token exchange

(defun spofy-auth--exchange-code (code callback)
  "Exchange authorization CODE for access and refresh tokens.
CALLBACK is called with (access-token refresh-token expires-in)
on success, or signals an error on failure."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data
         (url-build-query-string
          `(("grant_type" "authorization_code")
            ("code" ,code)
            ("redirect_uri" ,(spofy-auth--redirect-uri))
            ("client_id" ,spofy-client-id)
            ("client_secret" ,spofy-client-secret)))))
    (url-retrieve
     "https://accounts.spotify.com/api/token"
     (lambda (status)
       (let ((buf (current-buffer)))
         (unwind-protect
             (if (plist-get status :error)
                 (message "spofy: token exchange failed: %S" (plist-get status :error))
               (goto-char url-http-end-of-headers)
               (let* ((json (condition-case err
                                (json-parse-buffer :object-type 'alist)
                              (error
                               (message "spofy: failed to parse token response: %s"
                                        (error-message-string err))
                               nil)))
                      (access-token (and json (alist-get 'access_token json)))
                      (refresh-token (and json (alist-get 'refresh_token json)))
                      (expires-in (and json (alist-get 'expires_in json))))
                 (if access-token
                     (funcall callback access-token refresh-token expires-in)
                   (message "spofy: token exchange returned no access token: %S" json))))
           (kill-buffer buf)))))))

;;;; Token refresh

(defun spofy-auth-refresh-token ()
  "Use the refresh token to obtain a new access token.
Updates the stored tokens on success."
  (unless spofy-auth--refresh-token
    (error "spofy: No refresh token available; please re-authenticate with `spofy-authenticate'"))
  (unless (and spofy-client-id spofy-client-secret)
    (error "spofy: `spofy-client-id' and `spofy-client-secret' must be set"))
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data
         (url-build-query-string
          `(("grant_type" "refresh_token")
            ("refresh_token" ,spofy-auth--refresh-token)
            ("client_id" ,spofy-client-id)
            ("client_secret" ,spofy-client-secret)))))
    (when-let* ((buf (url-retrieve-synchronously
                      "https://accounts.spotify.com/api/token" t nil 10)))
      (unwind-protect
          (with-current-buffer buf
            (goto-char url-http-end-of-headers)
            (let* ((json (condition-case nil
                             (json-parse-buffer :object-type 'alist)
                           (json-parse-error nil)))
                   (access-token (and json (alist-get 'access_token json)))
                   (new-refresh (and json (alist-get 'refresh_token json)))
                   (expires-in (and json (alist-get 'expires_in json))))
              (if access-token
                  (spofy-auth--store-tokens access-token
                                            (or new-refresh spofy-auth--refresh-token)
                                            expires-in)
                (message "spofy: token refresh failed: %s"
                         (or (and json (alist-get 'error_description json))
                             (and json (alist-get 'error json))
                             "no access token in response")))))
        (kill-buffer buf)))))

;;;; Callback parsing

(defun spofy-auth--parse-callback (request)
  "Parse the OAuth2 callback REQUEST string.
Returns an alist of query parameters (code, state, error, etc.)."
  (when (string-match "GET /[^ ]*\\?\\([^ ]*\\) " request)
    (let ((query-string (match-string 1 request))
          (params nil))
      (dolist (pair (split-string query-string "&"))
        (when (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" pair)
          (push (cons (intern (match-string 1 pair))
                      (url-unhex-string (match-string 2 pair)))
                params)))
      (nreverse params))))

;;;; Local HTTP server

(defun spofy-auth--start-server ()
  "Start a temporary HTTP server on `spofy-redirect-port' for the OAuth2 callback."
  (when spofy-auth--server
    (spofy-auth--stop-server))
  (setq spofy-auth--server
        (make-network-process
         :name "spofy-auth-server"
         :server t
         :host "localhost"
         :service spofy-redirect-port
         :family 'ipv4
         :filter #'spofy-auth--server-filter
         :sentinel #'spofy-auth--server-sentinel
         :noquery t)))

(defun spofy-auth--stop-server ()
  "Stop the OAuth2 callback server."
  (when (and spofy-auth--server (process-live-p spofy-auth--server))
    (delete-process spofy-auth--server))
  (setq spofy-auth--server nil))

(defun spofy-auth--server-sentinel (_proc _event)
  "Sentinel for the auth server process.  No-op."
  nil)

(defun spofy-auth--server-filter (proc request)
  "Filter for the auth server.  Handle the OAuth2 callback in REQUEST.
PROC is the client connection process."
  (spofy-auth--handle-callback proc request))

(defun spofy-auth--handle-callback (proc request)
  "Handle an OAuth2 callback from PROC with REQUEST data.
Parses the authorization code and state, verifies state, exchanges
the code for tokens, and sends an HTTP response to the browser."
  (let ((params (spofy-auth--parse-callback request)))
    (cond
     ((null params)
      (spofy-auth--send-response proc 404 "Not found.")
      nil)
     ((alist-get 'error params)
      (spofy-auth--send-response proc 400
                                  "Authentication failed. You can close this window.")
      (spofy-auth--stop-server)
      (message "spofy: authentication denied by user: %s"
               (alist-get 'error params)))
     ((not (equal (alist-get 'state params) spofy-auth--state))
      (spofy-auth--send-response proc 400
                                  "Invalid state parameter. You can close this window.")
      (spofy-auth--stop-server)
      (message "spofy: OAuth2 state mismatch — possible CSRF attack"))
     ((alist-get 'code params)
      (spofy-auth--send-response proc 200
                                  "Authentication successful! You can close this window.")
      (spofy-auth--exchange-code
       (alist-get 'code params)
       (lambda (access-token refresh-token expires-in)
         (spofy-auth--store-tokens access-token refresh-token expires-in)
         (spofy-auth--stop-server)
         (message "spofy: authenticated successfully")
         (run-hooks 'spofy-authenticated-hook))))
     (t
      (spofy-auth--send-response proc 400
                                  "Missing authorization code. You can close this window.")
      (spofy-auth--stop-server)
      (message "spofy: callback received without authorization code")))))

(defun spofy-auth--send-response (proc status-code body)
  "Send an HTTP response to PROC with STATUS-CODE and BODY text."
  (let ((status-text (pcase status-code
                       (200 "OK")
                       (400 "Bad Request")
                       (404 "Not Found")
                       (_ "Error"))))
    (process-send-string
     proc
     (format "HTTP/1.1 %d %s\r\nContent-Type: text/html; charset=utf-8\r\nConnection: close\r\n\r\n<html><body><h2>%s</h2></body></html>"
             status-code status-text body))
    (when (process-live-p proc)
      (delete-process proc))))

;;;; Main entry point

;;;###autoload
(defun spofy-authenticate ()
  "Authenticate with Spotify via OAuth2.
Starts a local server, opens the browser to the authorization URL,
and waits for the callback."
  (interactive)
  (unless spofy-client-id
    (error "spofy: `spofy-client-id' is not set; configure it first"))
  (unless spofy-client-secret
    (error "spofy: `spofy-client-secret' is not set; configure it first"))
  (spofy-auth--start-server)
  (let ((url (spofy-auth--authorize-url)))
    (message "spofy: opening browser for authentication...")
    (browse-url url)))

(provide 'spofy-auth)
;;; spofy-auth.el ends here
