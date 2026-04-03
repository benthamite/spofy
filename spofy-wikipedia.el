;;; spofy-wikipedia.el --- Wikipedia lookup for Spofy  -*- lexical-binding: t; -*-

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

;; Wikipedia lookup for the currently playing track in Spofy.  For
;; non-classical music, searches the Wikipedia API directly for the album
;; article.  For classical music, uses an LLM (via gptel) to identify the
;; underlying musical work.  Results are cached in a SQLite database.

;;; Code:

(require 'url)
(require 'json)
(require 'cl-lib)

(declare-function spofy-api-get "spofy-api" (endpoint &optional params callback))
(declare-function spofy-player--poll-sync "spofy-player" ())
(declare-function gptel-request "gptel" (&optional prompt &rest args))
(declare-function gptel-get-backend "gptel" (name))
(declare-function gptel-backend-models "gptel-request" (backend))

(defvar spofy-player--current-state)
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel--known-backends)

;;;; Customization

(defcustom spofy-wikipedia-language "en"
  "Wikipedia language code for article lookups."
  :type 'string
  :group 'spofy)

(defcustom spofy-wikipedia-db-file
  (locate-user-emacs-file "spofy-wikipedia.sqlite")
  "SQLite database file for Wikipedia lookup cache."
  :type 'file
  :group 'spofy)

(defcustom spofy-wikipedia-backend nil
  "The gptel backend name for classical music lookups.
When nil, the backend is inferred from `spofy-wikipedia-model',
falling back to `gptel-backend'."
  :type '(choice (const :tag "Infer from model or use gptel default" nil)
                 (string :tag "Backend name"))
  :group 'spofy)

(defcustom spofy-wikipedia-model nil
  "The gptel model for classical music lookups.
When nil, defaults to `gptel-model'."
  :type '(choice (const :tag "Use gptel default" nil)
                 (symbol :tag "Model name"))
  :group 'spofy)

(defcustom spofy-wikipedia-fallback-to-artist t
  "Whether to fall back to the artist's Wikipedia page.
When non-nil and no Wikipedia article is found for the album or
musical work, open the artist's page instead of signaling an error."
  :type 'boolean
  :group 'spofy)

;;;; Constants

(defconst spofy-wikipedia--classical-genres
  '("classical" "orchestral" "opera" "baroque" "chamber music"
    "early music" "choral" "symphony" "romantic era"
    "post-romantic era" "contemporary classical" "modern classical"
    "neoclassicism" "impressionism" "minimalism"
    "classical performance" "art song")
  "Genre strings that indicate classical music.")

(defconst spofy-wikipedia--system-prompt
  "You are a music metadata assistant. Given a track name, album name, \
and artist, identify the underlying musical work and return its exact \
Wikipedia article title. Return ONLY the article title, with no \
explanation, quotes, or formatting. For example, given a track \
\"Symphony No. 5 in C minor, Op. 67: I. Allegro con brio\" from album \
\"Beethoven: Symphonies Nos. 5 & 7\" by \"Berlin Philharmonic\", return: \
Symphony No. 5 (Beethoven)"
  "System prompt for the classical music LLM lookup.")

;;;; SQLite database

(defvar spofy-wikipedia--db nil
  "Open SQLite database handle, or nil.")

(defun spofy-wikipedia--ensure-db ()
  "Open the SQLite database and create tables if needed.
Return the database handle."
  (unless (sqlitep spofy-wikipedia--db)
    (unless (fboundp 'sqlite-open)
      (user-error "Spofy: Emacs was built without SQLite support"))
    (setq spofy-wikipedia--db (sqlite-open spofy-wikipedia-db-file))
    (sqlite-execute
     spofy-wikipedia--db
     "CREATE TABLE IF NOT EXISTS wikipedia_cache (
        track       TEXT NOT NULL DEFAULT '',
        album       TEXT NOT NULL DEFAULT '',
        artist      TEXT NOT NULL,
        work        TEXT NOT NULL DEFAULT '',
        wiki_title  TEXT NOT NULL,
        wiki_url    TEXT NOT NULL,
        entity_type TEXT NOT NULL DEFAULT 'album',
        created_at  INTEGER NOT NULL,
        PRIMARY KEY (track, album, artist, entity_type))")
    (sqlite-execute
     spofy-wikipedia--db
     "CREATE TABLE IF NOT EXISTS artist_classification (
        artist_id    TEXT NOT NULL PRIMARY KEY,
        is_classical INTEGER NOT NULL,
        created_at   INTEGER NOT NULL)"))
  spofy-wikipedia--db)

;;;; Cache operations

(defun spofy-wikipedia--cache-lookup (track album artist entity-type)
  "Look up a cached Wikipedia entry.
Return an alist with keys `wiki-title', `wiki-url', `work', or nil."
  (let* ((db (spofy-wikipedia--ensure-db))
         (rows (sqlite-select
                db
                "SELECT wiki_title, wiki_url, work FROM wikipedia_cache
                 WHERE track = ? AND album = ? AND artist = ?
                   AND entity_type = ?"
                (list track album artist entity-type))))
    (when rows
      (let ((row (car rows)))
        `((wiki-title . ,(nth 0 row))
          (wiki-url   . ,(nth 1 row))
          (work       . ,(nth 2 row)))))))

(defun spofy-wikipedia--cache-store (track album artist entity-type
                                           work wiki-title wiki-url)
  "Store a Wikipedia lookup result in the cache."
  (let ((db (spofy-wikipedia--ensure-db)))
    (sqlite-execute
     db
     "INSERT OR REPLACE INTO wikipedia_cache
        (track, album, artist, work, wiki_title, wiki_url, entity_type, created_at)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
     (list track album artist (or work "") wiki-title wiki-url
           entity-type (round (float-time))))))

;;;; Artist classification

(defun spofy-wikipedia--classification-cached-p (artist-id)
  "Return the cached classical classification for ARTIST-ID, or \\='unknown."
  (let* ((db (spofy-wikipedia--ensure-db))
         (rows (sqlite-select
                db
                "SELECT is_classical FROM artist_classification
                 WHERE artist_id = ?"
                (list artist-id))))
    (if rows
        (= 1 (caar rows))
      'unknown)))

(defun spofy-wikipedia--cache-classification (artist-id is-classical)
  "Cache the IS-CLASSICAL boolean for ARTIST-ID."
  (let ((db (spofy-wikipedia--ensure-db)))
    (sqlite-execute
     db
     "INSERT OR REPLACE INTO artist_classification
        (artist_id, is_classical, created_at)
      VALUES (?, ?, ?)"
     (list artist-id (if is-classical 1 0) (round (float-time))))))

(defconst spofy-wikipedia--classical-track-patterns
  (concat
   "\\(?:"
   "Op\\. *[0-9]"                     ; opus numbers
   "\\|BWV *[0-9]"                    ; Bach catalog
   "\\|K\\. *[0-9]"                   ; Mozart catalog
   "\\|KV *[0-9]"                     ; Mozart catalog (alt)
   "\\|Hob\\."                        ; Haydn catalog
   "\\|D\\. *[0-9]"                   ; Schubert catalog
   "\\|RV *[0-9]"                     ; Vivaldi catalog
   "\\|S\\. *[0-9]"                   ; Liszt catalog
   "\\|HWV *[0-9]"                    ; Handel catalog
   "\\|WWV *[0-9]"                    ; Wagner catalog
   "\\|No\\. *[0-9]"                  ; numbered works
   "\\|in [A-G][#b]? [Mm]\\(ajor\\|inor\\)" ; key signatures
   "\\|: I\\. "                       ; movement numbers
   "\\|: II\\. "
   "\\|: III\\. "
   "\\|: IV\\. "
   "\\|Allegro\\|Adagio\\|Andante\\|Presto\\|Largo\\|Moderato"
   "\\|Scherzo\\|Menuett?o\\|Rondo"
   "\\)")
  "Regexp matching common classical music markers in track names.")

(defun spofy-wikipedia--classical-genre-p (genres)
  "Return non-nil if GENRES (a vector of strings) contains a classical genre."
  (cl-some (lambda (genre)
             (let ((g (downcase genre)))
               (cl-some (lambda (pattern)
                          (string-match-p (regexp-quote pattern) g))
                        spofy-wikipedia--classical-genres)))
           genres))

(defun spofy-wikipedia--classical-track-p (track-name)
  "Return non-nil if TRACK-NAME contains classical music markers."
  (and track-name
       (string-match-p spofy-wikipedia--classical-track-patterns track-name)))

(defun spofy-wikipedia--classify-artist (artist-id track-name callback)
  "Determine if ARTIST-ID is classical and call CALLBACK with the boolean.
Uses the cache first, then falls back to the Spotify API.  When the
artist has no genres, falls back to heuristic matching on TRACK-NAME."
  (let ((cached (spofy-wikipedia--classification-cached-p artist-id)))
    (if (not (eq cached 'unknown))
        (funcall callback cached)
      (require 'spofy-api)
      (spofy-api-get
       (format "artists/%s" artist-id) nil
       (lambda (data)
         (let* ((genres (or (alist-get 'genres data) []))
                (classical (if (= 0 (length genres))
                               (spofy-wikipedia--classical-track-p track-name)
                             (spofy-wikipedia--classical-genre-p genres))))
           (spofy-wikipedia--cache-classification artist-id classical)
           (funcall callback classical)))))))

;;;; Wikipedia API

(defun spofy-wikipedia--api-url (params)
  "Build a Wikipedia API URL from PARAMS alist."
  (concat (format "https://%s.wikipedia.org/w/api.php?" spofy-wikipedia-language)
          (mapconcat (lambda (pair)
                       (format "%s=%s"
                               (url-hexify-string (car pair))
                               (url-hexify-string (cdr pair))))
                     params "&")))

(defun spofy-wikipedia--api-get (params callback)
  "Make an async GET request to the Wikipedia API with PARAMS.
Call CALLBACK with the parsed JSON response."
  (let ((url (spofy-wikipedia--api-url params))
        (url-request-method "GET")
        (url-show-status nil))
    (url-retrieve
     url
     (lambda (_status cb)
       (let ((data nil))
         (save-excursion
           (goto-char (point-min))
           (when (re-search-forward "\r?\n\r?\n" nil t)
             (condition-case nil
                 (setq data (json-parse-string
                             (buffer-substring-no-properties (point) (point-max))
                             :object-type 'alist :array-type 'array))
               (json-parse-error nil))))
         (kill-buffer (current-buffer))
         (funcall cb data)))
     (list callback)
     t nil)))

(defun spofy-wikipedia--search-album (album artist callback)
  "Search Wikipedia for ALBUM by ARTIST.
Call CALLBACK with (WIKI-TITLE . WIKI-URL) or nil if not found."
  (spofy-wikipedia--api-get
   `(("action" . "query")
     ("list" . "search")
     ("srsearch" . ,(format "%s %s album" album artist))
     ("srlimit" . "1")
     ("format" . "json"))
   (lambda (data)
     (let* ((search (alist-get 'search (alist-get 'query data)))
            (title (and (> (length search) 0)
                        (alist-get 'title (aref search 0)))))
       (if title
           (spofy-wikipedia--validate-infobox
            title "Template:Infobox album" callback)
         (funcall callback nil))))))

(defun spofy-wikipedia--search-artist (artist callback)
  "Search Wikipedia for ARTIST.
Call CALLBACK with (WIKI-TITLE . WIKI-URL) or nil if not found."
  (spofy-wikipedia--api-get
   `(("action" . "query")
     ("list" . "search")
     ("srsearch" . ,(format "%s musician" artist))
     ("srlimit" . "1")
     ("format" . "json"))
   (lambda (data)
     (let* ((search (alist-get 'search (alist-get 'query data)))
            (title (and (> (length search) 0)
                        (alist-get 'title (aref search 0)))))
       (if title
           (spofy-wikipedia--validate-infobox
            title "Template:Infobox musical artist" callback)
         (funcall callback nil))))))

(defun spofy-wikipedia--validate-infobox (title template callback)
  "Check if Wikipedia article TITLE uses TEMPLATE.
Call CALLBACK with (TITLE . URL) if valid, nil otherwise."
  (spofy-wikipedia--api-get
   `(("action" . "query")
     ("titles" . ,title)
     ("prop" . "templates")
     ("tltemplates" . ,template)
     ("format" . "json"))
   (lambda (data)
     (let* ((pages (alist-get 'pages (alist-get 'query data)))
            (page (cdar pages))
            (templates (alist-get 'templates page)))
       (if templates
           (funcall callback
                    (cons title (spofy-wikipedia--article-url title)))
         (funcall callback nil))))))

(defun spofy-wikipedia--validate-title (title callback)
  "Validate that Wikipedia article TITLE exists, resolving redirects.
Call CALLBACK with (RESOLVED-TITLE . URL) or nil."
  (spofy-wikipedia--api-get
   `(("action" . "query")
     ("titles" . ,title)
     ("redirects" . "1")
     ("format" . "json"))
   (lambda (data)
     (let* ((query (alist-get 'query data))
            (pages (alist-get 'pages query))
            (page (cdar pages))
            (missing (alist-get 'missing page)))
       (if (eq missing :null)
           (funcall callback nil)
         (if missing
             (funcall callback nil)
           (let ((resolved (alist-get 'title page)))
             (if resolved
                 (funcall callback
                          (cons resolved
                                (spofy-wikipedia--article-url resolved)))
               (funcall callback nil)))))))))

(defun spofy-wikipedia--article-url (title)
  "Return the Wikipedia article URL for TITLE."
  (format "https://%s.wikipedia.org/wiki/%s"
          spofy-wikipedia-language
          (url-hexify-string (replace-regexp-in-string " " "_" title))))

;;;; LLM integration (classical path)

(defun spofy-wikipedia--find-backend-for-model (model)
  "Return the gptel backend that provides MODEL, or nil."
  (cl-loop for (_name . backend) in gptel--known-backends
           when (member model (gptel-backend-models backend))
           return backend))

(defun spofy-wikipedia--resolve-backend-and-model ()
  "Return (BACKEND . MODEL) for the LLM request."
  (let* ((model (or spofy-wikipedia-model gptel-model))
         (backend (cond
                   (spofy-wikipedia-backend
                    (gptel-get-backend spofy-wikipedia-backend))
                   (spofy-wikipedia-model
                    (or (spofy-wikipedia--find-backend-for-model
                         spofy-wikipedia-model)
                        gptel-backend))
                   (t gptel-backend))))
    (cons backend model)))

(defun spofy-wikipedia--llm-lookup (track album artist callback)
  "Use an LLM to identify the musical work for TRACK on ALBUM by ARTIST.
Call CALLBACK with the Wikipedia article title string, or nil on failure."
  (unless (require 'gptel nil t)
    (user-error "Spofy: gptel is required for classical music Wikipedia lookup"))
  (let* ((resolved (spofy-wikipedia--resolve-backend-and-model))
         (gptel-backend (car resolved))
         (gptel-model (cdr resolved))
         (prompt (format "Track: %s\nAlbum: %s\nArtist: %s" track album artist)))
    (gptel-request prompt
      :system spofy-wikipedia--system-prompt
      :callback
      (lambda (response info)
        (if (not response)
            (progn
              (message "Spofy: LLM request failed: %s"
                       (plist-get info :status))
              (funcall callback nil))
          (funcall callback (string-trim response)))))))

;;;; Main lookup logic

(defun spofy-wikipedia--fallback-or-error (artist message callback)
  "If `spofy-wikipedia-fallback-to-artist' is non-nil, look up ARTIST.
Otherwise signal a `user-error' with MESSAGE.  CALLBACK is called with
the artist result on success."
  (if spofy-wikipedia-fallback-to-artist
      (spofy-wikipedia--lookup-artist artist callback)
    (user-error "%s" message)))

(defun spofy-wikipedia--lookup-album (album artist callback)
  "Look up the Wikipedia article for non-classical ALBUM by ARTIST.
Call CALLBACK with (WIKI-TITLE . WIKI-URL) or signal an error."
  (let ((cached (spofy-wikipedia--cache-lookup "" album artist "album")))
    (if cached
        (funcall callback (cons (alist-get 'wiki-title cached)
                                (alist-get 'wiki-url cached)))
      (spofy-wikipedia--search-album
       album artist
       (lambda (result)
         (if result
             (progn
               (spofy-wikipedia--cache-store
                "" album artist "album" "" (car result) (cdr result))
               (funcall callback result))
           (spofy-wikipedia--fallback-or-error
            artist
            (format "Spofy: no Wikipedia article found for album \"%s\"" album)
            callback)))))))

(defun spofy-wikipedia--lookup-work (track album artist callback)
  "Look up the Wikipedia article for classical TRACK on ALBUM by ARTIST.
Call CALLBACK with (WIKI-TITLE . WIKI-URL) or signal an error."
  (let ((cached (spofy-wikipedia--cache-lookup track album artist "work")))
    (if cached
        (funcall callback (cons (alist-get 'wiki-title cached)
                                (alist-get 'wiki-url cached)))
      (spofy-wikipedia--llm-lookup
       track album artist
       (lambda (llm-title)
         (if (not llm-title)
             (spofy-wikipedia--fallback-or-error
              artist "Spofy: LLM failed to identify the musical work" callback)
           (spofy-wikipedia--validate-title
            llm-title
            (lambda (result)
              (if result
                  (progn
                    (spofy-wikipedia--cache-store
                     track album artist "work"
                     llm-title (car result) (cdr result))
                    (funcall callback result))
                (spofy-wikipedia--fallback-or-error
                 artist
                 (format "Spofy: Wikipedia article \"%s\" not found" llm-title)
                 callback))))))))))

(defun spofy-wikipedia--lookup-artist (artist callback)
  "Look up the Wikipedia article for ARTIST.
Call CALLBACK with (WIKI-TITLE . WIKI-URL) or signal an error."
  (let ((cached (spofy-wikipedia--cache-lookup "" "" artist "artist")))
    (if cached
        (funcall callback (cons (alist-get 'wiki-title cached)
                                (alist-get 'wiki-url cached)))
      (spofy-wikipedia--search-artist
       artist
       (lambda (result)
         (if result
             (progn
               (spofy-wikipedia--cache-store
                "" "" artist "artist" "" (car result) (cdr result))
               (funcall callback result))
           (user-error "Spofy: no Wikipedia article found for artist \"%s\""
                       artist)))))))

;;;; Interactive command

;;;###autoload
(defun spofy-wikipedia ()
  "Open the Wikipedia article for the currently playing album or work."
  (interactive)
  (require 'spofy-player)
  (unless spofy-player--current-state
    (spofy-player--poll-sync))
  (let* ((state spofy-player--current-state)
         (track (alist-get 'track state))
         (album (alist-get 'album state))
         (artist (alist-get 'artist state))
         (artist-id (alist-get 'artist-id state)))
    (unless track
      (user-error "Spofy: no track currently playing"))
    (unless artist-id
      (user-error "Spofy: no artist information available"))
    (spofy-wikipedia--classify-artist
     artist-id track
     (lambda (classical)
       (if classical
           (spofy-wikipedia--lookup-work
            track album artist
            (lambda (result) (browse-url (cdr result))))
         (spofy-wikipedia--lookup-album
          album artist
          (lambda (result) (browse-url (cdr result)))))))))

;;;; Embark action helpers

;;;###autoload
(defun spofy-wikipedia-track (target)
  "Open the Wikipedia article for the album/work of TARGET track."
  (when-let* ((entity (get-text-property 0 'spofy-entity target))
              (artists (alist-get 'artists entity))
              (artist (aref artists 0))
              (artist-id (alist-get 'id artist))
              (artist-name (alist-get 'name artist))
              (track-name (alist-get 'name entity))
              (album (alist-get 'album entity))
              (album-name (alist-get 'name album)))
    (spofy-wikipedia--classify-artist
     artist-id track-name
     (lambda (classical)
       (if classical
           (spofy-wikipedia--lookup-work
            track-name album-name artist-name
            (lambda (result) (browse-url (cdr result))))
         (spofy-wikipedia--lookup-album
          album-name artist-name
          (lambda (result) (browse-url (cdr result)))))))))

;;;###autoload
(defun spofy-wikipedia-album (target)
  "Open the Wikipedia article for TARGET album."
  (when-let* ((entity (get-text-property 0 'spofy-entity target))
              (album-name (alist-get 'name entity))
              (artists (alist-get 'artists entity))
              (artist (aref artists 0))
              (artist-name (alist-get 'name artist)))
    (spofy-wikipedia--lookup-album
     album-name artist-name
     (lambda (result) (browse-url (cdr result))))))

;;;###autoload
(defun spofy-wikipedia-artist (target)
  "Open the Wikipedia article for TARGET artist."
  (when-let* ((entity (get-text-property 0 'spofy-entity target))
              (artist-name (alist-get 'name entity)))
    (spofy-wikipedia--lookup-artist
     artist-name
     (lambda (result) (browse-url (cdr result))))))

(provide 'spofy-wikipedia)
;;; spofy-wikipedia.el ends here
