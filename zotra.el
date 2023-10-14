;;; zotra.el --- Import bibliographic data from (almost) everywhere  -*- lexical-binding: t; -*-


;; Author: Mohammad Pedramfar <https://github.com/mpedramfar>
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/mpedramfar/zotra


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides functions to fetch bibliographic information
;; in different formats (e.g. bibtex) using website urls or search
;; identifiers (e.g. DOI). It also provides functions to fetch
;; attachments (e.g. PDF files) associated with a url.
;;
;; This is done using ZOtero TRAnslators, but without using the Zotero client.
;;
;;
;; See the README.md file for details.


;;; Code:
(require 'url)
(require 'org-element)
(require 'org-protocol)
(require 'bibtex)


(defgroup zotra nil
  "Customization group for Zotra."
  :group 'editing)


(defcustom zotra-default-bibliography
  nil
  "Default bibliography file or list of files.

If this variable contains a single file, interactive calls to
`zotra-add-entry' will not ask the user for the bibfile.
Otherwise, Zotra will ask the user to choose one."
  :group 'zotra
  :type '(choice file (repeat file)))


(defcustom zotra-url-redirect-functions
  '(zotra-url-redirect--arxiv)
  "Currently, the Zotero translation server can't handle links to attachments.
\(See https://github.com/zotero/translation-server/issues/70\).
These functions provide a way to fix the issue by manually changing the link to
an attachment to a link to the url for the article.
Each function in this list should take a url and return a url. If the function
is not applicable, it should return its input without change."
  :group 'zotra
  :type 'hook)


(define-obsolete-variable-alias
  'zotra-after-add-entry-hook 'zotra-after-get-bibtex-entry-hook
  "[2023-08-18 Fri]")

(defcustom zotra-after-get-bibtex-entry-hook
  '(bibtex-clean-entry)
  "These functions are called after `zotra-get-entry'.
They take no arguments, and they can be used to cleanup and format new entries.

These hooks are run only if ENTRY-FORMAT is \"bibtex\" or \"biblatex\"."
  :group 'zotra
  :type 'hook)


(defcustom zotra-backend
  'citoid
  "Backend used by Zotra.
CITOID: The Wikimedia Citoid server.
ZOTRA-SERVER: An instance of the Zotra server.
TRANSLATION-SERVER: An instance of the Zotero translation server.
ZOTRA-CLI: Zotra server used as a command-line program."
  :group 'zotra
  :type '(choice
          (const citoid)
          (const zotra-server)
          (const translation-server)
          (const zotra-cli)))


(defcustom zotra-server-path
  "http://127.0.0.1:1969"
  "The url and the port of the Zotero translation server to be used.
This variable should not end in a trailing slash mark.
This is only relevant when `zotra-backend' is translation-server
or zotra-server."
  :group 'zotra
  :type 'string)


(defcustom zotra-local-server-directory
  nil
  "The directory of the local server.
When this variable is non-nil, Zotra will attempt to start the
server. Set this variable to nil if you are using a remote server
or if you want to run the server yourself.
This is only relevant when `zotra-backend' is translation-server
or zotra-server."
  :group 'zotra
  :type '(choice (const nil) directory))


(defcustom zotra-use-curl
  nil
  "Use the external curl program.
This is only relevant when `zotra-backend' is either citoid,
translation-server or zotra-server."
  :group 'zotra
  :type 'string)


(defcustom zotra-cli-command
  '("zotra")
  "The command to run the Zotra server as a cli program.
The command should be entered as a list of strings where the
first element is the command and the rest are its arguments."
  :group 'zotra
  :type '(repeat string))


(defcustom zotra-url-retrieve-timeout
  15
  "How many seconds to wait for server to get a response.
This is only relevant when `zotra-backend' is is either citoid or
translation-server."
  :group 'zotra
  :type 'natnum)

(defconst zotra-citoid-supported-formats
  '("mediawiki"
    "zotero"
    "bibtex"
    "mediawiki-basefields")
  "List of entry formats supported by the citoid backend.")

(defconst zotra-translation-server-supported-formats
  '("bibtex"
    "biblatex"
    "bookmarks"
    "coins"
    "csljson"
    "csv"
    "endnote_xml"
    "evernote"
    "mods"
    "rdf_bibliontology"
    "rdf_dc"
    "rdf_zotero"
    "refer"
    "refworks_tagged"
    "ris"
    "tei"
    "wikipedia"
    "zotero")
"List of entry formats supported by translation-server, zotra-server or zotra-cli backends.

See https://github.com/zotero/translation-server#export-translation
and https://github.com/zotero/translation-server/blob/master/src/formats.js

Note that the value \"zotero\" is not listed in the above links and
corresponds to Zotero JSON format.")


(defcustom zotra-default-entry-format
  "bibtex"
  "The bibliography format.

Can be one of the value in `zotra-citoid-supported-formats'
or `zotra-translation-server-supported-formats'"
  :group 'zotra
  :type 'string)


(defcustom zotra-multiple-item-strategy
  'ask
  "What should Zotra do when a url corresponds to multiple entries.
SINGLE: capture the url as a single entry.
MULTIPLE: ask user which entry in the page should be captures.
ASK: ask user if the url should be captured as a single entry or not."
  :group 'zotra
  :type '(choice (const single) (const multiple) (const ask)))


(defcustom zotra-protocol-multiple-item-strategy
  'single
  "What should Zotra protocol do when a url corresponds to multiple entries.
SINGLE: capture the url as a single entry.
MULTIPLE: ask user which entry in the page should be captures.
ASK: ask user if the url should be captured as a single entry or not."
  :group 'zotra
  :type '(choice (const single) (const multiple) (const ask)))


(defcustom zotra-download-attachment-default-directory
  (expand-file-name "zotra-attachment-dir" temporary-file-directory)
  "The default download directory (or directories) for attachments.
Used in `zotra-download-attachment'."
  :group 'zotra
  :type '(choice directory (repeat directory)))



(defun zotra-run-external-command (cmd &optional silent-error)
  "Run the external command described by the list CMD and return the stdout.
If SILENT-ERROR is nil and the command fails, raise a user-error."
  (with-temp-buffer
    (let* ((stderr-file (make-temp-file "zotra-stderr-"))
           (return-code
            (apply #'call-process
                   (append
                    (list (car cmd)
                          nil
                          (list (current-buffer) stderr-file)
                          nil)
                    (cdr cmd))))
           (out (buffer-string))
           (err (with-temp-buffer (insert-file-contents stderr-file) (buffer-string))))
      (delete-file stderr-file)
      (if (and (not (= return-code 0))
               (null silent-error))
          (user-error
           (if err
               err
             (format "Command '%s' failed with return code '%s'" cmd return-code)))
        (when err (message "%s" err)))
      out)))


(defun zotra-retrieve-synchronously (url &optional headers-alist data response-handler)
  (let (response-code output)
    (if zotra-use-curl
        (let ((output-file (make-temp-file "zotra-output-")))
          (setq response-code
                (string-to-number
                 (zotra-run-external-command
                  (flatten-list
                   (list "curl"
                         "--compressed"
                         "-w" "%{http_code}"
                         "-o" output-file
                         "--max-time" (format "%s" zotra-url-retrieve-timeout)
                         "-s" "--show-error"
                         (when data (list "-d" (format "%s" data)))
                         (mapcar (lambda (h)
                                   (list "-H" (format "%s: %s" (car h) (cdr h))))
                                 headers-alist)
                         url)))))
          (setq output
                (with-temp-buffer
                  (insert-file-contents output-file) (buffer-string)))
          (delete-file output-file))
      (condition-case e
          (let* ((url-request-method (if data "POST" "GET"))
                 (url-request-extra-headers headers-alist)
                 (url-request-data (when data (encode-coding-string data 'utf-8)))
                 (response-buffer
                  (url-retrieve-synchronously url nil nil
                                              zotra-url-retrieve-timeout)))
            (unless response-buffer
              (user-error "Request failed. If this issue persists, try changing `zotra-use-curl' or `zotra-backend'"))
            (setq response-code
                  (with-current-buffer response-buffer
                    (and (boundp 'url-http-response-status) url-http-response-status)))
            (setq output
                  (with-temp-buffer
                    (url-insert-buffer-contents response-buffer url)
                    (buffer-string))))
        (file-error
         (user-error (concat (format "%s\n" e)
                             (unless zotra-local-server-directory
                               "The variable `zotra-local-server-directory' is nil. ")
                             (format "Are you sure a server is running on `%s'?" zotra-server-path))))))
    (if response-handler
        (funcall response-handler response-code output))
    output))


(defvar zotra-local-server-process nil)
(defvar zotra-local-server-process-directory nil)
(defvar zotra-local-server-process-buffer-name "*zotra-local-server*")


(defun zotra-maybe-start-local-server ()
  (when (and zotra-local-server-directory
             (member zotra-backend '(translation-server zotra-server)))
    (let ((start (not (process-live-p zotra-local-server-process))))
      (when (and (process-live-p zotra-local-server-process)
                 (not (equal zotra-local-server-process-directory
                             zotra-local-server-directory)))
        (zotra-kill-local-server)
        (setq start t))
      (when start
        (message "Starting local %s..." zotra-backend)
        (setq zotra-local-server-process
              (make-process
               :name "zotra-server-process"
               :buffer (and zotra-local-server-process-buffer-name
                            (get-buffer-create
                             zotra-local-server-process-buffer-name))
               :noquery t
               :command (list "npm" "start" "--prefix"
                              zotra-local-server-directory)))
        (setq zotra-local-server-process-directory
              zotra-local-server-directory)
        (sleep-for 2)))))


(defun zotra-kill-local-server ()
  (when (process-live-p zotra-local-server-process)
    (kill-process zotra-local-server-process)))


;; For `zotra-server', `translation-server' and `zotra-cli' backends
(defun zotra-contact-server (data content-type endpoint &optional param)
  (cond 
   ((member zotra-backend '(translation-server zotra-server))
    (zotra-retrieve-synchronously
     (concat zotra-server-path "/" endpoint
             (when param (format "?%s=%s" (car param) (cdr param))))
     `(("Content-Type" . ,content-type)) data))
   ((equal zotra-backend 'zotra-cli)
    (zotra-run-external-command
     (append
      zotra-cli-command
      (if param
          (if (equal "single" (car param))
              (list "--single")
            (list (format "--%s=%s" (car param) (cdr param))))
        (when (equal content-type "application/json")
          (list "--json")))
      (list endpoint data))
     t))))


;; For `zotra-server', `translation-server' and `zotra-cli' backends
(defun zotra-get-json (data &optional endpoint)
  "Get bibliographic data of DATA in Zotero JSON format.

ENDPOINT must be \"search\" or \"web\". Any value other than \"search\" will
be treated as \"web\"."
  (let* ((j (or (zotra-contact-server
                 data "text/plain" (or endpoint "web")
                 (when (and (eq zotra-multiple-item-strategy 'single)
                            (not (equal endpoint "search")))
                   '("single" . "1")))
                "")))
    (condition-case nil
        (let* ((p (json-parse-string j :object-type 'alist :array-type 'list))
               (p-items (assoc 'items p)))
          (cond
           ((null p-items)
            j)
           ((and (not (eq zotra-multiple-item-strategy 'multiple))
                 (yes-or-no-p "Capture the page as a single item? "))
            (zotra-contact-server
             data
             "text/plain" "web" '("single" . "1")))
           (t
            (let* ((candidates
                    (cl-loop
                     for item in (cdr p-items)
                     collect
                     (replace-regexp-in-string
                      "," ";" (format "%s --- %s" (car item) (cdr item)))))
                   (choices
                    (cl-loop
                     for c in (completing-read-multiple "Select: " candidates nil t)
                     collect
                     (nth (cl-position c candidates :test #'equal) (cdr p-items)))))
              (setcdr p-items choices)
              (zotra-contact-server (json-serialize p) "application/json" "web")))))
      (json-parse-error
       (user-error "JSON parse error: %s" j)))))


;; For `zotra-server', `translation-server' and `zotra-cli' backends
(defun zotra-get-entry-from-json (json entry-format)
  "Convert Zotero JSON format to ENTRY-FORMAT.

This function does NOT run the hooks in `zotra-after-get-bibtex-entry-hook'."
  (let* ((entry (zotra-contact-server
                 json "application/json" "export" `("format" . ,entry-format))))
    (cond ((string= entry "Bad Request")
           (user-error "Bad Request"))
          ((string= entry "An error occurred during translation. Please check translation with the Zotero client.")
           (user-error "An error occurred during translation"))
          (t entry))))


(defun zotra-get-entry-1 (data entry-format &optional endpoint)
  (zotra-maybe-start-local-server)
  (cond
   ((equal zotra-backend 'citoid)
    (zotra-retrieve-synchronously
     (format "https://en.wikipedia.org/api/rest_v1/data/citation/%s/%s"
             entry-format
             (url-hexify-string data))
     nil nil
     (lambda (response-code output)
       (cl-case response-code
         (200 nil)
         (404
          (user-error "404: Citation data was not found" response-code output))
         (t
          (user-error "Citoid server responded with code: %s\n%s"
                      response-code
                      (or (ignore-errors
                            (string-join
                             (mapcar
                              (lambda (it) (format "%s: %s" (car it) (cdr it)))
                              (json-parse-string output :object-type 'alist))
                             "\n"))
                          output)))))))
   ((not (member zotra-backend '(citoid zotra-cli zotra-server translation-server)))
    nil)
   ((equal entry-format "zotero")
    (zotra-get-json data endpoint))
   (t
    (zotra-get-entry-from-json
     (zotra-get-json data endpoint) entry-format))))
  

(defun zotra-query-url-or-search-string (&optional url-or-search-string)
  "Ask the user where to get bibliographic data from.

If URL-OR-SEARCH-STRING is nil, ask the user.
If it is a url, ensure that url is redirected properly using
`zotra-url-redirect-functions'."
  (let* ((input (or url-or-search-string
                    (read-string
                     (if (equal zotra-backend 'citoid)
                         "URL or search identifier (DOI, PMCID, PMID or QID): "
                       "URL or search identifier (DOI, ISBN, PMID, arXiv ID): ")
                     (ignore-errors (current-kill 0 t))))))
    (if (not (string-match-p "https?://" input))
        (cons input "search")
      (mapc (lambda (x)
              (setq input (funcall x input)))
            zotra-url-redirect-functions)
      (cons input "web"))))


(defun zotra-get-entry (&optional url-or-search-string entry-format)
  "Get bibliography entry.

Return the entry for URL-OR-SEARCH-STRING in the format ENTRY-FORMAT or
`zotra-default-entry-format' if ENTRY_FORMAT is nil.
If the format is not supported by the current backend, default to \"bibtex\".

When ENTRY-FORMAT is \"bibtex\" or \"biblatex\", this function runs the hooks
in `zotra-after-get-bibtex-entry-hook' before returning its output."
  (let* ((zotra-backend
          (if (member zotra-backend '(citoid zotra-cli zotra-server translation-server))
              zotra-backend
            (message "Unrecognised backend. Trying `citoid'...")
            'citoid))
         (query-result
          (zotra-query-url-or-search-string url-or-search-string))
         (data (car query-result))
         (endpoint (cdr query-result))
         (entry-format (or entry-format
                           zotra-default-entry-format))
         (entry-format
          (if (member entry-format
                      (if (equal zotra-backend 'citoid)
                          zotra-citoid-supported-formats
                        zotra-translation-server-supported-formats))
              entry-format
            (message (concat
                      "The entry format `%s' is not supported by the backend `%s'.\n"
                      "Using `bibtex' instead...")
                     entry-format zotra-backend)
            "bibtex"))
         (entry-bibtex-dialect
          (cond
           ((equal entry-format "bibtex") 'BibTeX)
           ((equal entry-format "biblatex") 'biblatex))))
  (with-temp-buffer
    (insert "\n" (zotra-get-entry-1 data entry-format endpoint))
    (goto-char (point-min))
    (when entry-bibtex-dialect
      (bibtex-mode)
      (bibtex-set-dialect entry-bibtex-dialect t)
      (while (bibtex-next-entry)
        (save-excursion
          (save-restriction
            (bibtex-narrow-to-entry)
            (dolist (h zotra-after-get-bibtex-entry-hook)
              (bibtex-beginning-of-entry)
              (ignore-errors (funcall h)))))))
    (buffer-string))))


(defun zotra-add-entry (&optional url-or-search-string entry-format bibfile)
  "Add bibliography entry for URL-OR-SEARCH-STRING to BIBFILE.

Pass URL-OR-SEARCH-STRING and ENTRY-FORMAT to `zotra-get-entry'
to get the entry.

If BIBFILE is the symbol `here', then insert entry at point.
If BIBFILE is nil, use `zotra-default-bibliography'.
If `zotra-default-bibliography' is also nil, ask the user to choose
the bib file.

Return the last bibtex key of the added entries."
  (interactive)
  (let ((bibfile
         (or bibfile
             (and (stringp zotra-default-bibliography) zotra-default-bibliography)
             (and (listp zotra-default-bibliography)
                  (not (cdr zotra-default-bibliography))
                  (car zotra-default-bibliography))
             (completing-read
              "Bibfile: "
              (append zotra-default-bibliography
                      (directory-files "." t ".*\\.bib\\'")
                      (and (fboundp #'org-cite-list-bibliography-files)
                           (org-cite-list-bibliography-files))))))
        (entry (zotra-get-entry url-or-search-string entry-format))
        last-key)
    (if (equal bibfile 'here)
        (progn
          (insert entry)
          (save-excursion
            (bibtex-previous-entry)
            (setq last-key (cdr (assoc "=key=" (bibtex-parse-entry))))))
      (with-current-buffer (or (find-buffer-visiting bibfile)
                               (find-file-noselect bibfile))
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-max))
            (when (not (looking-at "^")) (insert "\n"))
            (insert entry)
            (bibtex-previous-entry)
            (setq last-key (cdr (assoc "=key=" (bibtex-parse-entry))))))
        (save-buffer)))
    last-key))


(define-obsolete-function-alias
  'zotra-add-entry-from-url 'zotra-add-entry
  "[2023-09-30 Fri]")


(define-obsolete-function-alias
  'zotra-add-entry-from-search 'zotra-add-entry
  "[2023-09-30 Fri]")


;; attachments


(defun zotra-get-attachments-1 (data &optional endpoint json)
  (let ((attachments
         (cl-case zotra-backend
           (zotra-cli
            (let* ((endpoint (or endpoint "web"))
                   (cli-output (zotra-run-external-command
                                (append
                                 zotra-cli-command
                                 (list "-a")
                                 (when json "-j")
                                 (list endpoint data))
                                t)))
              (cl-loop
               for item in (split-string cli-output "\n")
               for trimmed-item = (string-trim item)
               if (not (equal trimmed-item ""))
               collect trimmed-item)))
           (zotra-server
            (zotra-maybe-start-local-server)
            (let* ((j (zotra-get-json data endpoint))
                   (p (json-parse-string j :object-type 'alist :array-type 'list)))
              (flatten-tree
               (mapcar (lambda (item)
                         (and (equal (cdr (assoc 'itemType item)) "attachment")
                              (not (equal (cdr (assoc 'mimeType item)) "text/html"))
                              (cdr (assoc 'url item))))
                       p)))))))
    (if attachments
        attachments
      (user-error "Zotra failed to find any attachments in page"))))


(defun zotra-get-attachment (&optional url-or-search-string all)
  "Get the attachment URLs for the URL-OR-SEARCH-STRING.
If IS-SEARCH is nil, treat URL-OR-SEARCH-STRING as a url. Otherwise, treat it
as a search identifier.
If ALL is non-nil, return the list of attachments."
  (let* ((zotra-backend
          (if (member zotra-backend '(zotra-cli zotra-server))
              zotra-backend
            (message "Fetching attachments is only supported with `zotra-cli' and `zotra-server' backends.")
            (message "Trying `zotra-server'...")
            'zotra-server))
         (query-result
          (zotra-query-url-or-search-string url-or-search-string))
         (data (car query-result))
         (endpoint (cdr query-result))
         (attachments (zotra-get-attachments-1 data endpoint)))
    (if all
        attachments
      (if (cdr attachments)
          (completing-read "Which attachment? " attachments nil t)
        (car attachments)))))


(defun zotra-correct-file-extension? (path)
  "Check if the file extension of PATH matches its mime type."
  (if (not (and (executable-find "file") (file-exists-p "/etc/mime.types")))
      t  ;; we have no way of checking, so we assume it's correct!
    (let* ((mime-type
            (replace-regexp-in-string
             "\n$" ""
             (zotra-run-external-command
              (list "file" "-b" "--mime-type" path))))
           (correct-extensions
            (split-string
             (replace-regexp-in-string
              mime-type " "
              (with-temp-buffer
                (insert-file-contents "/etc/mime.types")
                (goto-char (point-min))
                (keep-lines (format "^%s" mime-type))
                (buffer-string)))
             nil t)))
      (when (member (file-name-extension path) correct-extensions)
        t))))


(defun zotra-download-attachment (&optional url-or-search-string download-dir filename confirm-filename)
  "Download the attachments for URL-OR-SEARCH-STRING.

If URL-OR-SEARCH-STRING is nil, ask the user.
If FILENAME does not contain directory, use the directory DOWNLOAD-DIR.
If DOWNLOAD-DIR is nil, use `zotra-download-attachment-default-directory'.
If `zotra-download-attachment-default-directory' is also nil, ask for
the download directory.
If FILENAME is nil or CONFIRM-FILENAME is non-nil, ask for the filename
to save.

Return the path to the downloaded attachment."
  (interactive)
  (let* ((attachment-url (zotra-get-attachment url-or-search-string))
         (download-dir (expand-file-name
                        (or (when filename (file-name-directory filename))
                            download-dir
                            (and (stringp zotra-download-attachment-default-directory)
                                 zotra-download-attachment-default-directory)
                            (and (listp zotra-download-attachment-default-directory)
                                 (not (cdr zotra-download-attachment-default-directory))
                                 (car zotra-download-attachment-default-directory))
                            (and zotra-download-attachment-default-directory
                                 (completing-read "Where to save? "
                                                  zotra-download-attachment-default-directory))
                            (and (not zotra-download-attachment-default-directory)
                                 (read-directory-name "Where to save? ")))))
         (filename (or (when (and filename (not confirm-filename))
                         (expand-file-name filename download-dir))
                       (expand-file-name
                        (completing-read
                         "Rename attachment to: " nil nil nil
                         (or filename
                             (car (last (split-string attachment-url "/" t)))))
                        download-dir))))
    (mkdir (file-name-directory filename) t)
    (url-copy-file attachment-url filename 1)
    (unless (file-exists-p filename)
      (browse-url attachment-url)
      (user-error "Failed to download file. Opening the attachment in browser"))
    (unless (zotra-correct-file-extension? filename)
      (delete-file filename)
      (browse-url attachment-url)
      (user-error "The attachment file seems to be corrupted. Opening the attachment in browser"))
    filename))


(define-obsolete-function-alias
  'zotra-download-attachment-from-url 'zotra-download-attachment
  "[2023-09-30 Fri]")


(define-obsolete-function-alias
  'zotra-download-attachment-from-search 'zotra-download-attachment
  "[2023-09-30 Fri]")


;; zotra-protocol


(defun zotra-protocol (info)
  (let ((url (plist-get info :url))
        (bibfile (plist-get info :bibfile))
        (entry-format (plist-get info :format))
        (zotra-multiple-item-strategy zotra-protocol-multiple-item-strategy))
    (message "Zotra received: `%s' to be saved in `%s'"
             url (or bibfile "zotra-default-bibliography"))
    (zotra-add-entry url entry-format bibfile)
    nil))


(add-to-list 'org-protocol-protocol-alist
             '("zotra-protocol"
               :protocol "zotra"
               :function zotra-protocol))


;; zotra-url-redirect-functions


(defun zotra-url-redirect--arxiv (url)
  (if (string-match
       "^\\(https?://\\)?\\(www\.\\)?arxiv.org/pdf/\\(.*\\)\.pdf" url)
      (concat
       "https://arxiv.org/abs/"
       (match-string-no-properties 3 url))
    url))


;;; Zotra + bibtex-completion


(declare-function bibtex-completion-add-pdf-to-library "bibtex-completion")
(declare-function bibtex-completion-get-entry "bibtex-completion")
(declare-function bibtex-completion-get-value "bibtex-completion")
(defvar bibtex-completion-library-path)
(defvar bibtex-completion-pdf-extension)
(defvar bibtex-completion-fallback-options)


(defcustom zotra-bibtex-completion-confirm-url
  t
  "Prompt user to confirm url before downloading attachment."
  :group 'zotra
  :type 'boolean)


(defun zotra--bibtex-completion-add-pdf-to-library (keys)
  "Add a PDF to the library for the first entry in KEYS.
The PDF can be added either from an open buffer, a file, a
URL, or using Zotra."
  (let* ((key (car keys))
         (source (char-to-string
                  (read-char-choice "Add pdf from [b]uffer, [f]ile, [u]rl, or [z]otra? " '(?b ?f ?u ?z))))
         (buffer (when (string= source "b")
                   (read-buffer-to-switch "Add pdf buffer: ")))
         (file (when (string= source "f")
                 (expand-file-name (read-file-name "Add pdf file: " nil nil t))))
         (url (cond
               ((string= source "u")
                (read-string "Add pdf URL: "))
               ((string= source "z")
                (let* ((entry (bibtex-completion-get-entry key))
                       (url-field (string-trim
                                   (or (bibtex-completion-get-value "url" entry) ""))))
                  (zotra-get-attachment
                   (if (and (< 0 (length url-field))
                            (or (not zotra-bibtex-completion-confirm-url)
                                (y-or-n-p
                                 (format "Use '%s' with Zotra? " url-field))))
                       url-field
                     (read-string "URL to use with Zotra: ")))))))
         (path (flatten-list bibtex-completion-library-path))
         (path (if (cdr path)
                   (completing-read "Add pdf to: " path nil t)
                 (car path)))
         (pdf (expand-file-name (completing-read "Rename pdf to: "
                                                 (mapcar (lambda (it) (concat key it))
                                                         (flatten-list bibtex-completion-pdf-extension))
                                                 nil nil key)
                                path)))
    (cond
     (buffer
      (with-current-buffer buffer
        (write-file pdf t)))
     (file
      (copy-file file pdf 1))
     (url
      (url-copy-file url pdf 1)))))


(defun zotra-bibtex-completion ()
  "Integrate Zotra with bibtex-completion."
  (interactive)
  (add-to-list 'bibtex-completion-fallback-options
               '("Add entry from URL or search identifier   (zotra.el)"
                 . zotra-add-entry))
  (defalias #'bibtex-completion-add-pdf-to-library
    #'zotra--bibtex-completion-add-pdf-to-library))


;; The end
(provide 'zotra)

;;; zotra.el ends here
