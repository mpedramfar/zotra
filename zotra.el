;;; zotra.el --- Library to use Zotero translators      -*- lexical-binding: t; -*-


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

;; This library provides functions to connect to a local instance of the Zotero
;; translation server and fetch bibliographic information in different formats
;; (e.g. bibtex) using website urls or search identifiers (e.g. DOI).
;; It also provides functions to retrieve attachments using urls.
;;
;; See the README.md file for details.


;;; Code:
(require 'url)
(require 'org-element)
(require 'org-protocol)
(require 'bibtex)


(defgroup zotra nil
  "Customization group for zotra."
  :group 'zotra)


(defcustom zotra-default-bibliography
  nil
  "Default bibliography file.

If this variable is non-nil, interactive calls to `zotra-add-entry-from-url'
and `zotra-add-entry-from-search' will not ask the user for the bibfile.
Otherwise, zotra will ask the user to choose one."
  :group 'zotra
  :type 'file)


(defcustom zotra-url-redirect-functions
  '(zotra-url-redirect--arxiv)
  "Currently, the Zotero translation server can't handle links to pdf files.
\(See https://github.com/zotero/translation-server/issues/70\).
These functions provide a way to fix the issue by manually changing the link to
a pdf to a link to another url for the article.
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
  'zotra-cli
  "Backend used by zotra.
TRANSLATION-SERVER: A local instance of translation server.
CURL_TRANSLATION-SERVER: External curl program and a local instance
  of translation server.
ZOTRA-CLI: The external zotra-cli program."
  :group 'zotra
  :type '(choice
          (const translation-server)
          (const curl_translation-server)
          (const zotra-cli)))


(defcustom zotra-server-path
  "http://127.0.0.1:1969"
  "The url and the port of the Zotero translation server to be used.
This variable should not end in a trailing slash mark.
This is only relevant when `zotra-backend' is TRANSLATION-SERVER
or CURL_TRANSLATION-SERVER"
  :group 'zotra
  :type 'string)


(defcustom zotra-cli-command
  '("zotra")
  "The command to run the external zotra-cli program.
The command should be entered as a list of strings where the
first element is the command and the rest are its arguments"
  :group 'zotra
  :type '(repeat string))


(defcustom zotra-url-retrieve-timeout
  3
  "How many seconds to wait for server to get a response.
This is only relevant when `zotra-backend' is TRANSLATION-SERVER
or CURL_TRANSLATION-SERVER"
  :group 'zotra
  :type 'natnum)


(defcustom zotra-default-entry-format
  "bibtex"
  "The bibliography format. Can be one of the following options:

bibtex
biblatex
bookmarks
coins
csljson
csv
endnote_xml
evernote
mods
rdf_bibliontology
rdf_dc
rdf_zotero
refer
refworks_tagged
ris
tei
wikipedia

See https://github.com/zotero/translation-server#export-translation
and https://github.com/zotero/translation-server/blob/master/src/formats.js"
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
  "The default download directory for attachments.
Used in `zotra-download-attachment'."
  :group 'zotra
  :type 'string)



(defun zotra-run-external-command (cmd &optional silent-error)
  "Run the external command described by the list CMD and return the stdout.
If SILENT-ERROR is not nil and the command fails, raise a user-error."
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


(defun zotra-contact-server (data content-type endpoint &optional param)
  (cond
   ((equal zotra-backend 'curl_translation-server)
    (zotra-run-external-command
     (list "curl"
           "--max-time" (format "%s" zotra-url-retrieve-timeout)
           "-s" "--show-error"
           "-d" (format "%s" data)
           "-H" (format "Content-Type: %s" content-type)
           (concat
            zotra-server-path "/" endpoint
            (when param (format "?%s=%s" (car param) (cdr param)))))))
   ((equal zotra-backend 'translation-server)
    (let*
        ((url-request-method "POST")
         (url-request-extra-headers `(("Content-Type" . ,content-type)))
         (url-request-data data)
         (response-buffer (url-retrieve-synchronously
                           (concat zotra-server-path "/" endpoint
                                   (when param (format "?%s=%s" (car param) (cdr param))))
                           nil nil zotra-url-retrieve-timeout))
         (output
          (if (null response-buffer)
              (user-error "Request failed. If this issue persists, try changing `zotra-backend'")
            (with-current-buffer response-buffer
              (goto-char (point-min))
              (search-forward "\n\n")
              (delete-region (point-min) (point))
              (buffer-string)))))
      (kill-buffer response-buffer)
      output))
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


(defun zotra-get-json (data &optional endpoint)
  "Get bibliographic data of DATA in Zotero JSON format.
ENDPOINT must be \"search\" or \"web\". Any value other than \"search\" will
be treated as \"web\"."
  (let* ((j (zotra-contact-server
             data "text/plain" (or endpoint "web")
             (when (and (eq zotra-multiple-item-strategy 'single)
                        (not (equal endpoint "search")))
               '("single" . "1")))))
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


(defun zotra-get-entry-from-json (json &optional entry-format)
  "Convert Zotero JSON format to ENTRY-FORMAT.
If ENTRY_FORMAT is nil, convert to `zotra-default-entry-format'.

This function does NOT run the hooks in `zotra-after-get-bibtex-entry-hook'."
  (let* ((entry-format (or entry-format
                           zotra-default-entry-format))
         (entry (zotra-contact-server
                 json "application/json" "export" `("format" . ,entry-format))))
    (cond ((string= entry "Bad Request")
           (user-error "Bad Request"))
          ((string= entry "An error occurred during translation. Please check translation with the Zotero client.")
           (user-error "An error occurred during translation"))
          (t entry))))


(defun zotra-query-url-or-search-string (&optional url-or-search-string is-search)
  "Ask the user where to get bibliographic data from.

If URL-OR-SEARCH-STRING and IS-SEARCH are nil, ask the user for the url.
If URL-OR-SEARCH-STRING is nil and IS-SEARCH is non-nil, ask for search
identifier.

If IS-SEARCH is nil, ensure that url is redirected properly using
`zotra-url-redirect-functions'."
  (let* ((url (and (not is-search)
                   (or url-or-search-string
                       (read-string
                        "url: "
                        (let ((link (and (equal major-mode 'org-mode)
                                         (org-element-context))))
                          (if (and link (eq (car link) 'link))
                              (org-element-property :raw-link link)
                            (ignore-errors (current-kill 0 t))))))))
         (identifier (and is-search
                          (or url-or-search-string
                              (read-string
                               "search identifier (DOI, ISBN, PMID, arXiv ID): "
                               (ignore-errors (current-kill 0 t)))))))
    (when url
      (mapc (lambda (x)
              (setq url (funcall x url)))
            zotra-url-redirect-functions))
    (cons (or url identifier)
          (if is-search "search" "web"))))


(defun zotra-get-entry (&optional url-or-search-string is-search entry-format)
  "Get bibliography entry.

If IS-SEARCH is nil, treat URL-OR-SEARCH-STRING as a url.
Otherwise, treat it as a search identifier.

Return the entry in the format ENTRY-FORMAT or `zotra-default-entry-format'
if ENTRY_FORMAT is nil.

When ENTRY-FORMAT is \"bibtex\" or \"biblatex\", this function runs the hooks
in `zotra-after-get-bibtex-entry-hook' before returning its output."
  (let* ((query-result (zotra-query-url-or-search-string
                        url-or-search-string is-search))
         (data (car query-result))
         (endpoint (cdr query-result))
         (entry-format (or entry-format
                           zotra-default-entry-format))
         (entry-bibtex-dialect
          (cond
           ((equal entry-format "bibtex") 'BibTeX)
           ((equal entry-format "biblatex") 'biblatex)
           (t nil))))
  (with-temp-buffer
    (insert (zotra-get-entry-from-json
             (zotra-get-json data endpoint) entry-format))
    (goto-char (point-min))
    (when entry-bibtex-dialect
      (bibtex-mode)
      (bibtex-set-dialect entry-bibtex-dialect t)
      (while (bibtex-next-entry)
        (save-excursion
          (save-restriction
            (bibtex-narrow-to-entry)
            (bibtex-beginning-of-entry)
            (ignore-errors
              (run-hooks 'zotra-after-get-bibtex-entry-hook))))))
    (buffer-string))))


(defun zotra-add-entry (&optional url-or-search-string is-search entry-format bibfile)
  (let ((bibfile
         (or bibfile zotra-default-bibliography
             (completing-read
              "Bibfile: "
              (append (directory-files "." t ".*\\.bib\\'")
                      (and (fboundp #'org-cite-list-bibliography-files)
                           (org-cite-list-bibliography-files))))))
        (entry (zotra-get-entry url-or-search-string is-search entry-format))
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


(defun zotra-add-entry-from-url (&optional url entry-format bibfile)
  "Add bibliography entry for URL to BIBFILE.

Pass URL-OR-SEARCH-STRING, IS-SEARCH and ENTRY-FORMAT to `zotra-get-entry'
to get the entry.

If BIBFILE is the symbol `here', then insert entry at point.
If BIBFILE is nil, use `zotra-default-bibliography'.
If `zotra-default-bibliography' is also nil, ask the user to choose
the bib file.

Return the last bibtex key of the added entries."
  (interactive)
  (zotra-add-entry url nil entry-format bibfile))


(defun zotra-add-entry-from-search (&optional identifier entry-format bibfile)
  "Add bibliography entry for search IDENTIFIER to BIBFILE.

Pass URL-OR-SEARCH-STRING, IS-SEARCH and ENTRY-FORMAT to `zotra-get-entry'
to get the entry.

If BIBFILE is the symbol `here', then insert entry at point.
If BIBFILE is nil, use `zotra-default-bibliography'.
If `zotra-default-bibliography' is also nil, ask the user to choose
the bib file.

Return the last bibtex key of the added entries."
  (interactive)
  (zotra-add-entry identifier t entry-format bibfile))


;; attachments


(defun zotra-get-attachments-1 (data &optional endpoint json)
  (let* ((endpoint (or endpoint "web"))
         (cli-output (zotra-run-external-command
                      (append
                       zotra-cli-command
                       (list "-a")
                       (when json "-j")
                       (list endpoint data))
                      t))
         (attachments (cl-loop
                       for item in (split-string cli-output "\n")
                       for trimmed-item = (string-trim item)
                       if (not (equal trimmed-item ""))
                       collect trimmed-item)))
    (if attachments
        attachments
      (user-error "Zotra failed to find any attachments in page"))))


(defun zotra-get-attachment (&optional url-or-search-string is-search all)
  "Get the attachments for the URL-OR-SEARCH-STRING.
If IS-SEARCH is nil, treat URL-OR-SEARCH-STRING as a url. Otherwise, treat it
as a search identifier.
If ALL is non-nil, return the list of attachments."
  (let* ((query-result (zotra-query-url-or-search-string
                        url-or-search-string is-search))
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


(defun zotra-download-attachment (attachment-url download-dir &optional filename confirm-filename)
  (let* ((download-dir (expand-file-name
                        (or (when filename (file-name-directory filename))
                            download-dir
                            zotra-download-attachment-default-directory
                            (read-directory-name "Where to save? "))))
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


(defun zotra-download-attachment-from-url (&optional url download-dir filename confirm-filename)
  "Download the attachments for the URL.

If URL is nil, ask the user.
If FILENAME does not contain directory, use the directory DOWNLOAD-DIR.
If DOWNLOAD-DIR is nil, use `zotra-download-attachment-default-directory'.
If `zotra-download-attachment-default-directory' is also nil, ask for
the download directory.
If FILENAME is nil or CONFIRM-FILENAME is non-nil, ask for the filename
to save.

Return the path to the downloaded attachment."
  (interactive)
  (zotra-download-attachment
   (zotra-get-attachment url)
   download-dir filename confirm-filename))


(defun zotra-download-attachment-from-search (&optional identifier download-dir filename confirm-filename)
  "Download the attachments for the IDENTIFIER.

If IDENTIFIER is nil, ask the user.
If FILENAME does not contain directory, use the directory DOWNLOAD-DIR.
If DOWNLOAD-DIR is nil, use `zotra-download-attachment-default-directory'.
If `zotra-download-attachment-default-directory' is also nil, ask for
the download directory.
If FILENAME is nil or CONFIRM-FILENAME is non-nil, ask for the filename
to save.

Return the path to the downloaded attachment."
  (interactive)
  (zotra-download-attachment
   (zotra-get-attachment identifier t)
   download-dir filename confirm-filename))


;; zotra-protocol


(defun zotra-protocol (info)
  (let ((url (plist-get info :url))
        (bibfile (plist-get info :bibfile))
        (entry-format (plist-get info :format))
        (zotra-multiple-item-strategy zotra-protocol-multiple-item-strategy))
    (message "Zotra received: `%s' to be saved in `%s'" url bibfile)
    (zotra-add-entry-from-url url entry-format bibfile)
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


;; The end
(provide 'zotra)

;;; zotra.el ends here
