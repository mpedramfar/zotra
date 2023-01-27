;;; zotra.el --- Zotero utilities for Emacs      -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Mohammad Pedramfar

;; Author: Mohammad Pedramfar <https://github.com/mpedramfar>

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
;; this library provides functions to connect to a local instance of the Zotero
;; translation server and fetch citation information in different formats (e.g. bibtex)
;; using website urls or search identifiers.


;;; Code:
(require 'url)
(require 'org-protocol)
(require 'bibtex)

(defgroup zotra nil
  "Customization group for zotra."
  :group 'zotra)


(defcustom zotra-default-bibliography
  nil
  "If this variable is non-nil, interactive calls to
`zotra-add-entry-from-url' and `zotra-add-entry-from-search'
will not ask the user for the bibfile.
Otherwise, zotra will make a list containing the `.bib' files
in the current directory and the files returned by
`org-cite-list-bibliography-files' and ask the user to choose one."
  :group 'zotra
  :type 'file)


(defcustom zotra-url-cleanup-functions
  '(zotra-url-cleanup--arxiv)
  "Currently, the Zotero translation server can't handle links to pdf files.
(See https://github.com/zotero/translation-server/issues/70).
These functions provide a way to fix the issue by manually changing the link to
a pdf to a link to another url for the article.
Each function in this list should take a url and return a url. If the function is
not applicable, it should return its input without change."
  :group 'zotra
  :type 'hook)


(defcustom zotra-after-add-entry-hook
  '(bibtex-clean-entry)
  "These functions are called after adding an entry.
They take no arguments, and they can be used to
 cleanup and format new entries."
  :group 'zotra
  :type 'hook)


(defcustom zotra-backend
  'zotra-cli
  "Backend used by zotra.
TRANSLATION-SERVER: A local instance of translation server
CURL_TRANSLATION-SERVER: External curl program and a local instance of translation server
ZOTRA-CLI: The external zotra-cli program"
  :group 'zotra
  :type '(choice
          (const translation-server)
          (const curl_translation-server)
          (const zotra-cli)))


(defcustom zotra-server-path
  "http://127.0.0.1:1969"
  "The url and the port of the Zotero translation server to be used, without a trailing slash mark.
This is only relevant when `zotra-backend' is TRANSLATION-SERVER or CURL_TRANSLATION-SERVER"
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
This is only relevant when `zotra-backend' is TRANSLATION-SERVER or CURL_TRANSLATION-SERVER"
  :group 'zotra
  :type 'natnum)


(defcustom zotra-default-entry-format
  "bibtex"
  "The citation format. Can be one of the following options:

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
  "This variable determines how zotra handles a url
that corresponds to multiple entries.
SINGLE: capture the url as a single entry.
MULTIPLE: ask user which entry in the page should be captures.
ASK: ask user if the url should be captured as a single entry or not."
  :group 'zotra
  :type '(choice (const single) (const multiple) (const ask)))


(defcustom zotra-protocol-multiple-item-strategy
  'single
  "This variable determines how zotra-protocol handles a url
that corresponds to multiple entries.
SINGLE: capture the url as a single entry.
MULTIPLE: ask user which entry in the page should be captures.
ASK: ask user if the url should be captured as a single entry or not."
  :group 'zotra
  :type '(choice (const single) (const multiple) (const ask)))


(defcustom zotra-download-attachment-default-directory
  "/tmp/zotra-attachment-dir"
  "The default directory used to download attachments when
using `zotra-download-attachment' or `zotra-open-attachment'."
  :group 'zotra
  :type 'string)



(defun zotra-run-external-command (cmd &optional silent-error)
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


(defun zotra-run-external-curl (data content-type url)
  (zotra-run-external-command
   (list "curl"
         "--max-time" (format "%s" zotra-url-retrieve-timeout)
         "-s" "--show-error"
         "-d" (format "%s" data)
         "-H" (format "'Content-Type: %s'" content-type)
         (format "%s" url))))


(defun zotra-contact-server (data content-type endpoint &optional param)
  (cond
   ((equal zotra-backend 'curl_translation-server)
    (zotra-run-external-curl data
                             content-type
                             (concat
                              zotra-server-path "/" endpoint
                              (when param (format "?%s=%s" (car param) (cdr param))))))
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
              (user-error "Request failed. If this issue persists, try changing `zotra-backend'.")
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


(defun zotra-get-json (url-or-search-string &optional is-search)
  "Get citation data of URL-OR-SEARCH-STRING in Zotero JSON format."
  (let* ((j (zotra-contact-server
             url-or-search-string
             "text/plain"
             (if is-search "search" "web")
             (when (and (eq zotra-multiple-item-strategy 'single)
                        (not is-search))
               '("single" . "1"))))
         (p (json-parse-string j :object-type 'alist :array-type 'list))
         (p-items (assoc 'items p)))
    (cond
     ((null p-items)
      j)
     ((and (not (eq zotra-multiple-item-strategy 'multiple))
           (yes-or-no-p "Capture the page as a single item? "))
      (zotra-contact-server
       url-or-search-string
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
               (nth (cl-position c candidates :test 'equal) (cdr p-items)))))
        (setcdr p-items choices)
        (zotra-contact-server (json-serialize p) "application/json" "web"))))))


(defun zotra-get-entry-from-json (json &optional entry-format)
  "Convert Zotero JSON format to ENTRY-FORMAT or `zotra-default-entry-format'
if ENTRY_FORMAT is nil."
  (let* ((entry-format (if (null entry-format)
                     zotra-default-entry-format
                   entry-format))
         (entry (zotra-contact-server
                 json "application/json" "export" `("format" . ,entry-format))))
    (cond ((string= entry "Bad Request")
           (user-error "Bad Request"))
          ((string= entry "An error occurred during translation. Please check translation with the Zotero client.")
           (user-error "An error occurred during translation"))
          (t entry))))


(defun zotra-get-entry (url-or-search-string &optional is-search entry-format)
  (zotra-get-entry-from-json (zotra-get-json url-or-search-string is-search) entry-format))


(defun zotra-add-entry (url-or-search-string &optional is-search bibfile entry-format)
  (let ((bibfile
         (or bibfile zotra-default-bibliography
             (completing-read
              "Bibfile: "
              (append (directory-files "." t ".*\\.bib$")
                      (org-cite-list-bibliography-files)))))
        (entry (zotra-get-entry url-or-search-string is-search entry-format)))
    (save-window-excursion
      (find-file bibfile)
      (goto-char (point-max))
      (when (not (looking-at "^")) (insert "\n"))
      (save-excursion (insert entry))
      (while (bibtex-next-entry)
        (save-excursion
          (save-restriction
            (bibtex-narrow-to-entry)
            (bibtex-beginning-of-entry)
            (ignore-errors
              (run-hooks 'zotra-after-add-entry-hook)))))
      (goto-char (point-max))
      (when (not (looking-at "^")) (insert "\n"))
      (save-buffer))))


(defun zotra-add-entry-from-url (url &optional bibfile entry-format)
  "Get citation entry for URL using Zotero translation server. Then
add it to BIBFILE.

If BIBFILE is nil, use `zotra-default-bibliography'.
If `zotra-default-bibliography' is nil, ask the user to choose between
the .bib files in the current directory and the files returned by
`org-cite-list-bibliography-files'.

ENTRY-FORMAT can take the same values as `zotra-default-entry-format'.
If ENTRY-FORMAT is nil, use `zotra-default-entry-format'."
  (interactive
   (list (read-string
          "url: "
          (ignore-errors (current-kill 0 t)))))
  (mapc (lambda (x)
          (setq url (funcall x url)))
        zotra-url-cleanup-functions)
  (zotra-add-entry url nil bibfile entry-format))


(defun zotra-add-entry-from-search (identifier &optional bibfile entry-format)
  "Get citation entry for search IDENTIFIER (DOI, ISBN, PMID, arXiv ID)
using Zotero translation server. Then add it to BIBFILE.

If BIBFILE is nil, use `zotra-default-bibliography'.
If `zotra-default-bibliography' is nil, ask the user to choose between
the .bib files in the current directory and the files returned by
`org-cite-list-bibliography-files'.

ENTRY-FORMAT can take the same values as `zotra-default-entry-format'.
If ENTRY-FORMAT is nil, use `zotra-default-entry-format'."
  (interactive
   (list (read-string
          "search identifier (DOI, ISBN, PMID, arXiv ID): "
          (ignore-errors (current-kill 0 t)))))
  (zotra-add-entry identifier t bibfile entry-format))


;; attachments


(defun zotra-get-attachments (data &optional endpoint json)
  (let* ((endpoint (or endpoint "web"))
         (cli-output
          (zotra-run-external-command
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


(defun zotra-download-attachment-from-url (&optional url download-dir)
  "Download the attachments for the URL to DOWNLOAD-DIR.
If URL is nil and point is at an org-mode link, use the link.
If DOWNLOAD-DIR is nil, use `zotra-download-attachment-default-directory'.
If `zotra-download-attachment-default-directory' is also nil, prompt for the download directory."
  (interactive)
  (let* ((url (or url
                  (read-string
                   "url: "
                   (let ((link (org-element-context)))
                     (if (and link
                              (eq (car link) 'link))
                         (org-element-property :raw-link link)
                       (ignore-errors (current-kill 0 t)))))))
         (attachments (zotra-get-attachments url))
         (attachment (if attachments
                         (completing-read
                          "Which attachment to open? "
                          attachments nil t)
                       (user-error "zotra failed to find any attachments in page")))
         (download-dir (expand-file-name
                        (or download-dir
                            zotra-download-attachment-default-directory
                            (read-directory-name
                             "Where to save? "))))
         (pdf (expand-file-name
               (completing-read
                "Rename attachment to: " nil nil nil
                (car (last (split-string attachment "/" t))))
               download-dir)))
    (mkdir (file-name-directory pdf) t)
    (url-copy-file attachment pdf 1)
    pdf))


(defun zotra-download-attachment-from-search (&optional identifier download-dir)
  "Download the attachments for the IDENTIFIER to DOWNLOAD-DIR.
If DOWNLOAD-DIR is nil, use `zotra-download-attachment-default-directory'.
If `zotra-download-attachment-default-directory' is also nil, prompt for the download directory."
  (interactive)
  (let* ((identifier
          (read-string
           "search identifier (DOI, ISBN, PMID, arXiv ID): "
           (ignore-errors (current-kill 0 t))))
         (attachments (zotra-get-attachments identifier "search"))
         (attachment (if attachments
                         (completing-read
                          "Which attachment to open? "
                          attachments nil t)
                       (user-error "zotra failed to find any attachments in page")))
         (download-dir (expand-file-name
                        (or download-dir
                            zotra-download-attachment-default-directory
                            (read-directory-name
                             "Where to save? "))))
         (pdf (expand-file-name
               (completing-read
                "Rename attachment to: " nil nil nil
                (car (last (split-string attachment "/" t))))
               download-dir)))
    (mkdir (file-name-directory pdf) t)
    (url-copy-file attachment pdf 1)
    pdf))


(defun zotra-open-attachment-from-url (&optional url download-dir)
  "Use `zotra-download-attachment-from-url' to download attachments and open them using `find-file'.
See `zotra-download-attachment-from-url' for more details."
  (interactive)
  (let ((pdf (funcall #'zotra-download-attachment-from-url url download-dir)))
    (when pdf
      (find-file pdf))))


(defun zotra-open-attachment-from-search (&optional identifier download-dir)
  "Use `zotra-download-attachment-from-search' to download attachments and open them using `find-file'.
See `zotra-download-attachment-from-search' for more details."
  (interactive)
  (let ((pdf (funcall #'zotra-download-attachment-from-search identifier download-dir)))
    (when pdf
      (find-file pdf))))


;; zotra-protocol


(defun zotra-protocol (info)
  (let ((url (plist-get info :url))
        (bibfile (plist-get info :bibfile))
        (entry-format (plist-get info :format))
        (zotra-multiple-item-strategy zotra-protocol-multiple-item-strategy))
    (message (format "Zotra received: `%s' to be saved in `%s'" url bibfile))
    (zotra-add-entry-from-url url bibfile entry-format)
    nil))


(add-to-list 'org-protocol-protocol-alist
             '("zotra-protocol"
               :protocol "zotra"
               :function zotra-protocol))


;; zotra-url-cleanup-functions


(defun zotra-url-cleanup--arxiv (url)
  (if (string-match
       "^\\(https?://\\)?\\(www\.\\)?arxiv.org/pdf/\\(.*\\)\.pdf" url)
      (concat
       "https://arxiv.org/abs/"
       (match-string-no-properties 3 url))
    url))


;;* The end
(provide 'zotra)

;;; zotra.el ends here
