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


(defcustom zotra-server-path
  "http://127.0.0.1:1969"
  "The url and the port of the Zotero translation server to be used, without a trailing slash mark."
  :group 'zotra
  :type 'string)


(defcustom zotra-use-curl
  nil
  "Functions for retrieving citation entries use `url-retrieve-synchronously',
but it sometimes fails. An alternative is to use the external ???curl??? program
 to retrieve the data."
  :group 'zotra
  :type 'boolean)


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


(defcustom zotra-url-retrieve-timeout
  3
  "How many seconds to wait for server to get a response."
  :group 'zotra
  :type 'natnum)



(defun zotra-run-external-curl (data content-type url)
  (let* ((curl-cmd-string
          (format "curl --max-time '%s' -s --show-error -d '%s' -H 'Content-Type: %s' '%s'"
                  zotra-url-retrieve-timeout data content-type url))
         (stdout-buffer (get-buffer-create
                         (generate-new-buffer-name "stdout")))
         (stderr-buffer (get-buffer-create
                         (generate-new-buffer-name "stderr")))
         (return-code (shell-command curl-cmd-string stdout-buffer stderr-buffer))
         (out (with-current-buffer stdout-buffer (buffer-string)))
         (err (with-current-buffer stderr-buffer (buffer-string))))
    (kill-buffer stdout-buffer)
    (kill-buffer stderr-buffer)
    (when (not (= return-code 0))
      (user-error err))
    out))


(defun zotra-get-json (url-or-search-string &optional is-search)
  "Get citation data of URL-OR-SEARCH-STRING in Zotero JSON format."
  (let
      ((json
        (if zotra-use-curl
            (zotra-run-external-curl url-or-search-string
                                     "text/plain"
                                     (concat zotra-server-path
                                             (if is-search "/search" "/web")))
          (let*
              ((url-request-method "POST")
               (url-request-extra-headers '(("Content-Type" . "text/plain")))
               (url-request-data url-or-search-string)
               (response-buffer (url-retrieve-synchronously
                                 (concat zotra-server-path
                                         (if is-search "/search" "/web"))
                                 nil nil zotra-url-retrieve-timeout))
               (output
                (if (null response-buffer)
                    (user-error "Request failed. If this issue persists, try again with `zotra-use-curl'.")
                  (with-current-buffer response-buffer
                    (goto-char (point-min))
                    (search-forward "\n\n")
                    (delete-region (point-min) (point))
                    (buffer-string)))))
            (kill-buffer response-buffer)
            output))))
    (cond ((string= json "URL not provided")
           (user-error "URL not provided"))
          ((string= json "No identifiers found")
           (user-error "No identifiers found"))
          (t json))))


(defun zotra-get-entry-from-json (json &optional entry-format)
  "Convert Zotero JSON format to ENTRY-FORMAT or `zotra-default-entry-format'
if ENTR_FORMAT is nil."
  (let
      ((entry
        (if zotra-use-curl
            (zotra-run-external-curl url-or-search-string
                                     "application/json"
                                     (concat zotra-server-path "/export?format="
                                             (if (null entry-format)
                                                 zotra-default-entry-format
                                               entry-format)))
          (let*
              ((url-request-method "POST")
               (url-request-extra-headers '(("Content-Type" . "application/json")))
               (url-request-data json)
               (response-buffer (url-retrieve-synchronously
                                 (concat
                                  zotra-server-path
                                  "/export?format="
                                  (if (null entry-format) zotra-default-entry-format entry-format))
                                 nil nil zotra-url-retrieve-timeout))
               (output
                (if (null response-buffer)
                    (user-error "Request failed. If this issue persists, try again with `zotra-use-curl'.")
                  (with-current-buffer response-buffer
                    (goto-char (point-min))
                    (search-forward "\n\n")
                    (delete-region (point-min) (point))
                    (buffer-string)))))
            (kill-buffer response-buffer)
            output))))
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
                      (org-cite-list-bibliography-files))))))
    (save-window-excursion
      (find-file bibfile)
      (goto-char (point-max))
      (when (not (looking-at "^")) (insert "\n"))
      (insert (zotra-get-entry url-or-search-string is-search entry-format))
      (save-excursion
        (save-restriction
          (bibtex-narrow-to-entry)
          (bibtex-beginning-of-entry)
          (run-hooks 'zotra-after-add-entry-hook)))
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


(defun zotra-protocol (info)
  (let ((url (plist-get info :url))
        (bibfile (plist-get info :bibfile))
        (entry-format (plist-get info :format)))
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
