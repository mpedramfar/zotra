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
;; translation server and fetch bibtex entries using website urls.

;;; Code:
(require 'url)
(require 'org-protocol)


(defgroup zotra nil
  "Customization group for zotra."
  :group 'zotra)


(defcustom zotra-default-bibliography
  nil
  "If this value is not nil, entries will only be added here.
Otherwise, zotra will look at the current directory 
for `.bib' files and also use `org-cite-list-bibliography-files'."
  :group 'zotra
  :type 'file)


(defcustom zotra-after-add-entry-hook
  nil
  "These functions are called after adding an entry.
They take no arguments, and they can be used to
 cleanup and format new entries."
  :group 'zotra
  :type 'hook)


(defcustom zotra-server-path
  "http://127.0.0.1:1969"
  "The url and the port of the Zotero translation server to be used."
  :group 'zotra
  :type 'string)


(defcustom zotra-use-curl
  nil
  "Function for retrieving bibtex entries use `url-retrieve-synchronously',
but it sometimes fails. An alternative is to use the external `curl' program
to retrieve the data."
  :group 'zotra
  :type 'boolean)


(defun zotra-get-json (url)
  "Get citation data of URL in Zotero JSON format, using url-retrieve-synchronously
and Zotero translation server."
  (let*
      ((url-request-method "POST")
       (url-request-extra-headers '(("Content-Type" . "text/plain")))
       (url-request-data url)
       (response-buffer (url-retrieve-synchronously
                         (concat zotra-server-path "/web")))
       (output (with-current-buffer response-buffer
                 (goto-char (point-min))
                 (search-forward "\n\n")
                 (delete-region (point-min) (point))
                 (buffer-string))))
    (kill-buffer response-buffer)
    (if (equal output "URL not provided")
        (user-error "URL not provided")
      output)))


(defun zotra-get-bibtex-from-json (json)
  "Convert Zotero JSON format to bibtex, using url-retrieve-synchronously
and Zotero translation server."
  (let*
      ((url-request-method "POST")
       (url-request-extra-headers '(("Content-Type" . "application/json")))
       (url-request-data json)
       (response-buffer (url-retrieve-synchronously
                         (concat zotra-server-path "/export?format=bibtex")))
       (output (with-current-buffer response-buffer
                 (goto-char (point-min))
                 (search-forward "\n\n")
                 (delete-region (point-min) (point))
                 (buffer-string))))
    (kill-buffer response-buffer)
    (if (equal output "Bad Request")
        (user-error "Bad Request")
      output)))


(defun zotra-get-bibtex (url)
  "Get bibtex data for URL using Zotero translation server."
  (if zotra-use-curl
      (shell-command-to-string
       (format "curl -s -d '%s' -H 'Content-Type: text/plain' '%s/web' | curl -s -d @- -H 'Content-Type: application/json' '%s/export?format=bibtex'"
               url zotra-server-path zotra-server-path))
    (zotra-get-bibtex-from-json (zotra-get-json url))))



(defun zotra-add-bibtex-entry (url &optional bibfile)
  "Get bibtex entry for URL using Zotero translation server. Then
add it to BIBFILE"
  (interactive
   (list (read-string
          "url: "
          (ignore-errors (current-kill 0 t)))))
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
      (insert (zotra-get-bibtex url))
      (save-excursion
        (save-restriction
          (bibtex-narrow-to-entry)
          (bibtex-beginning-of-entry)
          (run-hooks 'zotra-after-add-entry-hook)))
      (goto-char (point-max))
      (when (not (looking-at "^")) (insert "\n"))
      (save-buffer))))



(defun zotra-protocol (info)
  (let ((url (plist-get info :url))
        (bibfile (plist-get info :bibfile)))
    (message (format "Zotra received: `%s' to be saved in `%s'" url bibfile))
    (zotra-add-bibtex-entry url bibfile)
    nil))



(add-to-list 'org-protocol-protocol-alist
             '("zotra-protocol"
               :protocol "zotra"
               :function zotra-protocol))



;;* The end
(provide 'zotra)

;;; zotra.el ends here
