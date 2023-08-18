# Zotra

[![MELPA](https://melpa.org/packages/zotra-badge.svg)](https://melpa.org/#/zotra)

This emacs library provides functions to get bibliographic information from a url and save it into a bibtex file.
It also provides a way to obtain a list of attachments (e.g. PDF files) associated with a url.
This is done using [Zotero translators](https://www.zotero.org/support/translators), but without using the Zotero client.

## Changelog

- [2023-07-23 Sun]: Some internal functions have changed. `zotra-url-cleanup-functions` is renamed to `zotra-url-redirect-functions`.
- [2023-01-27 Fri]: `zotra-cli-command` now takes a list of strings instead of a single string.

## Instalation

### Zotra

The prefered instllation method is from MELPA.

Alternatively, you can clone the repository and add the following line to your init file.

``` emacs-lisp
(add-to-list 'load-path "/path/to/zotra")
(require 'zotra)
```

### Backend

Note that this library tries to connect to a backend to do the translation.
The choice of backend can be customized through `zotra-backend` variable.

There are 3 options for `zotra-backend`:
- `zotra-cli`

Use the external [zotra-cli](https://github.com/mpedramfar/zotra-cli) library for translation.
This is the recommended backend to use as **it is the only backend that is able to fetch attachments**.
See [zotra-cli](https://github.com/mpedramfar/zotra-cli) repository for its installation guide.

Note that if `zotra-cli` is installed locally (i.e. the `zotra` command is not available from your shell), then you can customize `zotra-cli-command` to point at the installed library:
```elisp
(setq zotra-cli-command '("node" "/path/to/zotra-cli/bin/index.js"))
```


- `translation-server`

Use emacs url libraries to connect to a running instance of zotero translation server.
See [zotero translation server](https://github.com/zotero/translation-server/) repository for its installation guide.

- `curl_translation-server`

Use the external curl program to connect to a running instance of zotero translation server.
Sometimes the previous backend fails. In this case you can try this backend.
To use this backend you need to install the zotero translation server and the curl program.

## Usage and configuration

From inside emacs, run `zotra-add-entry-from-url`, then enter the url and choose the bibtex file to save into.
Alternatively, you can run `zotra-add-entry-from-search`, then enter the search identifier (DOI, ISBN, PMID, arXiv ID) and choose the bibtex file to save into.

To download attachments from a url or search identifier, run `zotra-download-attachment-from-url`/`zotra-download-attachment-from-search`.

See `zotra` customization group for a complete list of options and their description.

### Automatically download attachment after adding a bibtex entry

When the point is at a bibtex entry, the following function downloads the attachment for it and adds the filename to a bibtex field named "File".
```emacs-lisp
(defun zotra-download-attachment-for-current-entry ()
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((entry (bibtex-parse-entry t))
           (key (cdr (assoc "=key=" entry)))
           (url (cdr (assoc "url" entry)))
           (filename (concat key ".pdf"))
           (filename (when entry
                       (zotra-download-attachment-from-url
                        url nil filename))))
      (when filename
        (bibtex-make-field (list "File" nil filename) t)
        (save-buffer)))))
```

This function can be added to `zotra-after-add-entry-hook`. 
*Note that this function chooses the bibtex key as the filename so it should be be after any hook that might change the bibtex key.*
When it is added to this hook, any time a bibtex entry is added, its attachments will also be downloaded.
Alternatively, you can add the following function to your init file to add an entry from url and download its attachments.

```emacs-lisp
(defun zotra-add-entry-from-url-and-download-attachment (&optional url)
  (interactive)
  (let ((zotra-after-add-entry-hook
         (append zotra-after-add-entry-hook
                 '(zotra-download-attachment-for-current-entry))))
    (zotra-add-entry-from-url url)))
```

### Using zotra with a browser and org-protocol

First you need to set up [org-protocol](https://orgmode.org/worg/org-contrib/org-protocol.html). [This section](https://www.orgroam.com/manual.html#Installation-_00281_0029) of the `org-roam` manual describes how to set it up on Linux, macOS, and Windows.

Then make a bookmark in your browser with the following url:
```
javascript:location.href=('org-protocol://zotra?url='+ encodeURIComponent(location.href)+'&bibfile='+encodeURIComponent('/path/to/bibfile.bib')+'&format=my-save-format').replace(/'/gi,"%27")
```
where `/path/to/bibfile.bib` should be replaced with the path to the bibfile and `my-save-format` should be replaced with one of the acceptable formats listed in the description of `zotra-default-entry-format`.

You can omit format and bibfile to use the value of `zotra-default-entry-format` and `zotra-default-bibliography` respectively:
```
javascript:location.href=('org-protocol://zotra?url='+ encodeURIComponent(location.href)).replace(/'/gi,"%27")
```
Now you can click on the bookmark in any page and the bibliographic information of the page will be saved into emacs automatically.

### Using zotra with [org-ref](https://github.com/jkitchin/org-ref)

The functions in `zotra-after-add-entry-hook` are called after adding an entry.
They take no arguments, and they can be used to cleanup and format new entries.
For example, if you are using org-ref, you could add this line to your init file:
```emacs-lisp
(add-hook 'zotra-after-add-entry-hook 'org-ref-clean-bibtex-entry)
```

### Using zotra with [bibtex-completion](https://github.com/tmalsburg/helm-bibtex/)

If you are using bibtex-completion, you can add the following functions to your init file.
These functions allow you to use zotra to add a pdf to your library.

The variable `bibtex-completion-add-pdf-field-after-add-to-library` determines if the pdf path should be added to a field named `bibtex-completion-pdf-field` in the bibtex entry.
See [bibtex-completion](https://github.com/tmalsburg/helm-bibtex/) for more details about `bibtex-completion-pdf-field`.

```emacs-lisp
(setq bibtex-completion-add-pdf-field-after-add-to-library t)

(defun bibtex-completion-add-pdf-field (key pdf)
  (save-window-excursion
    (bibtex-completion-show-entry (list key))
    (bibtex-make-field (list bibtex-completion-pdf-field nil pdf) t)
    (save-buffer)))
```

Next we override the function `bibtex-completion-add-pdf-to-library` so that it provides the option to use Zotra.

```emacs-lisp
(defun bibtex-completion-add-pdf-to-library (keys)
  "Add a PDF to the library for the first entry in KEYS.
The PDF can be added either from an open buffer, a file, a
URL, or using Zotra."
  (let* ((key (car keys))
         (source (char-to-string
                  (if (fboundp 'zotra-get-attachment)
                      (read-char-choice "Add pdf from [b]uffer, [f]ile, [u]rl, or [z]otra? " '(?b ?f ?u ?z))
                    (read-char-choice "Add pdf from [b]uffer, [f]ile, or [u]rl? " '(?b ?f ?u)))))
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
                            (y-or-n-p
                             (format "Use '%s' with Zotra? " url-field)))
                       url-field
                     (read-string
                      "url to use with Zotra: "
                      (ignore-errors (current-kill 0 t)))))))))
         (path (-flatten (list bibtex-completion-library-path)))
         (path (if (cdr path)
                   (completing-read "Add pdf to: " path nil t)
                 (car path)))
         (pdf (expand-file-name (completing-read "Rename pdf to: "
                                                 (--map (s-concat key it)
                                                        (-flatten bibtex-completion-pdf-extension))
                                                 nil nil key)
                                path)))
    (cond
     (buffer
      (with-current-buffer buffer
        (write-file pdf t)))
     (file
      (copy-file file pdf 1))
     (url
      (url-copy-file url pdf 1)))
    (when (and bibtex-completion-add-pdf-field-after-add-to-library
               (or buffer file url)
               (f-exists? pdf))
      (bibtex-completion-add-pdf-field key pdf))))
```
