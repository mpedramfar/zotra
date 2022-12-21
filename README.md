# Zotra

This emacs library provides functions to get bibliographic information from a url and save it into a bibtex file.
It also provides a way to obtain a list of attachments (e.g. PDF files) associated with a url.
This is done using [Zotero translators](https://www.zotero.org/support/translators), but without using the Zotero client.

## Instalation

### Zotra

Clone the repository and add the following line to your init file.

``` emacs-lisp
(add-to-list 'load-path "/path/to/zotra")
(require 'zotra)
```

Note that this library tries to connect to a backend to do the translation.
The choice of backend can be customized through `zotra-backend` variable.

There are 3 options for `zotra-backend`:
- `zotra-cli`

Use the external [zotra-cli](https://github.com/mpedramfar/zotra-cli) library for translation.
This is the recommended backend to use as **it is the only backend that is able to fetch attachments**.
See [zotra-cli](https://github.com/mpedramfar/zotra-cli) repository for its installation guide.

Note that if `zotra-cli` is installed locally (i.e. the `zotra` command is not available from your shell), then you can customize `zotra-cli-command` to point at the installed library:
```elisp
(setq zotra-cli-command "node /path/to/zotra-cli/bin/index.js")
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

To download and open attachments from a url or search identifier, run `zotra-open-attachment-from-url`/`zotra-open-attachment-from-search`.

See `zotra` customization group for a complete list of options and their description.

### Using zotra with a browser and org-protocol

First you need to set up [org-protocol](https://orgmode.org/worg/org-contrib/org-protocol.html).
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

```emacs-lisp
(defun zotra-bibtex-completion-get-pdf-url-from-keys (keys)
  "Get the url attachment from the url field of bibtex entry or prompt user."
  (let* ((key (car keys))
         (entry (bibtex-completion-get-entry key))
         (url-field (or (bibtex-completion-get-value "url" entry) ""))
         (url-field (string-trim url-field))
         (url (if (and (< 0 (length url-field))
                       (y-or-n-p
                        (format "Use '%s'? " url-field)))
                  url-field
                (read-string
                 "url: "
                 (ignore-errors (current-kill 0 t)))))
         (attachments (zotra-get-attachments url)))
    (if attachments
        (completing-read
         "Which attachment to add? " attachments nil t)
      (user-error "zotra failed to find any attachments in page"))))
```


```emacs-lisp
(defun bibtex-completion-add-pdf-to-library (keys)
  "Add a PDF to the library for the first entry in KEYS.
The PDF can be added either from an open buffer, a file, a
URL, or using zotra."
  (let* ((key (car keys))
         (source (char-to-string
                  (read-char-choice "Add pdf from [b]uffer, [f]ile, [u]rl, or [z]otra? " '(?b ?f ?u ?z))))
         (buffer (when (string= source "b")
                   (read-buffer-to-switch "Add pdf buffer: ")))
         (file (when (string= source "f")
                 (expand-file-name (read-file-name "Add pdf file: " nil nil t))))
         (url (when (string= source "u")
                (read-string "Add pdf URL: ")))
         (url (if (string= source "z")
                  (zotra-bibtex-completion-get-pdf-url-from-keys keys)
                url))
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
      (url-copy-file url pdf 1)))))
```

