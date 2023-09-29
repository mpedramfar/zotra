# Zotra

[![MELPA](https://melpa.org/packages/zotra-badge.svg)](https://melpa.org/#/zotra)

This emacs library provides functions to fetch bibliographic information in different formats (e.g. bibtex) using website urls or search identifiers (e.g. DOI). 
It also provides functions to fetch attachments (e.g. PDF files) associated with a url.

This is done using [**ZO**tero **TRA**nslators](https://www.zotero.org/support/translators), but without using the Zotero client.


## Changelog

- [2023-09-30 Thu]: The `citoid` backend added.
- [2023-09-30 Thu]: The functions `zotra-add-entry-from-url`, `zotra-add-entry-from-search`, `zotra-download-attachment-from-url` and `zotra-download-attachment-from-search` are obsolete. Use `zotra-add-entry` and `zotra-download-attachment` instead.
- [2023-09-30 Thu]: The `curl_translation-server` backend removed. Use the variable `zotra-use-curl` instead.
- [2023-09-21 Thu]: Better integration with bibtex-completion. Also the variables `zotra-download-attachment-default-directory` and `zotra-default-bibliography` can now be lists.
- [2023-08-18 Fri]: The variable `zotra-after-add-entry-hook` is now obsolete; use `zotra-after-get-bibtex-entry-hook` instead.
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

This library tries to connect to a backend to do the translation.
The choice of backend can be customized through `zotra-backend` variable.

There are 3 options for `zotra-backend`:

- `citoid`

Use the Wikimedia Citoid public API.
See [here](https://en.wikipedia.org/api/rest_v1/#/Citation/getCitation) for terms of service and documentation.
This is the default backend since it only uses a public API and does not require any extra installations.
However, this backend is more limited than the other two options.

- `zotra-cli`

Use the external [zotra-cli](https://github.com/mpedramfar/zotra-cli) library for translation.
This is the recommended backend to use as **it is the only backend that is able to fetch attachments**.
See [zotra-cli](https://github.com/mpedramfar/zotra-cli) repository for its installation guide.

Note that if `zotra-cli` is installed locally (i.e. the `zotra` command is not available from your shell), then you can customize `zotra-cli-command` to point at the installed library:
```elisp
(setq zotra-cli-command '("node" "/path/to/zotra-cli/bin/index.js"))
```

- `translation-server`

Use emacs url libraries to connect to a running instance of Zotero translation server.
See [Zotero translation server](https://github.com/zotero/translation-server/) repository for its installation guide.
If you have the external curl program, it is recommended to set `zotra-use-curl` to `t` to use curl instead.


## Usage and configuration

From inside emacs, run `zotra-add-entry`, then enter the url or the search identifier (DOI, ISBN, PMID, arXiv ID) and choose the bibtex file to save into.
To download attachments from a url or search identifier, run `zotra-download-attachment`.

See `zotra` customization group for a complete list of options and their description.

### Using Zotra with a browser and org-protocol

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

### Using Zotra with [bibtex-completion](https://github.com/tmalsburg/helm-bibtex/)

If bibtex-completion is loaded, Zotra will add a fallback options to `bibtex-completion-fallback-options`.
It will also add the option to download with Zotra when calling `bibtex-completion-add-pdf-to-library`.

You can also set `zotra-download-attachment-default-directory` to one (or more) of the values in `bibtex-completion-library-path` and `zotra-default-bibliography` to one (or more) of the values in `bibtex-completion-bibliography`, for example:
```emacs-lisp
(setq zotra-download-attachment-default-directory bibtex-completion-library-path)
(setq zotra-default-bibliography bibtex-completion-bibliography)
```
Note that bibtex-completion can work with bibtex, biblatex and org-bibtex bibliography files, while Zotra can work with formats listed in the description of `zotra-default-entry-format`.
In particular, org-bibtex is not supported in Zotra, so you can't set `zotra-download-attachment-default-directory` equal to `bibtex-completion-library-path` if it contains org-bibtex items.

### Using Zotra with [org-ref](https://github.com/jkitchin/org-ref)

The functions in `zotra-after-get-bibtex-entry-hook` are called before a bibtex/biblatex entry is added.
They take no arguments, and they can be used to cleanup and format new entries.
For example, if you are using org-ref, you could add this line to your init file:
```emacs-lisp
(add-hook 'zotra-after-get-bibtex-entry-hook 'org-ref-clean-bibtex-entry)
```
Note that org-ref depends on bibtex-completion.

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
                       (zotra-download-attachment
                        url nil filename))))
      (when filename
        (bibtex-make-field (list "File" nil filename) t)))))
```

This function can be added to `zotra-after-get-bibtex-entry-hook`. 
*Note that this function chooses the bibtex key as the filename so it should be be after any hook that might change the bibtex key.*
When it is added to this hook, any time a bibtex entry is added, its attachments will also be downloaded.
Alternatively, you can add the following function to your init file to add an entry from url and download its attachments.

```emacs-lisp
(defun zotra-add-entry-and-download-attachment (&optional url)
  (interactive)
  (let ((zotra-after-get-bibtex-entry-hook
         (append zotra-after-get-bibtex-entry-hook
                 '(zotra-download-attachment-for-current-entry))))
    (zotra-add-entry url)))
```
