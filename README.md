# Zotra

This emacs library provides functions to get bibliographic information from a url and save it into a bibtex file.
It also provides a way to obtain a list of attachments (e.g. PDF files) associated with a url.
This is done using [Zotero translators](https://www.zotero.org/support/translators), but without using the Zotero client.

## Instalation

### Zotra

Clone the repository and add the following line to your init file.

``` emacs-lisp
(add-to-list 'load-path "/path/to/zotra")
```

Note that this library tries to connect to a backend to do the translation.
The choice of backend can be customized through `zotra-backend` variable.

There are 3 options for `zotra-backend`:
- `translation-server`

Use emacs url libraries to connect to a running instance of zotero translation server.
- `curl_translation-server`

Use the external curl program to connect to a running instance of zotero translation server.
Sometimes the previous backend fails. In this case you can try this backend.
- `zotra-cli`

Use the external [zotra-cli](https://github.com/mpedramfar/zotra-cli) library for translation.
This is the recommended backend to use as **it is the only backend that is able to fetch attachments**.
### **zotra-cli**
See [zotra-cli](https://github.com/mpedramfar/zotra-cli) repository for its installation guide.
This is the recommended backend to use with zotra as it has the option to fetch attachments corresponding to a url.

### Zotero translation server
Download and run the [Zotero translation server](https://github.com/zotero/translation-server/).

**Method 1: Using docker (easier to set up but uses more reources)**

The easiest way to install this is to install [docker](https://docs.docker.com/engine/install/) and run
```bash
docker pull zotero/translation-server
docker run -d -p 1969:1969 --rm --name translation-server zotero/translation-server
```
If you want the translation server to always run (so that you wouldn't have to run it manually after each reboot), you can replace the second command with 
```bash
docker run --restart=always -d -p 1969:1969 --name translation-server zotero/translation-server
```
**Method 2: Using nodejs (more lightweight)**

You can also run the translation server [without docker, using nodejs](https://github.com/zotero/translation-server/).

## Usage and configuration

From inside emacs, run `zotra-add-entry-from-url`, then enter the url and choose the bibtex file to save into.
Alternatively, you can run `zotra-add-entry-from-search`, then enter the search identifier (DOI, ISBN, PMID, arXiv ID) and choose the bibtex file to save into.

To download/open attachments from a url, run `zotra-download-attachment`/`zotra-open-attachment`.

See `zotra` customization group for a complete list of options and their description.

### Using zotra with a browser and org-protocol

First you need to set up [org-protocol](https://orgmode.org/worg/org-contrib/org-protocol.html).
Then make a bookmark in your browser with the following url:
```
javascript:location.href=('org-protocol://zotra?url='+ encodeURIComponent(location.href)+'&bibfile='+encodeURIComponent('/path/to/bibfile.bib')+'&format=my-save-format').replace(/'/gi,"%27")
```
where `/path/to/bibfile.bib` should be replaced with the path to the bibfile and `my-save-format` should be replaced with one of the acceptable formats listed above.

You can omit format and bibfile to use the value of `zotra-default-entry-format` and `zotra-default-bibliography` respectively:
```
javascript:location.href=('org-protocol://zotra?url='+ encodeURIComponent(location.href)).replace(/'/gi,"%27")
```
Now you can click on the bookmark in any page and the bibliographic information of the page will be saved into emacs automatically.