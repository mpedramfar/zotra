# Zotra

This emacs library provides functions to get bibliographic information from a url and save it into a bibtex file.
This is done using [Zotero translators](https://www.zotero.org/support/translators), but without using the Zotero client.

## Instalation

### Zotero translation server
Download and run the [Zotero translation server](https://github.com/zotero/translation-server/).
Note that this library tries to connect to this server and does not work when the server is not running.

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

### Zotra

Clone the repository and add the following line to your init file.

``` emacs-lisp
(add-to-list 'load-path "/path/to/zotra")
```

## Usage and configuration

From inside emacs, run `zotra-add-entry-from-url`, then enter the url and choose the bibtex file to save into.
Alternatively, you can run `zotra-add-entry-from-search`, then enter the search identifier (DOI, ISBN, PMID, arXiv ID) and choose the bibtex file to save into.

Here is the complete list of customizable variables:

- `zotra-after-add-entry-hook`

  These functions are called after adding an entry. They take no arguments, and they can be used to cleanup and format new entries.
  For example, if you are using `org-ref`, you could add this line to your init file:
  ```emacs-lisp
  (add-hook 'zotra-after-add-entry-hook 'org-ref-clean-bibtex-entry)
  ```
  The default value is: '(bibtex-clean-entry)


- `zotra-default-bibliography`
  
  If this variable is non-nil, interactive calls to `zotra-add-entry-from-url` and `zotra-add-entry-from-search` will not ask the user for the bibfile.
  Otherwise, zotra will make a list containing the `.bib` files
  in the current directory and the files returned by
  `org-cite-list-bibliography-files` and ask the user to choose one."


- `zotra-default-entry-format`

   The citation format. Can be one of the following options:
   + bibtex
   + biblatex
   + bookmarks
   + coins
   + csljson
   + csv
   + endnote_xml
   + evernote
   + mods
   + rdf_bibliontology
   + rdf_dc
   + rdf_zotero
   + refer
   + refworks_tagged
   + ris
   + tei
   + wikipedia
   
   See https://github.com/zotero/translation-server#export-translation
   and https://github.com/zotero/translation-server/blob/master/src/formats.js

   The default value is: "bibtex"

- `zotra-server-path`

   The url and the port of the Zotero translation server to be used, without a trailing slash mark.
   
   The default value is: "http://127.0.0.1:1969"


- `zotra-url-cleanup-functions`
  
  Currently, the Zotero translation server can’t handle links to pdf files.
  (See https://github.com/zotero/translation-server/issues/70).
  These functions provide a way to fix the issue by manually changing the link to a pdf to a link to another url for the article.
  Each function in this list should take a url and return a url. If the function is not applicable, it should return its input without change.

  Currently, this is implemeted for:
  + arxiv.org

- `zotra-url-retrieve-timeout`
   
   How many seconds to wait for server to get a response.
   
   The default value is: 3

- `zotra-use-curl`

  Functions for retrieving citation entries use `url-retrieve-synchronously`, but it sometimes fails. An alternative is to use the external `curl` program to retrieve the data.

  The default value is: nil


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