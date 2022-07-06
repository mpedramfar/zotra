# Zotra

This emacs library provides functions to get bibliographic information from a url and save it into a bibtex file.
This is done using [Zotero translators](https://www.zotero.org/support/translators), but without using the Zotero client.

## Instalation

Download and run the [Zotero translation server](https://github.com/zotero/translation-server/).
Note that this library tries to connect to this server and does not work when the server is not running.
The easiest way to install this is to install [docker](https://docs.docker.com/engine/install/) and run
```bash
docker pull zotero/translation-server
docker run -d -p 1969:1969 --rm --name translation-server zotero/translation-server
```
If you want the translation server to always run (so that you wouldn't have to run it manually after each reboot), you can replace the second command with 
```bash
docker run --restart=always -d -p 1969:1969 --name translation-server zotero/translation-server
```
You can also run the translation server [without docker, using nodejs](https://github.com/zotero/translation-server/).

Clone the repository and add the following line to your init file.

``` emacs-lisp
(add-to-list 'load-path "/path/to/zotra")
```

## Usage and configuration

From inside emacs, run `zotra-add-bibtex-entry`, then enter the url and choose the bibtex file to save into.

You can set the variable `zotra-default-bibliography` so that it always saves there.

You can also set `zotra-after-add-entry-hook` to clean up the recently added entries. For example, if you are using `org-ref`, you could add this line to your init file.
```emacs-lisp
(add-hook 'zotra-after-add-entry-hook 'org-ref-clean-bibtex-entry)
```
### Using zotra with a browser and org-protocol

First you need to set up [org-protocol](https://orgmode.org/worg/org-contrib/org-protocol.html).
Then make a bookmark in your browser with the following url:
```
javascript:location.href=('org-protocol://zotra?url='+ encodeURIComponent(location.href)+'&bibfile='+encodeURIComponent('/path/to/bibfile.bib')).replace(/'/gi,"%27")
```
If you want to save to `zotra-default-bibliography`, you can simply use:
```
javascript:location.href=('org-protocol://zotra?url='+ encodeURIComponent(location.href)).replace(/'/gi,"%27")
```
Now you can click on the bookmark in any page and the bibliographic information of the page will be saved into emacs automatically.