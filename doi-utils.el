;;; doi-utils.el --- DOI utilities for making bibtex entries

;; Copyright (C) 2015  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((org-ref))

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

;; This package provides functionality to download bibtex entries from
;; a DOI, as well as to update a bibtex entry from a DOI.  It depends slightly
;; on org-ref, to determine where to insert
;; bibtex entries in the default bibliography.

;; The principle commands you will use from here are:

;; - doi-utils-insert-bibtex-entry-from-doi to insert a bibtex entry at your cursor, clean it and try to get a pdf.
;; - doi-utils-add-bibtex-entry-from-doi to add an entry to your default bibliography (cleaned with pdf if possible).
;; - doi-utils-update-bibtex-entry-from-doi with cursor in an entry to update its fields.

;;; Code:

(defvar org-ref-bibliography-notes)
(defvar org-ref-default-bibliography)
(defvar reftex-default-bibliography)
(defvar url-http-end-of-headers)
(declare-function org-ref-bib-citation "org-ref-core")
(declare-function org-ref-find-bibliography "org-ref-core")
(declare-function org-ref-clean-bibtex-entry "org-ref-core")
(declare-function reftex-get-bib-field "reftex-cite")
(declare-function bibtex-completion-edit-notes "bibtex-completion")
(declare-function helm "helm")
(declare-function org-bibtex-yank "org-bibtex")
(declare-function org-ref-possible-bibfiles "org-ref-core")

(eval-when-compile
  (require 'cl-lib))
(require 'bibtex)
(require 'dash)
(require 'json)
(require 'org)                          ; org-add-link-type

(or (require 'ol-bibtex nil t)
    (require 'org-bibtex)) ; org-bibtex-yank

(require 'url-http)
(require 'org-ref-utils)

;;* Customization
(defgroup doi-utils nil
  "Customization group for doi-utils."
  :tag "DOI utils"
  :group 'doi-utils)



(defcustom doi-utils-make-notes
  t
  "Whether to create notes when adding bibtex entries."
  :type 'boolean
  :group 'doi-utils)

(defcustom doi-utils-timestamp-field
  "DATE_ADDED"
  "The bibtex field to store the date when an entry has been added."
  :type 'string
  :group 'doi-utils)

(defcustom doi-utils-timestamp-format-function
  'current-time-string
  "The function to format the timestamp for a bibtex entry.
Set to a function that returns nil to avoid setting timestamps in the entries.
e.g. (lambda () nil)"
  :type 'function
  :group 'doi-utils)

(defcustom doi-utils-make-notes-function
  (lambda ()
    (bibtex-beginning-of-entry)
    (bibtex-completion-edit-notes (list (cdr (assoc "=key=" (bibtex-parse-entry))))))
  "Function to create notes for a bibtex entry.

Set `doi-utils-make-notes' to nil if you want no notes."
  :type 'function
  :group 'doi-utils)

(defcustom doi-utils-dx-doi-org-url
  "https://doi.org/"
  "Base url to retrieve doi metadata from. A trailing / is required."
  :type 'string
  :group 'doi-utils)

(defcustom doi-utils-metadata-function 'doi-utils-get-json-metadata
  "Function for retrieving json metadata from `doi-utils-dx-doi-org-url'.
The default is `doi-utils-get-json-metadata', but it sometimes
fails with a proxy. An alternative is
`doi-utils-get-json-metadata-curl' which requires an external
program to use curl.")


;;* Getting pdf files from a DOI

;; The idea here is simple. When you visit http://dx.doi.org/doi or
;; https://doi.org/doi, you get redirected to the journal site. Once you have
;; the url for the article, you can usually compute the url to the pdf, or find
;; it in the page. Then you simply download it.

;; There are some subtleties in doing this that are described here. To get the
;; redirect, we have to use url-retrieve, and a callback function. The callback
;; does not return anything, so we communicate through global variables.
;; url-retrieve is asynchronous, so we have to make sure to wait for it to
;; finish.

(defvar *doi-utils-waiting* t
  "Stores waiting state for url retrieval.")

(defvar *doi-utils-redirect* nil
  "Stores redirect url from a callback function.")

(defun doi-utils-redirect-callback (&optional status)
  "Callback for `url-retrieve' to set the redirect.
Optional argument STATUS Unknown why this is optional."
  (when (plist-get status :error)
    (signal (car (plist-get status :error)) (cdr(plist-get status :error))))
  (when (plist-get status :redirect) ;  is nil if there none
    (setq *doi-utils-redirect* (plist-get status :redirect)))
  ;; we have done our job, so we are not waiting any more.
  (setq *doi-utils-waiting* nil))

;; To actually get the redirect we use url-retrieve like this.

(defun doi-utils-get-redirect (doi)
  "Get redirect url from `doi-utils-dx-doi-org-url'/doi."
  ;; we are going to wait until the url-retrieve is done
  (setq *doi-utils-waiting* t)
  ;; start with no redirect. it will be set in the callback.
  (setq *doi-utils-redirect* nil)
  (url-retrieve
   (format "%s%s" doi-utils-dx-doi-org-url doi)
   'doi-utils-redirect-callback)
  ;; I suspect we need to wait here for the asynchronous process to
  ;; finish. we loop and sleep until the callback says it is done via
  ;; `*doi-utils-waiting*'. this works as far as i can tell. Before I
  ;; had to run this a few times to get it to work, which i suspect
  ;; just gave the first one enough time to finish.
  (while *doi-utils-waiting* (sleep-for 0.1)))

;; Once we have a redirect for a particular doi, pdf-utils can convert the
;; destination url to the appropriate PDF url

(defun doi-utils-get-pdf-url (doi)
  "Return a url to a pdf for the DOI if one can be calculated.
Loops through the functions in `doi-utils-pdf-url-functions'
until one is found."
  (doi-utils-get-redirect doi)

  (unless *doi-utils-redirect*
    (error "No redirect found for %s" doi))

  (pdf-utils-get-pdf-url *doi-utils-redirect*))


;; I
;; [[http://homepages.see.leeds.ac.uk/~eeaol/notes/2013/02/doi-metadata/][found]]
;; you can download metadata about a DOI from http://dx.doi.org. You just have
;; to construct the right http request to get it. Here is a function that gets
;; the metadata as a plist in emacs.

(defun doi-utils-get-json-metadata (doi)
  "Try to get json metadata for DOI.  Open the DOI in a browser if we do not get it."
  (let ((url-request-method "GET")
        (url-mime-accept-string "application/citeproc+json")
        (json-object-type 'plist)
        (json-data)
	(url (concat doi-utils-dx-doi-org-url doi)))
    (with-current-buffer
        (url-retrieve-synchronously
         ;; (concat "http://dx.doi.org/" doi)
	 url)
      (setq json-data (buffer-substring url-http-end-of-headers (point-max)))
      (cond
       ((or (string-match "<title>Error: DOI Not Found</title>" json-data)
	    (string-match "Resource not found" json-data)
	    (string-match "Status *406" json-data)
	    (string-match "400 Bad Request" json-data))
	(browse-url (concat doi-utils-dx-doi-org-url doi))
	(error "Something went wrong.  We got this response:
%s

Opening %s" json-data url))

       ;; everything seems ok with the data
       (t
	(json-read-from-string json-data))))))


(defun doi-utils-get-json-metadata-curl (doi)
  "Try to get json metadata for DOI.  Open the DOI in a browser if we do not get it."
  (let ((json-object-type 'plist)
        (json-data)
	(url (concat doi-utils-dx-doi-org-url doi)))
    (with-temp-buffer
      (call-process "curl" nil t nil
                    "--location"
                    "--silent"
                    "--header"
                    "Accept: application/citeproc+json"
                    url)
      (setq json-data (buffer-string))
      (cond
       ((or (string-match "<title>Error: DOI Not Found</title>" json-data)
	    (string-match "Resource not found" json-data)
	    (string-match "Status *406" json-data)
	    (string-match "400 Bad Request" json-data))
	(browse-url url)
	(error "Something went wrong.  We got this response:
%s
Opening %s" json-data url))
       ;; everything seems ok with the data
       (t
	(json-read-from-string json-data))))))

;; We can use that data to construct a bibtex entry. We do that by defining a
;; template, and filling it in. I wrote this template expansion code which
;; makes it easy to substitute values like %{} in emacs lisp.


(defun doi-utils-expand-template (s)
  "Expand a string template S containing %{} with the eval of its contents."
  (replace-regexp-in-string "%{\\([^}]+\\)}"
			    (lambda (arg)
			      (let ((sexp (substring arg 2 -1)))
				(format "%s" (eval (read sexp)))))
			    s))


;; Now we define a function that fills in that template from the metadata.

;; As different bibtex types share common keys, it is advantageous to separate
;; data extraction from json, and the formatting of the bibtex entry.

;; We use eval-and-compile because we use the three following forms in the
;; `doi-utils-def-bibtex-type' macro.  Since the macro is expanded at compile
;; time, we need to ensure these defuns and defvars are evaluated at
;; compile-time.
(eval-and-compile
  (defvar doi-utils-json-metadata-extract
    '((type       (plist-get results :type))
      (author     (mapconcat (lambda (x)
			       (message "%s" x)
			       (if (plist-get x :name)
				   (plist-get x :name)
				 (concat (plist-get x :given) " " (plist-get x :family))))
                             (plist-get results :author) " and "))
      (title      (plist-get results :title))
      (subtitle   (plist-get results :subtitle))
      (journal    (plist-get results :container-title))
      (series     (plist-get results :container-title))
      (publisher  (plist-get results :publisher))
      (volume     (plist-get results :volume))
      (issue      (plist-get results :issue))
      (number     (plist-get results :issue))
      (year       (elt (elt (plist-get (plist-get results :issued) :date-parts) 0) 0))
      ;; Some dates don't have a month in them.
      (month      (let ((date (elt
			       (plist-get (plist-get results :issued) :date-parts) 0)))
		    (if (>= (length date) 2)
			(elt date 1)
		      "-")))
      (pages      (or (plist-get results :page)
		      (plist-get results :article-number)))
      (doi        (plist-get results :DOI))
      (url        (plist-get results :URL))
      (booktitle  (plist-get results :container-title))))

  ;; Next, we need to define the different bibtex types. Each type has a bibtex
  ;; type (for output) and the type as provided in the doi record. Finally, we
  ;; have to declare the fields we want to output.

  (defvar doi-utils-bibtex-type-generators nil)

  (defun doi-utils-concat-prepare (lst &optional acc)
    "Minimize the number of args passed to `concat' from LST.
Given a list LST of strings and other expressions, which are
intended to be passed to `concat', concat any subsequent strings,
minimising the number of arguments being passed to `concat'
without changing the results.  ACC is the list of additional
expressions."
    (cond ((null lst) (nreverse acc))
          ((and (stringp (car lst))
                (stringp (car acc)))
           (doi-utils-concat-prepare (cdr lst) (cons (concat (car acc) (car lst))
                                                     (cdr acc))))
          (t (doi-utils-concat-prepare (cdr lst) (cons (car lst) acc))))))

(defmacro doi-utils-def-bibtex-type (name matching-types &rest fields)
  "Define a BibTeX type identified by (symbol) NAME.
MATCHING-TYPES is a list of strings.  FIELDS are symbols that
match to retrieval expressions in
`doi-utils-json-metadata-extract'.  This type will only be used
when the `:type' parameter in the JSON metadata is contained in
MATCHING-TYPES."
  `(push (lambda (type results)
           (when
               (or ,@(mapcar
                      (lambda (match-type)
                        `(string= type ,match-type)) matching-types))
             (let ,(mapcar (lambda (field)
                             (let ((field-expr
                                    (assoc field doi-utils-json-metadata-extract)))
                               (if field-expr
                                   ;; need to convert to string first
                                   `(,(car field-expr) (format "%s" ,(cadr field-expr)))
                                 (error "Unknown bibtex field type %s" field))))
                           fields)
               (concat
                ,@(doi-utils-concat-prepare
                   (-flatten
                    (list (concat "@" (symbol-name name) "{,\n")
                          ;; there seems to be some bug with mapcan,
                          ;; so we fall back to flatten
                          (mapcar (lambda (field)
                                    `("  " ,(symbol-name field) " = {" ,field "},\n"))
                                  fields)
                          "}\n")))))))
         doi-utils-bibtex-type-generators))

(doi-utils-def-bibtex-type article ("journal-article" "article-journal")
                           author title journal year volume number pages doi url)

(doi-utils-def-bibtex-type inproceedings ("proceedings-article" "paper-conference")
                           author title booktitle year month pages doi url)

(doi-utils-def-bibtex-type book ("book")
                           author title series publisher year pages doi url)

(doi-utils-def-bibtex-type inbook ("chapter" "book-chapter" "reference-entry")
                           author title booktitle series publisher year pages doi url)

;; this is what preprints in chemrxiv look like for now
(doi-utils-def-bibtex-type misc ("posted-content")
			   author title year doi url)



;; With the code generating the bibtex entry in place, we can glue it to the json retrieval code.

(defun doi-utils-doi-to-bibtex-string (doi)
  "Return a bibtex entry as a string for the DOI.  Not all types are supported yet."
  (let* ((results (funcall doi-utils-metadata-function doi))
         (type (plist-get results :type)))
    ;; (format "%s" results) ; json-data
    (or (-some (lambda (g) (funcall g type results)) doi-utils-bibtex-type-generators)
        (message "%s not supported yet\n%S." type results))))

;; That is just the string for the entry. To be useful, we need a function that
;; inserts the string into a buffer. This function will insert the string at the
;; cursor, clean the entry, try to get the pdf, and create a notes entry for
;; you.

(defun doi-utils-insert-bibtex-entry-from-doi (doi)
  "Insert bibtex entry from a DOI.
Also cleans entry using ‘org-ref’, and tries to download the corresponding pdf."
  (insert (doi-utils-doi-to-bibtex-string doi))
  (backward-char)
  ;; set date added for the record
  (let ((ts (funcall doi-utils-timestamp-format-function)))
    (when ts
      (bibtex-set-field doi-utils-timestamp-field
			ts)))
  (org-ref-clean-bibtex-entry)
  (save-buffer)

  ;; try to get pdf
  (when pdf-utils-download
    (doi-utils-get-bibtex-entry-pdf))

  (when (and doi-utils-make-notes org-ref-bibliography-notes)
    (save-excursion
      (when (f-file? org-ref-bibliography-notes)
	(find-file-noselect org-ref-bibliography-notes)
	(save-buffer))
      (let ((bibtex-completion-bibliography (list (buffer-file-name))))
	(funcall doi-utils-make-notes-function)))))


;; It may be you are in some other place when you want to add a bibtex entry.
;; This next function will open the first entry in org-ref-default-bibliography
;; go to the end, and add the entry. You can sort it later.


;;;###autoload
(defun doi-utils-add-bibtex-entry-from-doi (doi &optional bibfile)
  "Add DOI entry to end of a file in the current directory.
Pick the file ending with .bib or in
`org-ref-default-bibliography'.  If you have an active region that
starts like a DOI, that will be the initial prompt.  If no region
is selected and the first entry of the ‘kill-ring’ starts like a
DOI, then that is the intial prompt.  Otherwise, you have to type
or paste in a DOI.
Argument BIBFILE the bibliography to use."
  (interactive
   (list (read-string
          "DOI: "
          ;; now set initial input
          (doi-utils-maybe-doi-from-region-or-current-kill))))

  (unless bibfile
    (setq bibfile (completing-read "Bibfile: " (org-ref-possible-bibfiles))))
  ;; Wrap in save-window-excursion to restore your window arrangement after this
  ;; is done.
  (save-window-excursion
    (with-current-buffer
        (find-file-noselect bibfile)
      ;; Check if the doi already exists
      (goto-char (point-min))
      (if (word-search-forward (concat doi) nil t)
          (message "%s is already in this file" doi)
        (goto-char (point-max))

	(when (not (looking-back "\n\n" (min 3 (point))))
	  (insert "\n\n"))

        (doi-utils-insert-bibtex-entry-from-doi doi)
        (save-buffer)))))

(defalias 'doi-add-bibtex-entry 'doi-utils-add-bibtex-entry-from-doi
  "Alias function for convenience.")

(defun doi-utils-maybe-doi-from-region-or-current-kill ()
  "Try to get a DOI from the active region or current kill."
  (let* ((the-active-region (if (region-active-p) ;; nil if no active region
                                (buffer-substring (region-beginning) (region-end))
                              nil))
         (the-current-kill (ignore-errors (current-kill 0 t)))  ;; nil if empty kill ring
         ;; DOI urls
         ;; Ex: https://doi.org/10.1109/MALWARE.2014.6999410
         ;; Ex: https://dx.doi.org/10.1007/978-3-319-60876-1_10
         (doi-url-prefix-regexp "^https?://\\(dx\\.\\)?doi\\.org/")
         ;; https://www.crossref.org/blog/dois-and-matching-regular-expressions/
         (doi-regexp "10\\.[0-9]\\{4,9\\}/[-._;()/:A-Z0-9]+$"))
    (cond
     ;; Check if a DOI can be found in the active region
     ;; DOI raw
     ;; Ex: 10.1109/MALWARE.2014.6999410
     ((and (stringp the-active-region)
           (s-match (concat "^" doi-regexp) the-active-region))
      the-active-region)
     ;; DOI url
     ;; Ex: https://doi.org/10.1109/MALWARE.2014.6999410
     ((and (stringp the-active-region)
           (s-match (concat doi-url-prefix-regexp doi-regexp) the-active-region))
      (replace-regexp-in-string doi-url-prefix-regexp "" the-active-region))
     ;; DOI url as customized
     ((and (stringp the-active-region)
           (s-match (regexp-quote doi-utils-dx-doi-org-url) the-active-region))
      (replace-regexp-in-string (regexp-quote doi-utils-dx-doi-org-url) "" the-active-region))
     ;; Check if DOI can be found in the current kill
     ;; DOI raw
     ;; Ex: 10.1109/MALWARE.2014.6999410
     ((and (stringp the-current-kill)
           (s-match (concat "^" doi-regexp) the-current-kill))
      the-current-kill)
     ;; DOI url
     ;; Ex: https://doi.org/10.1109/MALWARE.2014.6999410
     ((and (stringp the-current-kill)
           (s-match (concat doi-url-prefix-regexp doi-regexp) the-current-kill))
      (replace-regexp-in-string doi-url-prefix-regexp "" the-current-kill))
     ;; DOI url as customized
     ((and (stringp the-current-kill)
           (s-match (regexp-quote doi-utils-dx-doi-org-url) the-current-kill))
      (replace-regexp-in-string (regexp-quote doi-utils-dx-doi-org-url) "" the-current-kill))
     ;; otherwise, return nil
     (t
      nil))))

;;;###autoload
(defun doi-utils-doi-to-org-bibtex (doi)
  "Convert a DOI to an ‘org-bibtex’ form and insert it at point."
  (interactive "sDOI: ")
  (with-temp-buffer
    (insert (doi-utils-doi-to-bibtex-string doi))
    (bibtex-clean-entry)
    (kill-region (point-min) (point-max)))
  (org-bibtex-yank)
  (org-metaright)
  (org-metaright))

;;* Updating bibtex entries

;; I wrote this code because it is pretty common for me to copy bibtex entries
;; from ASAP articles that are incomplete, e.g. no page numbers because it is
;; not in print yet. I wanted a convenient way to update an entry from its DOI.
;; Basically, we get the metadata, and update the fields in the entry.

;; There is not bibtex set field function, so I wrote this one.


;;;###autoload
(defun bibtex-set-field (field value &optional nodelim)
  "Set FIELD to VALUE in bibtex file.  create field if it does not exist.
Optional argument NODELIM see `bibtex-make-field'."
  (interactive "sfield: \nsvalue: ")
  (bibtex-beginning-of-entry)
  (let ((found))
    (if (setq found (bibtex-search-forward-field field t))
        ;; we found a field
        (progn
          (goto-char (car (cdr found)))
          (when value
            (bibtex-kill-field)
            (bibtex-make-field field nil nil nodelim)
            (backward-char)
            (insert value)))
      ;; make a new field
      (bibtex-beginning-of-entry)
      (forward-line) (beginning-of-line)
      (bibtex-next-field nil)
      (forward-char)
      (bibtex-make-field field nil nil nodelim)
      (backward-char)
      (insert value))))


;; The updating function for a whole entry looks like this. We get all the keys
;; from the json plist metadata, and update the fields if they exist.


(defun plist-get-keys (plist)
  "Return keys in a PLIST."
  (-slice plist 0 nil 2))

;;;###autoload
(defun doi-utils-update-bibtex-entry-from-doi (doi)
  "Update fields in a bibtex entry from the DOI.
Every field will be updated, so previous change will be lost."
  (interactive (list
                (or (replace-regexp-in-string
                     "https?://\\(dx.\\)?doi.org/" ""
                     (bibtex-autokey-get-field "doi"))
                    (read-string "DOI: "))))
  (let* ((results (funcall doi-utils-metadata-function doi))
         (type (plist-get results :type))
         (author (mapconcat
                  (lambda (x)
		    ;; There are two possible ways an author is named. The most
		    ;; common is with :given and :family, but sometimes there is
		    ;; :name instead.
		    (if (plist-get x :name)
			(plist-get x :name)
		      (concat (plist-get x :given)
			      " " (plist-get x :family))))
                  (plist-get results :author) " and "))
         (title (plist-get results :title))
         (journal (plist-get results :container-title))
         (year (format "%s"
                       (elt
                        (elt
                         (plist-get
                          (plist-get results :issued) :date-parts) 0) 0)))
         (volume (plist-get results :volume))
         (number (or (plist-get results :issue) ""))
         (pages (or (plist-get results :page) ""))
         (url (or (plist-get results :URL) ""))
         (doi (plist-get results :DOI))
         mapping)

    ;; map the json fields to bibtex fields. The code each field is mapped to is
    ;; evaluated.
    (setq mapping '((:author . (bibtex-set-field "author" author))
                    (:title . (bibtex-set-field "title" title))
                    (:container-title . (bibtex-set-field "journal" journal))
                    (:issued . (bibtex-set-field "year" year))
                    (:volume . (bibtex-set-field "volume" volume))
                    (:issue . (bibtex-set-field "number" number))
                    (:page . (bibtex-set-field "pages" pages))
                    (:DOI . (bibtex-set-field "doi" doi))
                    (:URL . (bibtex-set-field "url" url))))

    ;; now we have code to run for each entry. we map over them and evaluate the code
    (mapc
     (lambda (key)
       (eval (cdr (assoc key mapping))))
     (plist-get-keys results)))

  (org-ref-clean-bibtex-entry))


;; A downside to updating an entry is it overwrites what you have already fixed.
;; So, we next develop a function to update the field at point.


;;;###autoload
(defun doi-utils-update-field ()
  "Update the field at point in the bibtex entry.
Data is retrieved from the doi in the entry."
  (interactive)
  (let* ((doi (bibtex-autokey-get-field "doi"))
         (results (funcall doi-utils-metadata-function doi))
         (field (car (bibtex-find-text-internal nil nil ","))))
    (cond
     ((string= field "volume")
      (bibtex-set-field field (plist-get results :volume)))
     ((string= field "number")
      (bibtex-set-field field (plist-get results :issue)))
     ((string= field "pages")
      (bibtex-set-field field (or (plist-get results :page)
                                  (plist-get results :article-number))))
     ((string= field "year")
      (bibtex-set-field field (plist-get results :year)))
     (t
      (message "%s not supported yet." field)))))



;;* DOI functions for WOS

;; I came across this API http://wokinfo.com/media/pdf/OpenURL-guide.pdf to make
;; links to the things I am interested in here. Based on that document, here are
;; three links based on a doi:10.1021/jp047349j that take you to different Web
;; Of Science (WOS) pages.


;; 1. go to article in WOS: http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info:doi/10.1021/jp047349j
;; 2. citing articles: http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info%3Adoi%2F10.1021/jp047349j&svc_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Asch_svc&svc.citing=yes
;; 3. related articles: http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info%3Adoi%2F10.1021/jp047349j&svc_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Asch_svc&svc.related=yes

;; These are pretty easy to construct, so we can write functions that will
;; create them and open the url in our browser. There are some other options
;; that could be considered, but since we usually have a doi, it seems like the
;; best way to go for creating the links. Here are the functions.

;;;###autoload
(defun doi-utils-wos (doi)
  "Open Web of Science entry for DOI."
  (interactive "sDOI: ")
  (browse-url
   (format
    "http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info:doi/%s" doi)))

;;;###autoload
(defun doi-utils-wos-citing (doi)
  "Open Web of Science citing articles entry for DOI.
May be empty if none are found."
  (interactive "sDOI: ")
  (browse-url
   (concat
    "http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info%3Adoi%2F"
    doi
    "&svc_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Asch_svc&svc.citing=yes")))

;;;###autoload
(defun doi-utils-wos-related (doi)
  "Open Web of Science related articles page for DOI."
  (interactive "sDOI: ")
  (browse-url
   (concat "http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info%3Adoi%2F"
           doi
           "&svc_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Asch_svc&svc.related=yes")))


;;* DOI functions for ADS

;;;###autoload
(defun doi-utils-ads (doi)
  "Open ADS entry for DOI"
  (interactive "sDOI: ")
  (browse-url
   (concat
    "https://ui.adsabs.harvard.edu/abs/" "%22" doi "%22")))


;;* A new doi link for org-mode
;; The idea is to add a menu to the doi link, so rather than just clicking to open the article, you can do other things.
;; 1. open doi
;; 2. open in wos
;; 3. open citing articles
;; 4. open related articles
;; 5. open bibtex entry
;; 6. get bibtex entry


;;;###autoload
(defun doi-utils-open (doi)
  "Open DOI in browser."
  (interactive "sDOI: ")
  (browse-url (concat doi-utils-dx-doi-org-url doi)))


;;;###autoload
(defun doi-utils-open-bibtex (doi)
  "Search through variable `reftex-default-bibliography' for DOI."
  (interactive "sDOI: ")
  (catch 'file
    (dolist (f reftex-default-bibliography)
      (find-file f)
      (when (search-forward doi (point-max) t)
        (bibtex-beginning-of-entry)
        (throw 'file t)))))


;;;###autoload
(defun doi-utils-crossref (doi)
  "Search DOI in CrossRef."
  (interactive "sDOI: ")
  (browse-url
   (format
    "http://search.crossref.org/?q=%s" doi)))


;;;###autoload
(defun doi-utils-google-scholar (doi)
  "Google scholar the DOI."
  (interactive "sDOI: ")
  (browse-url
   (format
    "http://scholar.google.com/scholar?q=%s" doi)))


;;;###autoload
(defun doi-utils-pubmed (doi)
  "Search Pubmed for the DOI."
  (interactive "sDOI: ")
  (browse-url
   (format
    "http://www.ncbi.nlm.nih.gov/pubmed/?term=%s"
    (url-hexify-string doi))))


(defvar doi-link-menu-funcs '()
  "Functions to run in doi menu.
Each entry is a list of (key menu-name function).  The function
must take one argument, the doi.")

(setq doi-link-menu-funcs
      '(("o" "pen" doi-utils-open)
        ("w" "os" doi-utils-wos)
        ("c" "iting articles" doi-utils-wos-citing)
        ("r" "elated articles" doi-utils-wos-related)
        ("a" "ds" doi-utils-ads)
        ("s" "Google Scholar" doi-utils-google-scholar)
        ("f" "CrossRef" doi-utils-crossref)
        ("p" "ubmed" doi-utils-pubmed)
        ("b" "open in bibtex" doi-utils-open-bibtex)
        ("g" "et bibtex entry" doi-utils-add-bibtex-entry-from-doi)))


;;;###autoload
(defun doi-link-menu (link-string)
  "Generate the link menu message, get choice and execute it.
Options are stored in `doi-link-menu-funcs'.
Argument LINK-STRING Passed in on link click."
  (interactive)
  (message
   (concat
    (mapconcat
     (lambda (tup)
       (concat "[" (elt tup 0) "]"
               (elt tup 1) " "))
     doi-link-menu-funcs "") ": "))
  (let* ((input (read-char-exclusive))
         (choice (assoc
                  (char-to-string input) doi-link-menu-funcs)))
    (when choice
      (funcall
       (elt
        choice
        2)
       link-string))))

(org-ref-link-set-parameters "doi"
  :follow #'doi-link-menu
  :export (lambda (doi desc format)
            (cond
             ((eq format 'html)
              (format "<a href=\"%s%s\">%s</a>"
                      doi-utils-dx-doi-org-url
                      doi
                      (or desc (concat "doi:" doi))))
             ((eq format 'latex)
              (format "\\href{%s%s}{%s}"
                      doi-utils-dx-doi-org-url
                      doi
                      (or desc (concat "doi:" doi)))))))

;;* Getting a doi for a bibtex entry missing one

;; Some bibtex entries do not have a DOI, maybe because they were entered by
;; hand, or copied from a source that did not have it available. Here we develop
;; some functions to help you find the DOI using Crossref.

;; Here is our example bibtex entry.
;; #+BEGIN_SRC bibtex
;; @article{deml-2014-oxide,
;;   author =	 {Ann M. Deml and Vladan Stevanovi{\'c} and
;;                   Christopher L. Muhich and Charles B. Musgrave and
;;                   Ryan O'Hayre},
;;   title =	 {Oxide Enthalpy of Formation and Band Gap Energy As
;;                   Accurate Descriptors of Oxygen Vacancy Formation
;;                   Energetics},
;;   journal =	 {Energy Environ. Sci.},
;;   volume =	 7,
;;   number =	 6,
;;   pages =	 1996,
;;   year =	 2014,
;;   doi =		 {10.1039/c3ee43874k,
;;   url =		 {http://dx.doi.org/10.1039/c3ee43874k}},

;; }


;; The idea is to query Crossref in a way that is likely to give us a hit
;; relevant to the entry.

;; According to http://search.crossref.org/help/api we can send a query with a
;; free form citation that may give us something back. We do this to get a list
;; of candidates, and run a helm command to get the doi.


;;;###autoload
(defun doi-utils-crossref-citation-query ()
  "Query Crossref with the title of the bibtex entry at point.
Get a list of possible matches.  This opens a helm buffer to
select an entry.  The default action inserts a doi and url field
in the bibtex entry at point.  The second action opens the doi
url.  If there is already a doi field, the function raises an
error."
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
	 (raw-json-string)
         (json-string)
         (json-data)
         (doi))
    (unless (string= ""(reftex-get-bib-field "doi" entry))
      (error "Entry already has a doi field"))

    (with-current-buffer
        (url-retrieve-synchronously
         (concat
          "http://search.crossref.org/dois?q="
          (url-hexify-string (org-ref-bib-citation))))
      (save-excursion
      	(goto-char (point-min))
      	(while (re-search-forward "<i>\\|</i>" nil t)
      	  (replace-match ""))
	(goto-char (point-min))
	(while (re-search-forward "&amp;" nil t)
	  (replace-match "&"))
      	(goto-char (point-min))
      	(while (re-search-forward "&quot;" nil t)
      	  (replace-match "\\\"" nil t)))
      (setq raw-json-string (buffer-substring url-http-end-of-headers (point-max)))
      ;; decode json string
      (setq json-string (decode-coding-string (string-make-unibyte raw-json-string) 'utf-8))
      (setq json-data (json-read-from-string json-string)))

    (let* ((name (format "Crossref hits for %s" (org-ref-bib-citation)))
           (helm-candidates (mapcar (lambda (x)
                                      (cons
                                       (concat
                                        (cdr (assoc 'fullCitation x)))
                                       (cdr (assoc 'doi x))))
                                    json-data))

           (source `((name . ,name)
                     (candidates . ,helm-candidates)
                     ;; just return the candidate
                     (action . (("Insert doi and url field" . (lambda (doi)
                                                                (bibtex-make-field "doi" t)
                                                                (backward-char)
                                                                ;; crossref returns doi url, but I prefer only a doi for the doi field
                                                                (insert (replace-regexp-in-string "^https?://\\(dx.\\)?doi.org/" "" doi))
                                                                (when (string= ""(reftex-get-bib-field "url" entry))
                                                                  (bibtex-make-field "url" t)
                                                                  (backward-char)
                                                                  (insert doi))))
                                ("Open url" . (lambda (doi)
                                                (browse-url doi))))))))
      (helm :sources source
	    :buffer "*doi utils*"))))



;;* Debugging a DOI

;; I wrote this function to help debug a DOI. This function generates an
;; org-buffer with the doi, gets the json metadata, shows the bibtex entry, and
;; the pdf link for it.
(defun doi-utils-get-json (doi)
  "Return json data as a string for DOI."
  (let ((url-request-method "GET")
        (url-mime-accept-string "application/citeproc+json")
        (json-data))
    (with-current-buffer
        (url-retrieve-synchronously
         (concat doi-utils-dx-doi-org-url doi))
      (setq json-data (buffer-substring url-http-end-of-headers (point-max)))
      (if (string-match "Resource not found" json-data)
          (progn
            (browse-url (concat doi-utils-dx-doi-org-url doi))
            (error "Resource not found.  Opening website"))
	json-data))))


;;;###autoload
(defun doi-utils-debug (doi)
  "Generate an org-buffer showing data about DOI."
  (interactive "sDOI: ")
  (switch-to-buffer "*debug-doi*")
  (erase-buffer)
  (org-mode)
  (insert (concat "doi:" doi) "\n\n")
  (insert "* JSON
"
	  (let ((url-request-method "GET")
		(url-mime-accept-string "application/citeproc+json"))
	    (pp
	     (json-read-from-string (with-current-buffer
					(url-retrieve-synchronously
					 (concat doi-utils-dx-doi-org-url doi))
				      (buffer-substring url-http-end-of-headers (point-max))))))
	  "\n\n")
  (goto-char (point-min)))

;;* Adding a bibtex entry from a crossref query

;; The idea here is to perform a query on Crossref, get a helm buffer of
;; candidates, and select the entry(ies) you want to add to your bibtex file.
;; You can select a region, e.g. a free form citation, or set of words, or you
;; can type the query in by hand.

;;;###autoload
(defun doi-utils-add-entry-from-crossref-query (query bibtex-file)
  "Search Crossref with QUERY and use helm to select an entry to add to BIBTEX-FILE."
  (interactive (list
                (read-string
                 "Query: "
                 ;; now set initial input
                 (cond
                  ;; If region is active assume we want it
                  ((region-active-p)
                   (replace-regexp-in-string
                    "\n" " "
                    (buffer-substring (region-beginning) (region-end))))
                  ;; type or paste it in
                  (t
                   nil)))
                (completing-read
                 "Bibfile: "
                 (append (f-entries "." (lambda (f) (f-ext? f "bib")))
                         org-ref-default-bibliography))))
  (let* ((raw-json-string)
	 (json-string)
	 (json-data)
	 (doi))

    (with-current-buffer
	(url-retrieve-synchronously
	 (concat
	  "http://search.crossref.org/dois?q="
	  (url-hexify-string query)))
      ;; replace html entities
      (save-excursion
      	(goto-char (point-min))
      	(while (re-search-forward "<i>\\|</i>" nil t)
      	  (replace-match ""))
	(goto-char (point-min))
	(while (re-search-forward "&amp;" nil t)
	  (replace-match "&"))
      	(goto-char (point-min))
      	(while (re-search-forward "&quot;" nil t)
      	  (replace-match "\\\"" nil t)))
      (setq raw-json-string (buffer-substring url-http-end-of-headers (point-max)))
      ;; decode json string
      (setq json-string (decode-coding-string (string-make-unibyte raw-json-string) 'utf-8))
      (setq json-data (json-read-from-string json-string)))

    (let* ((name (format "Crossref hits for %s"
			 ;; remove carriage returns. they cause problems in helm.
			 (replace-regexp-in-string "\n" " " query)))
	   (helm-candidates (mapcar (lambda (x)
				      (cons
				       (concat
					(cdr (assoc 'fullCitation x)))
				       (cdr (assoc 'doi x))))
				    json-data))
	   (source `((name . ,name)
		     (candidates . ,helm-candidates)
		     ;; just return the candidate
		     (action . (("Insert bibtex entry" .  (lambda (doi)
							    (with-current-buffer (find-file-noselect bibtex-file)
							      (cl-loop for doi in (helm-marked-candidates)
								       do
								       (doi-utils-add-bibtex-entry-from-doi
									(replace-regexp-in-string
									 "^https?://\\(dx.\\)?doi.org/" "" doi)
									,bibtex-file)))))
				("Open url" . (lambda (doi)
						(browse-url doi))))))))
      (helm :sources source
	    :buffer "*doi utils*"))))

(defalias 'crossref-add-bibtex-entry 'doi-utils-add-entry-from-crossref-query
  "Alias function for convenience.")


;;* The end
(provide 'doi-utils)
;;; doi-utils.el ends here
