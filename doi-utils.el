
;;; doi-utils.el --- get bibtex entries and pdfs from a DOI

;; Copyright(C) 2014 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Lisp code to generate and update bibtex entries from a DOI, and to
;; download pdfs from publisher websites from a DOI.
;;
;; Package-Requires: ((org-ref))

(require 'json)

(defvar *doi-utils-waiting* t
  "stores waiting state for url retrieval.")

(defvar *doi-utils-redirect* nil
  "stores redirect url from a callback function")

(defun doi-utils-redirect-callback (&optional status)
  "callback for url-retrieve to set the redirect"
  (when (plist-get status :error)
    (signal (car (plist-get status :error)) (cdr(plist-get status :error))))
  (when (plist-get status :redirect) ;  is nil if there none
    (message "redirects = %s" (plist-get status :redirect))
    (message "*doi-utils-redirect* set to %s"
	     (setq *doi-utils-redirect* (plist-get status :redirect))))
  ;; we have done our job, so we are not waiting any more.
  (setq *doi-utils-waiting* nil))

(defun doi-utils-get-redirect (doi)
  "get redirect url from dx.doi.org/doi"
  ;; we are going to wait until the url-retrieve is done
  (setq *doi-utils-waiting* t)
  ;; start with no redirect. it will be set in the callback.
  (setq *doi-utils-redirect* nil) 
  (url-retrieve 
   (format "http://dx.doi.org/%s" doi)
   'doi-utils-redirect-callback)
  ; I suspect we need to wait here for the asynchronous process to
  ; finish. we loop and sleep until the callback says it is done via
  ; `*doi-utils-waiting*'. this works as far as i can tell. Before I
  ; had to run this a few times to get it to work, which i suspect
  ; just gave the first one enough time to finish.
  (while *doi-utils-waiting* (sleep-for 0.1)))

(defvar doi-utils-pdf-url-functions nil
  "list of functions that return a url to a pdf from a redirect url. Each function takes one argument, the redirect url. The function must return a pdf-url, or nil.")

(defun aps-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://journals.aps.org" *doi-utils-redirect*)
    (replace-regexp-in-string "/abstract/" "/pdf/" *doi-utils-redirect*)))

(defun science-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://www.sciencemag.org" *doi-utils-redirect*)
    (concat *doi-utils-redirect* ".full.pdf")))

(defun nature-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://www.nature.com" *doi-utils-redirect*)
    (let ((result *doi-utils-redirect*))
      (setq result (replace-regexp-in-string "/full/" "/pdf/" result))
      (replace-regexp-in-string "\.html$" "\.pdf" result))))

(defun doi-utils-get-wiley-pdf-url (redirect-url)
  "wileyscience direct hides the pdf url in html. we get it out here"
  (setq *doi-utils-waiting* t)
  (url-retrieve redirect-url
		(lambda (status)
		  (beginning-of-buffer)
		  (re-search-forward "<iframe id=\"pdfDocument\" src=\"\\([^\"]*\\)\"" nil)
		  (setq *doi-utils-pdf-url* (match-string 1)
			*doi-utils-waiting* nil)))
  (while *doi-utils-waiting* (sleep-for 0.1))
  *doi-utils-pdf-url*)

(defun wiley-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://onlinelibrary.wiley.com" *doi-utils-redirect*)
   (doi-utils-get-wiley-pdf-url (replace-regexp-in-string "/abstract" "/pdf" *doi-utils-redirect*))
   *doi-utils-pdf-url*))

(defun springer-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://link.springer.com" *doi-utils-redirect*)
    (replace-regexp-in-string "/article/" "/content/pdf/" (concat *doi-utils-redirect* ".pdf"))))

(defun acs-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://pubs.acs.org" *doi-utils-redirect*)
    (replace-regexp-in-string "/abs/" "/pdf/" *doi-utils-redirect*)))

(defun iop-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://iopscience.iop.org" *doi-utils-redirect*)
    (let ((tail (replace-regexp-in-string "^http://iopscience.iop.org" "" *doi-utils-redirect*)))
      (concat "http://iopscience.iop.org" tail "/pdf" (replace-regexp-in-string "/" "_" tail) ".pdf"))))

(defun jstor-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://www.jstor.org" *doi-utils-redirect*)
    (concat (replace-regexp-in-string "/stable/" "/stable/pdfplus/" *doi-utils-redirect*) ".pdf")))

(defun aip-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://scitation.aip.org" *doi-utils-redirect*)
    ;; get stuff after content
    (let (p1 p2 s p3)
      (setq p2 (replace-regexp-in-string "^http://scitation.aip.org/" "" *doi-utils-redirect*))
      (setq s (split-string p2 "/"))
      (setq p1 (mapconcat 'identity (-remove-at-indices '(0 6) s) "/"))
      (setq p3 (concat "/" (nth 0 s) (nth 1 s) "/" (nth 2 s) "/" (nth 3 s)))
      (format "http://scitation.aip.org/deliver/fulltext/%s.pdf?itemId=/%s&mimeType=pdf&containerItemId=%s"
	      p1 p2 p3))))

(defun tandfonline-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://www.tandfonline.com" *doi-utils-redirect*)
    (replace-regexp-in-string "/abs/\\|/full/" "/pdf/" *doi-utils-redirect*)))

(defun ecs-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://jes.ecsdl.org" *doi-utils-redirect*)
    (replace-regexp-in-string "\.abstract$" ".full.pdf" *doi-utils-redirect*)))

(defun ecst-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://ecst.ecsdl.org" *doi-utils-redirect*)
    (concat *doi-utils-redirect* ".full.pdf")))

(defun rsc-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://pubs.rsc.org" *doi-utils-redirect*)
    (let ((url (downcase *doi-utils-redirect*)))
      (setq url (replace-regexp-in-string "articlelanding" "articlepdf" url))
      url)))

(defvar *doi-utils-pdf-url* nil
  "stores url to pdf download from a callback function")

(defun doi-utils-get-science-direct-pdf-url (redirect-url)
  "science direct hides the pdf url in html. we get it out here"
  (setq *doi-utils-waiting* t)
  (url-retrieve redirect-url
		(lambda (status)
		  (beginning-of-buffer)
		  (re-search-forward "pdfurl=\"\\([^\"]*\\)\"" nil t)
		  (setq *doi-utils-pdf-url* (match-string 1)
			*doi-utils-waiting* nil)))
  (while *doi-utils-waiting* (sleep-for 0.1))
  *doi-utils-pdf-url*)


(defun science-direct-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://www.sciencedirect.com" *doi-utils-redirect*)
    (doi-utils-get-science-direct-pdf-url *doi-utils-redirect*)
    *doi-utils-pdf-url*))

;; sometimes I get
;; http://linkinghub.elsevier.com/retrieve/pii/S0927025609004558
;; which actually redirect to
;; http://www.sciencedirect.com/science/article/pii/S0927025609004558
(defun linkinghub-elsevier-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://linkinghub.elsevier.com/retrieve" *doi-utils-redirect*)
    (let ((second-redirect (replace-regexp-in-string
			    "http://linkinghub.elsevier.com/retrieve"
			    "http://www.sciencedirect.com/science/article"
			    *doi-utils-redirect*)))
      (message "getting pdf url from %s" second-redirect)
      ;(doi-utils-get-science-direct-pdf-url second-redirect)
      *doi-utils-pdf-url*)))

(defun pnas-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http://www.pnas.org" *doi-utils-redirect*)
    (concat *doi-utils-redirect* ".full.pdf?with-ds=yes")))

(setq doi-utils-pdf-url-functions
      (list
       'aps-pdf-url
       'science-pdf-url
       'nature-pdf-url
       'wiley-pdf-url	     
       'springer-pdf-url
       'acs-pdf-url
       'iop-pdf-url
       'jstor-pdf-url
       'aip-pdf-url
       'science-direct-pdf-url
       'linkinghub-elsevier-pdf-url
       'tandfonline-pdf-url
       'ecs-pdf-url
       'ecst-pdf-url
       'rsc-pdf-url
       'pnas-pdf-url))

(defun doi-utils-get-pdf-url (doi)
  "returns a url to a pdf for the doi if one can be
calculated. Loops through the functions in `doi-utils-pdf-url-functions'
until one is found"
  (doi-utils-get-redirect doi)
  
  (unless *doi-utils-redirect*
    (error "No redirect found for %s" doi))
  (message "applying functions")
  (catch 'pdf-url
    (dolist (func doi-utils-pdf-url-functions)
     (message "calling %s" func)
      (let ((this-pdf-url (funcall func *doi-utils-redirect*)))
(message "t: %s" this-pdf-url)
	(when this-pdf-url
          (message "found pdf url: %s" this-pdf-url)
	  (throw 'pdf-url this-pdf-url))))))

(defun doi-utils-get-bibtex-entry-pdf ()
  "download pdf for entry at point if the pdf does not already
exist locally. The entry must have a doi. The pdf will be saved
to `org-ref-pdf-directory', by the name %s.pdf where %s is the
bibtex label. Files will not be overwritten. The pdf will be
checked to make sure it is a pdf, and not some html failure
page. you must have permission to access the pdf. We open the pdf
at the end."
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry) 
    (let (;; get doi, removing http://dx.doi.org/ if it is there.
	  (doi (replace-regexp-in-string
		"http://dx.doi.org/" ""
		(bibtex-autokey-get-field "doi")))	       
	  (key)
	  (pdf-url)
	  (pdf-file)
	  (content))
      ;; get the key and build pdf filename.
      (re-search-forward bibtex-entry-maybe-empty-head)
      (setq key (match-string bibtex-key-in-head))
      (setq pdf-file (concat org-ref-pdf-directory key ".pdf"))

      ;; now get file if needed.
      (when (and doi (not (file-exists-p pdf-file)))
	(setq pdf-url (doi-utils-get-pdf-url doi))
	(if pdf-url
	    (progn
	      (url-copy-file pdf-url pdf-file)
	      ;; now check if we got a pdf
	      (with-temp-buffer
		(insert-file-contents pdf-file)
		;; PDFS start with %PDF-1.x as the first few characters.
		(if (not (string= (buffer-substring 1 6) "%PDF-"))
		    (progn
		      (message "%s" (buffer-string))
		      (delete-file pdf-file))
		  (message "%s saved" pdf-file)))
	
	      (when (file-exists-p pdf-file)
		(org-open-file pdf-file)))
	  (message "No pdf-url found for %s at %s" doi *doi-utils-redirect* ))
	  pdf-file))))

(defun doi-utils-get-json-metadata (doi)
  (let ((url-request-method "GET") 
       (url-mime-accept-string "application/citeproc+json")
       (json-object-type 'plist))
    (with-current-buffer
	(url-retrieve-synchronously
	 (concat "http://dx.doi.org/" doi))
      (json-read-from-string (buffer-substring url-http-end-of-headers (point-max))))))       

(defun doi-utils-expand-template (s)
  "expand a template containing %{} with the eval of its contents"
  (replace-regexp-in-string "%{\\([^}]+\\)}"
                            (lambda (arg)
                              (let ((sexp (substring arg 2 -1)))
                                (format "%s" (eval (read sexp))))) s))

(defun doi-utils-doi-to-bibtex-string (doi)
  "return a bibtex entry as a string for the doi. Only articles are currently supported"
  (let (type
	results
	author
	title
	journal
	year
	volume
	number
	pages
	month
	url
	json-data)
    (setq results (doi-utils-get-json-metadata doi)
	  json-data (format "%s" results)
	  type (plist-get results :type)
	  author (mapconcat (lambda (x) (concat (plist-get x :given) " " (plist-get x :family)))
			    (plist-get results :author) " and ")
	  title (plist-get results :title)
	  journal (plist-get results :container-title)
	  volume (plist-get results :volume)
	  issue (plist-get results :issue)
	  year (elt (elt (plist-get (plist-get results :issued) :date-parts) 0) 0)
	  pages (plist-get results :page)
	  doi (plist-get results :DOI)
	  url (plist-get results :URL))
    (cond
     ((string= type "journal-article")
      (doi-utils-expand-template "@article{,
  author = 	 {%{author}},
  title = 	 {%{title}},
  journal = 	 {%{journal}},
  year = 	 {%{year}},
  volume = 	 {%{volume}},
  number = 	 {%{issue}},
  pages = 	 {%{pages}},
  doi =          {%{doi}},
  url =          {%{url}},
}"))
    (t (message-box "%s not supported yet." type)))))

(defun doi-utils-insert-bibtex-entry-from-doi (doi)
  "insert bibtex entry from a doi. Also cleans entry using
org-ref, and tries to download the corresponding pdf."
  (interactive "sDOI: ")
  (insert (doi-utils-doi-to-bibtex-string doi))
  (backward-char)
  (if (bibtex-key-in-head nil)
       (org-ref-clean-bibtex-entry t)
     (org-ref-clean-bibtex-entry))
   ;; try to get pdf
   (doi-utils-get-bibtex-entry-pdf)
   (save-selected-window
     (org-ref-open-bibtex-notes)))

(defun doi-utils-add-bibtex-entry-from-doi (doi)
  "add entry to end of first entry in `org-ref-default-bibliography'."
  (interactive "sDOI: ")
  (find-file (car org-ref-default-bibliography))
  (end-of-buffer)
  (insert "\n\n")
  (doi-utils-insert-bibtex-entry-from-doi doi))

(defun doi-utils-add-bibtex-entry-from-region (start end)
  "add entry assuming region is a doi to end of first entry in `org-ref-default-bibliography'."
  (interactive "r")
  (let ((doi (buffer-substring start end)))
    (find-file (car org-ref-default-bibliography))
    (end-of-buffer)
    (insert "\n")
    (doi-utils-insert-bibtex-entry-from-doi doi)))

(defun bibtex-set-field (field value)
  "set field to value in bibtex file. create field if it does not exist"
  (interactive "sfield: \nsvalue: ")
  (bibtex-beginning-of-entry)
  (let ((found))
    (if (setq found (bibtex-search-forward-field field t))
	;; we found a field
	(progn
	  (goto-char (car (cdr found)))
	  (when value
	    (bibtex-kill-field)
	    (bibtex-make-field field)
	    (backward-char)
	    (insert value)))
      ;; make a new field
      (message "new field being made")
      (bibtex-beginning-of-entry)
      (forward-line) (beginning-of-line)
      (bibtex-next-field nil)
      (forward-char)
      (bibtex-make-field field)
      (backward-char)
      (insert value))))

(defun plist-get-keys (plist)
   "return keys in a plist"
  (loop
   for key in results by #'cddr collect key))

(defun doi-utils-update-bibtex-entry-from-doi (doi)
  "update fields in a bibtex entry from the doi. Every field will be updated, so previous changes will be lost."
  (interactive (list
		(or (replace-regexp-in-string "http://dx.doi.org/" "" (bibtex-autokey-get-field "doi"))
		    (read-string "DOI: "))))
  (let* ((results (doi-utils-get-json-metadata doi))
	 (type (plist-get results :type))
	 (author (mapconcat
		  (lambda (x) (concat (plist-get x :given)
				    " " (plist-get x :family)))
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
	(doi (plist-get results :DOI)))
    
    ;; map the json fields to bibtex fields. The code each field is mapped to is evaluated.
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
    (mapcar
     (lambda (key)
       (eval (cdr (assoc key mapping))))
     (plist-get-keys results)))
  
  ; reclean entry, but keep key if it exists.
  (if (bibtex-key-in-head)
      (org-ref-clean-bibtex-entry t)
    (org-ref-clean-bibtex-entry)))

(provide 'doi-utils)
