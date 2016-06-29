;;; org-ref-url-utils.el --- Utility functions to scrape DOIs from urls  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Keywords:

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

;; Drag a webpage onto a bibtex file to insert a bibtex entry.

;; This works by scraping DOIs from the content at the URL using patterns in
;; `org-ref-doi-regexps'. If one DOI is found, it is added as an entry. If
;; multiple DOIs are found, you will get a helm selection buffer to choose what
;; you want to add. You can add new patterns to `org-ref-doi-regexps'.

;; You can press Control to "debug" a URL, which will open a buffer of the
;; content with the current DOI patterns highlighted. If you want to get all the
;; DOIs at a URL, you can press Meta during the drag-n-drop.

;;; Code:
(defvar org-ref-bibliography-entry-format)
(defvar org-ref-get-pdf-filename-function)
(defvar org-ref-notes-function)
(defvar org-ref-cite-types)

(declare-function 'org-ref-get-bibtex-key-and-file "org-ref-core.el")
(declare-function 'org-ref-find-bibliography "org-ref-core.el")
(declare-function 'org-ref-key-in-file-p "org-ref-core.el")
(declare-function 'org-ref-bib-citation "org-ref-core.el")
(declare-function 'org-ref-get-bibtex-key-under-cursor "org-ref-core.el")

(require 'doi-utils)
(require 'f)
(eval-when-compile
  (require 'cl))

(defgroup org-ref-url nil
  "Customization group for org-ref-url-utils"
  :tag "Org Ref URL"
  :group 'org-ref-url-utils)


(defcustom org-ref-doi-regexps
  '("scheme=\"doi\" content=\"\\([^\"]*\\)\""
    "citation_doi\" content=\"\\([^\"]*\\)\""
    "data-doi=\"\\([^\"]*\\)\""
    "content=\"\\([^\"]*\\)\" name=\"citation_doi"
    "objectDOI\" : \"\\([^\"]*\\)\""
    "doi = '\\([^']*\\)'"
    "\"http://dx.doi.org/\\([^\"]*\\)\""
    "/doi/\\([^\"]*\\)\">"
    "doi/full/\\(.*\\)&"
    "doi=\\([^&]*\\)&amp")
  "List of regexps to match a DOI.
The doi should be in group 1 so that (match-string 1) contains
the DOI."
  :type '(repeat regexp)
  :group 'org-ref-url-utils)


;;* Scrape DOIs from a URL

(defun org-ref-url-scrape-dois (url)
  "Scrape all dois from a URL matching a pattern in `org-ref-doi-regexps'.
Returns a list of collected DOIs in the order found."
  (let ((dois '()))
    (with-current-buffer (url-retrieve-synchronously url)
      (cl-loop for doi-pattern in org-ref-doi-regexps
	    do
	    (goto-char (point-min))
	    (while (re-search-forward doi-pattern nil t)
	      (cl-pushnew (match-string 1) dois :test #'equal)))
      (reverse dois))))


(defun org-ref-url-add-doi-entries (_)
  "Add all entries for CANDIDATE in `helm-marked-candidates'.
This is used in a helm selection command in `org-ref-url-dnd-protocol'."
  (cl-loop for doi in (helm-marked-candidates)
	   do
	   (doi-utils-add-bibtex-entry-from-doi
	    doi
	    (buffer-file-name))
	   ;; this removes two blank lines before each entry.
	   (bibtex-beginning-of-entry)
	   (delete-char -2)))


(defun org-ref-url-dnd-protocol (url action)
  "Protocol function for use in `dnd-protocol-alist'.
We scrape DOIs from the url first. If there is one, we add it. If
there is more than one, we offer a helm buffer of selections. If
no DOI is found, we create a misc entry, with a prompt for a key."
  ;; make sure we are on a bib-file
  (if (and (buffer-file-name)
	   (f-ext? (buffer-file-name) "bib"))
      (let ((dois (org-ref-url-scrape-dois url)))
	(cond
	 ;; One doi found. Assume it is what we want.
	 ((= 1 (length dois))
	  (doi-utils-add-bibtex-entry-from-doi
	   (car dois)
	   (buffer-file-name))
	  action)
	 ;; Multiple DOIs found
	 ((> (length dois) 1)
	  (helm :sources
		`((name . "Select a DOI")
		  (candidates . ,(let ((dois '()))
				   (with-current-buffer (url-retrieve-synchronously url)
				     (cl-loop for doi-pattern in org-ref-doi-regexps
					      do
					      (goto-char (point-min))
					      (while (re-search-forward doi-pattern nil t)
						(cl-pushnew
						 ;; Cut off the doi, sometimes
						 ;; false matches are long.
						 (cons (format "%40s | %s"
							       (substring
								(match-string 1)
								0 (min
								   (length (match-string 1))
								   40))
							       doi-pattern)
						       (match-string 1))
						 dois
						 :test #'equal)))
				     (reverse dois))))
		  (action . org-ref-url-add-doi-entries)))
	  action)
	 ;; No DOIs found, add a misc entry.
	 (t
	  (goto-char (point-max))
	  (insert (format "\n@misc{,
    url = {%s},
    note = {Last accessed %s}
  }"
			  url
			  (current-time-string)))
	  (bibtex-clean-entry)
	  action)))
    ;; pass back to dnd. Copied from `org-download-dnd'. Apparently
    ;; returning nil does not do this.
    (let ((dnd-protocol-alist
	   (rassq-delete-all
	    'org-ref-url-dnd-protocol
	    (copy-alist dnd-protocol-alist))))
      (dnd-handle-one-url nil action url))))


(add-to-list 'dnd-protocol-alist '("^https?" . org-ref-url-dnd-protocol))

;;* Enable a DOI to be dragged onto a bibtex buffer
(defun org-ref-doi-dnd-protocol (doi action)
  "Protocol for when a doi is dragged onto a bibtex file.
A doi will be either doi:10.xxx  or 10.xxx."
  (if (and (buffer-file-name)
	   (f-ext? (buffer-file-name) "bib"))
      (let ((doi (dnd-unescape-uri doi)))
	;; Get the actual doi now
	(string-match "\\(?:DOI\\|doi\\)?:? *\\(10.*\\)" doi)
	(setq doi (match-string 1 doi))
	(when doi
	  (doi-add-bibtex-entry doi (buffer-file-name))
	  (save-buffer)
	  action))
    ;; not on a bib file
    (let ((dnd-protocol-alist
	   (rassq-delete-all
	    'org-ref-url-dnd-protocol
	    (copy-alist dnd-protocol-alist))))
      (dnd-handle-one-url nil action doi))))

(add-to-list 'dnd-protocol-alist '("^doi" . org-ref-doi-dnd-protocol))
(add-to-list 'dnd-protocol-alist '("^10" . org-ref-doi-dnd-protocol))


;;* Debug URL in a buffer with C-dnd

;; You can use this to see if there are any DOIs in a URL, and to use re-builder
;; to add new patterns to `org-ref-doi-regexps'.
;;;###autoload
(defun org-ref-url-debug-url (url)
  "Open a buffer to URL with all doi patterns highlighted."
  (interactive)
  (switch-to-buffer
   (url-retrieve-synchronously url))
  (highlight-regexp
   (mapconcat 'identity org-ref-doi-regexps "\\|")))


;;;###autoload
(defun org-ref-url-dnd-debug (event)
  "Drag-n-drop function to debug a url."
  (interactive "e")
  (org-ref-url-debug-url (cadr (car (last event)))))


(define-key bibtex-mode-map (kbd "<C-drag-n-drop>") 'org-ref-url-dnd-debug)

;;* Add all DOI bibtex entries with M-dnd

(defun org-ref-url-add-all-doi-entries (url)
  "Add all DOI bibtex entries for URL."
  (cl-loop for doi in (org-ref-url-scrape-dois url)
	do
	(ignore-errors
	  (doi-utils-add-bibtex-entry-from-doi
	   doi
	   (buffer-file-name))
	  ;; this removes two blank lines before each entry.
	  (bibtex-beginning-of-entry)
	  (delete-char -2))))


;;;###autoload
(defun org-ref-url-dnd-all (event)
  "Drag-n-drop function to get all DOI bibtex entries for a url.
You probably do not want to do this since the DOI patterns are
not perfect, and some hits are not actually DOIs."
  (interactive "e")
  (org-ref-url-add-all-doi-entries (cadr (car (last event)))))

(define-key bibtex-mode-map (kbd "<M-drag-n-drop>") 'org-ref-url-dnd-all)


;; Get first DOI if there is one with s-dnd
(defun org-ref-url-add-first-doi-entry (url)
  "Add first DOI bibtex entry for URL if there is one."
  (let* ((dois (org-ref-url-scrape-dois url))
	 (doi (car dois)))
    (if doi
	(progn
	  (doi-utils-add-bibtex-entry-from-doi
	   doi
	   (buffer-file-name))
	  ;; this removes two blank lines before each entry.
	  (bibtex-beginning-of-entry)
	  (delete-char -2))
      ;; no doi, add misc
      (goto-char (point-max))
	  (insert (format "\n@misc{,
    url = {%s},
    note = {Last accessed %s}
  }"
			  url
			  (current-time-string)))
	  (bibtex-clean-entry))))


;;;###autoload
(defun org-ref-url-dnd-first (event)
  "Drag-n-drop function to download the first DOI in a url."
  (interactive "e")
  (org-ref-url-add-first-doi-entry (cadr (car (last event)))))

(define-key bibtex-mode-map (kbd "<s-drag-n-drop>") 'org-ref-url-dnd-first)


(provide 'org-ref-url-utils)
;;; org-ref-url-utils.el ends here
