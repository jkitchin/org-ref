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

;; Drag a webpage onto a bibtex file to insert a bibtex entry. If the webpage is
;; known in `org-ref-scrape-doi', it will scrape a doi, and then use `doi-utils'
;; to insert a bibtex entry into the buffer.

;;; Code:
(require 'doi-utils)

(defvar org-ref-url-scrapers
  '( ;; ACS journals
    ((string-match "http://pubs.acs.org" url)
     (with-current-buffer (url-retrieve-synchronously url)
       (re-search-forward "scheme=\"doi\" content=\"\\([^\"]*\\)\"")
       (match-string 1)))

    ;; AIP journals
    ((string-match "http://scitation.aip.org/" url)
     (with-current-buffer (url-retrieve-synchronously url)
       (re-search-forward "citation_doi\" content=\"\\([^\"]*\\)\"")
       (match-string 1)))

    ;; APS journals
    ((string-match "http://journals.aps.org" url)
     (with-current-buffer (url-retrieve-synchronously url)
       (re-search-forward "data-doi=\"\\([^\"]*\\)\"")
       (match-string 1)))

    ;; http://ecst.ecsdl.org ECS
    ((string-match "http://ecst.ecsdl.org" url)
     (with-current-buffer (url-retrieve-synchronously url)
       (re-search-forward "content=\"\\([^\"]*\\)\" name=\"citation_doi")
       (match-string 1)))

    ;; http://iopscience.iop.org
    ((string-match "http://iopscience.iop.org" url)
     (with-current-buffer (url-retrieve-synchronously url)
       (re-search-forward "name=\"citation_doi\" content=\"\\([^\"]*\\)\"")
       (match-string 1)))

    ;; http://jes.ecsdl.org
    ((string-match "http://jes.ecsdl.org" url)
     (with-current-buffer (url-retrieve-synchronously url)
       (re-search-forward "content=\"\\([^\"]*\\)\" name=\"citation_doi")
       (match-string 1)))

    ;; http://www.jstor.org
    ((string-match "http://www.jstor.org" url)
     (with-current-buffer (url-retrieve-synchronously url)
       (re-search-forward "objectDOI\" : \"\\([^\"]*\\)\"")
       (match-string 1)))

    ;; <meta name="citation_doi" content="doi:10.1038/nature15540">
    ((string-match "http://www.nature.com" url)
     (with-current-buffer (url-retrieve-synchronously url)
       (re-search-forward "name=\"citation_doi\" content=\"doi:\\([^\"]*\\)\"")
       (match-string 1)))

    ;; http://www.pnas.org
    ((string-match "http://www.pnas.org" url)
     (with-current-buffer (url-retrieve-synchronously url)
       (re-search-forward "content=\"\\([^\"]*\\)\" name=\"citation_doi")
       (match-string 1)))

    ;; http://pubs.rsc.org
    ((string-match "http://pubs.rsc.org" url)
     (with-current-buffer (url-retrieve-synchronously url)
       (re-search-forward "scheme=\"doi\" content=\"\\([^\"]*\\)\"")
       (match-string 1)))

    ;; Science Magazine
    ((string-match "http://www.sciencemag.org" url)
     (with-current-buffer (url-retrieve-synchronously url)
       (re-search-forward "content=\"\\([^\"]*\\)\" name=\"citation_doi")
       (match-string 1)))

    ;; ScienceDirect
    ((string-match "http://www.sciencedirect.com/science/article" url)
     (with-current-buffer (url-retrieve-synchronously url)
       (re-search-forward "doi = '\\([^']*\\)'")
       (match-string 1)))

    ;; Springer journals
    ((string-match "http://link.springer.com" url)
     (with-current-buffer (url-retrieve-synchronously url)
       (re-search-forward "name=\"citation_doi\" content=\"\\([^\"]*\\)\"")
       (match-string 1)))

    ;; http://www.tandfonline.com Taylor and Francis
    ((string-match "http://www.tandfonline.com" url)
     (with-current-buffer (url-retrieve-synchronously url)
       (re-search-forward "scheme=\"doi\" content=\"\\([^\"]*\\)\"")
       (match-string 1)))

    ;; Wiley journals
    ((string-match "http://onlinelibrary.wiley.com" url)
     (with-current-buffer (url-retrieve-synchronously url)
       (re-search-forward "name=\"citation_doi\" content=\"\\([^\"]*\\)\"")
       (match-string 1)))

    ;; A Pubmed page
    ((string-match "http://www.ncbi.nlm.nih.gov/pubmed" url)
     (with-current-buffer (url-retrieve-synchronously url)
       (re-search-forward "\"http://dx.doi.org/\\([^\"]*\\)\"")
       (match-string 1))))
  "cond blocks to match urls, and a recipe to extract a DOI. This
  variable exists to make it easy for users to add new recipes.")

(defun org-ref-scrape-doi (url)
  "Scrape a doi from a URL.
These all work by regular expressions that were
reverse-engineered for each publisher. You can add new scraper
recipes by adding to `org-ref-url-scrapers'. A recipe looks like:
((conditional statement matching a url) expressions that return a doi.)"
  (eval `(cond
	  ,@org-ref-url-scrapers
	  (t
	   (message
	    "We don't know how to get a doi from %s. Try adding the doi by hand."
	    url)
	   nil))))


(defun org-ref-url-dnd-doi-func (event)
  "Drag-n-Drop support to add a bibtex entry from a url."
  (interactive "e")
  (goto-char (nth 1 (event-start event)))
  (x-focus-frame nil)
  (let* ((payload (car (last event)))
         (url (cadr payload))
	 (doi (org-ref-scrape-doi url)))
    (if doi
	(doi-utils-add-bibtex-entry-from-doi doi (buffer-file-name))
      (message "No DOI found at %s." url))))

(defun org-ref-url-dnd-protocol (url action)
  "Protocol function for use in `dnd-protocol-alist'.
If a doi is found, add a bibtex entry from it. Otherwise, create
a misc entry, with prompt for key."
  (when (f-ext? (buffer-file-name) "bib")
    (let ((doi (org-ref-scrape-doi url)))
      (cond
       (doi
	(doi-utils-add-bibtex-entry-from-doi doi (buffer-file-name))
	action)
       ;; no doi found. add misc entry
       (t
	(goto-char (point-max))
	(insert (format "\n@misc{,
  url = {%s},
  note = {Last accessed %s}
}"
			url
			(current-time-string)))
	(bibtex-clean-entry)
	action)))))


;; (define-key bibtex-mode-map (kbd "<drag-n-drop>") 'org-ref-url-dnd-doi-func)
(add-to-list 'dnd-protocol-alist '("^https?" . org-ref-url-dnd-protocol))

(provide 'org-ref-url-utils)
;;; org-ref-url-utils.el ends here
