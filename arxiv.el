;;; arxiv.el --- arxiv utilities for org-mode        -*- lexical-binding: t; -*-

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
;; this library creates a new org-link for Arxiv (http://arxiv.org/) entries,
;; and provides functions to retrieve bibtex entries from an Arxiv number.
;;
;; An Arxiv number might look like: cond-mat/0410285 or 1503.01742

;;; Code:
;; * The org-mode link
;; this just makes a clickable link that opens the entry.
(org-add-link-type
 "arxiv"
 ;; clicking
 (lambda (link-string) (browse-url (format "http://arxiv.org/abs/%s" link-string)))
 ;; formatting
(lambda (keyword desc format)
   (cond
    ((eq format 'html)
     (format  "<a href=\"http://arxiv.org/abs/%s\">arxiv:%s</a>" keyword  keyword))
    ((eq format 'latex)
     ;; write out the latex command
     (format "\\url{http://arxiv.org/abs/%s}" keyword)))))

;; arxiv:cond-mat/0410285

;; * Getting a bibtex entry for an arxiv article
;; For an arxiv article, there is a link to a NASA ADS page like this:
;; http://adsabs.harvard.edu/cgi-bin/bib_query?arXiv:1503.01742
;; On that page, there is a link to a bibtex entry:
;; http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=2015arXiv150301742H&data_type=BIBTEX&db_key=PRE&nocookieset=1
;;
;; It looks like you need to get a Bibliographic code from the arxiv number to
;; then get the bibtex entry.

(defun arxiv-get-bibliographic-code (arxiv-number)
  "Get Bibliographic code for ARXIV-NUMBER."
  (with-current-buffer
      (url-retrieve-synchronously
       (concat
	"http://adsabs.harvard.edu/cgi-bin/bib_query?arXiv:"
	arxiv-number))
    (search-forward-regexp "name=\\\"bibcode\\\" value=\\\"\\(.*\\)\\\"")
    (match-string 1)))

(defun arxiv-get-bibtex-entry (arxiv-bibliographic-code)
  "Get bibtex entry for ARXIV-BIBLIOGRAPHIC-CODE"
  (with-current-buffer
      (url-retrieve-synchronously
       (format
	"http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=%s&data_type=BIBTEX&db_key=PRE&nocookieset=1"
	arxiv-bibliographic-code))
    (goto-char  url-http-end-of-headers)
    (if (search-forward  "Retrieved 1 abstracts" (point-max) t)
	(progn
	  (forward-line)
	  (buffer-substring (point) (point-max)))
      (error "Did not get one entry: %s" (buffer-substring (point) (point-max))))))


(defun arxiv-add-bibtex-entry (arxiv-number bibfile)
  "Add bibtex entry for ARXIV-NUMBER to BIBFILE."
 (interactive
   (list (read-input "arxiv: ")
	 ;;  now get the bibfile to add it to
	 (ido-completing-read
	  "Bibfile: "
	  (append (f-entries "." (lambda (f) (f-ext? f "bib")))
		  org-ref-default-bibliography))))
 (save-window-excursion
   (find-file bibfile)
   (goto-char (point-max))
   (when (not (looking-at "^")) (insert "\n"))
   (insert (arxiv-get-bibtex-entry (arxiv-get-bibliographic-code arxiv-number)))
   (save-buffer)))


(provide 'arxiv)
;;; arxiv.el ends here
