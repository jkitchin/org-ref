;;; org-ref-arxiv.el --- arxiv utilities for org-mode        -*- lexical-binding: t; -*-

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
(require 'bibtex)
(require 'dash)
(require 'f)
(require 'org)
(require 's)
(require 'org-ref-utils)
(require 'parsebib)


;; This is a local variable defined in `url-http'.  We need it to avoid
;; byte-compiler errors.
(defvar url-http-end-of-headers)
(defvar org-ref-default-bibliography)
(defvar org-ref-pdf-directory)

(declare-function parsebib-find-bibtex-dialect "parsebib")
(declare-function org-ref-clean-bibtex-entry "org-ref-core")
;; this is a C function
(declare-function libxml-parse-xml-region "xml")

;;* The org-mode link
;; this just makes a clickable link that opens the entry.
;; example: arxiv:cond-mat/0410285
(org-ref-link-set-parameters "arxiv"
  :follow (lambda (link-string)
            (browse-url (format "http://arxiv.org/abs/%s" link-string)))
  :export (lambda (keyword desc format)
            (cond
             ((eq format 'html)
              (format  "<a href=\"http://arxiv.org/abs/%s\">arxiv:%s</a>"
                       keyword  (or desc keyword)))
             ((eq format 'latex)
              ;; write out the latex command
              (format "\\url{http://arxiv.org/abs/%s}{%s}" keyword (or desc keyword))))))

;;* Getting a bibtex entry for an arXiv article using remote service:
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
    (search-forward-regexp "<link rel=\"canonical\" href=\"http://ui.adsabs.harvard.edu/abs/\\(.*\\)/abstract\"/>")
    (match-string 1)))


(defun arxiv-get-bibtex-entry (arxiv-bibliographic-code)
  "Get bibtex entry for ARXIV-BIBLIOGRAPHIC-CODE."
  (with-current-buffer
      (url-retrieve-synchronously (format "https://ui.adsabs.harvard.edu/abs/%s/exportcitation" arxiv-bibliographic-code))
    (when (re-search-forward
	   "<textarea.*>\\(.*\\(?:\n.*\\)*?\\(?:\n\\s-*\n\\|\\'\\)\\)</textarea>"
	   nil t)
      (xml-substitute-special (match-string 1)))))

;;* Getting a bibtex entry for an arXiv article using arXiv API:
;; Retrieves the meta data of an article view arXiv's http API,
;; extracts the necessary information, and formats a new BibTeX entry.

(defvar arxiv-entry-format-string "@article{%s,
  journal = {CoRR},
  title = {%s},
  author = {%s},
  archivePrefix = {arXiv},
  year = {%s},
  eprint = {%s},
  primaryClass = {%s},
  abstract = {%s},
  url = {%s},
}"
	"Template for BibTeX entries of arXiv articles.")


(defun arxiv-get-bibtex-entry-via-arxiv-api (arxiv-number)
  "Retrieve meta data for ARXIV-NUMBER.
Returns a formatted BibTeX entry."
  (with-current-buffer
      (url-retrieve-synchronously (format "http://export.arxiv.org/api/query?id_list=%s" arxiv-number) t)
    (let* ((parse-tree (libxml-parse-xml-region
                        (progn (goto-char 0)
                               (search-forward "<?xml ")
                               (match-beginning 0))
                        (point-max)))
           (entry (assq 'entry parse-tree))
           (authors (--map (nth 2 (nth 2 it))
                           (--filter (and (listp it) (eq (car it) 'author)) entry)))
           (year (format-time-string "%Y" (date-to-time (nth 2 (assq 'published entry)))))
           (title (nth 2 (assq 'title entry)))
           (names (arxiv-bibtexify-authors authors))
           (category (cdar (nth 1 (assq 'primary_category entry))))
           (abstract (s-trim (nth 2 (assq 'summary entry))))
           (url (nth 2 (assq 'id entry)))
           (temp-bibtex (format arxiv-entry-format-string "" title names year arxiv-number category abstract url))
           (key (with-temp-buffer
                  (insert temp-bibtex)
		  (bibtex-mode)
		  (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
		  (org-ref-replace-nonascii)
                  (bibtex-generate-autokey)))
	   (doi (assq 'doi entry)))
      (if doi
	  (doi-utils-doi-to-bibtex-string (nth 2 doi))
	;; no doi, so we fall back to the simple template
	(format arxiv-entry-format-string key title names year arxiv-number category abstract url)))))


(defun arxiv-bibtexify-authors (authors)
  "Return names in 'SURNAME, FIRST NAME' format from AUTHORS list."
  (s-join " and "
          (--map (concat (-last-item it) ", " (s-join " " (-remove-last 'stringp it)))
                 (--map (s-split " +" it) authors))))


(defun arxiv-maybe-arxiv-id-from-current-kill ()
  "Try to get an arxiv ID from the current kill."
  (let* ((the-current-kill (ignore-errors (current-kill 0 t)))  ;; nil if empty kill ring
         (arxiv-url-prefix-regexp "^https?://arxiv\\.org/\\(pdf\\|abs\\|format\\)/")
         (arxiv-cite-prefix-regexp "^\\(arXiv\\|arxiv\\):")
         (arxiv-id-old-regexp "[a-z-]+\\(\\.[A-Z]\\{2\\}\\)?/[0-9]\\{5,7\\}$") ; Ex: math.GT/0309136
         (arxiv-id-new-regexp "[0-9]\\{4\\}[.][0-9]\\{4,5\\}\\(v[0-9]+\\)?$") ; Ex: 1304.4404v2
         (arxiv-id-regexp (concat "\\(" arxiv-id-old-regexp "\\|" arxiv-id-new-regexp "\\)")))
  (cond
   (;; make sure current-kill has something in it
    ;; if current-kill is not a string, return nil
    (not (stringp the-current-kill))
    nil)
   (;; check if current-kill looks like an arxiv ID
    ;; if so, return it
    ;; Ex: 1304.4404v2
    (s-match (concat "^" arxiv-id-regexp) the-current-kill)
    the-current-kill)
   (;; check if current-kill looks like an arxiv cite
    ;; if so, remove the prefix and return
    ;; Ex: arXiv:1304.4404v2 --> 1304.4404v2
    (s-match (concat arxiv-cite-prefix-regexp arxiv-id-regexp) the-current-kill)
    (replace-regexp-in-string arxiv-cite-prefix-regexp "" the-current-kill))
   (;; check if current-kill looks like an arxiv url
    ;; if so, remove the url prefix and return
    ;; Ex: https://arxiv.org/pdf/1304.4404 --> 1304.4404
    (s-match (concat arxiv-url-prefix-regexp arxiv-id-regexp) the-current-kill)
    (replace-regexp-in-string arxiv-url-prefix-regexp "" the-current-kill))
   ;; otherwise, return nil
   (t
    nil))))


;;;###autoload
(defun arxiv-add-bibtex-entry (arxiv-number bibfile)
  "Add bibtex entry for ARXIV-NUMBER to BIBFILE."
  (interactive
   (list (read-string
          "arxiv: "
          (arxiv-maybe-arxiv-id-from-current-kill))
         ;;  now get the bibfile to add it to
         (completing-read
          "Bibfile: "
          (append (f-entries "." (lambda (f) (f-ext? f "bib")))
                  org-ref-default-bibliography))))
  (save-window-excursion
    (find-file bibfile)
    (goto-char (point-max))
    (when (not (looking-at "^")) (insert "\n"))
    (insert (arxiv-get-bibtex-entry-via-arxiv-api arxiv-number))
    (org-ref-clean-bibtex-entry)
    (goto-char (point-max))
    (when (not (looking-at "^")) (insert "\n"))
    (save-buffer)))


;;;###autoload
(defun arxiv-get-pdf (arxiv-number pdf)
  "Retrieve a pdf for ARXIV-NUMBER and save it to PDF."
  (interactive
   (list (read-string
          "arxiv: "
          (arxiv-maybe-arxiv-id-from-current-kill))
         (read-string
          "PDF: ")))
  (let ((pdf-url (with-current-buffer
                     (url-retrieve-synchronously
                      (concat
                       "http://arxiv.org/abs/" arxiv-number))
                   ;; <meta name="citation_pdf_url" content="http://arxiv.org/pdf/0801.1144" />
                   (goto-char (point-min))
                   (search-forward-regexp
                    "name=\\\"citation_pdf_url\\\" content=\\\"\\(.*\\)\\\"")
                   (match-string 1))))
    (url-copy-file pdf-url pdf)
    ;; now check if we got a pdf
    (if (org-ref-pdf-p pdf)
        (org-open-file pdf)
      (delete-file pdf)
      (message "Error downloading arxiv pdf %s" pdf-url))))

;;;###autoload
(defun arxiv-get-pdf-add-bibtex-entry (arxiv-number bibfile pdfdir)
  "Add bibtex entry for ARXIV-NUMBER to BIBFILE.
Remove troublesome chars from the bibtex key, retrieve a pdf
for ARXIV-NUMBER and save it to PDFDIR with the same name of the
key."
  (interactive
   (list (read-string
          "arxiv: "
          (arxiv-maybe-arxiv-id-from-current-kill))
         ;;  now get the bibfile to add it to
         (completing-read
          "Bibfile: "
          (append (f-entries "." (lambda (f) (f-ext? f "bib")))
                  org-ref-default-bibliography))
         (read-directory-name
          "PDF directory: "
          org-ref-pdf-directory)))

  (arxiv-add-bibtex-entry arxiv-number bibfile)

  (save-window-excursion
    (let ((key ""))
      (find-file bibfile)
      (goto-char (point-max))
      (bibtex-beginning-of-entry)
      (re-search-forward bibtex-entry-maybe-empty-head)
      (if (match-beginning bibtex-key-in-head)
          (progn
            (setq key (delete-and-extract-region
                       (match-beginning bibtex-key-in-head)
                       (match-end bibtex-key-in-head)))
            ;; remove potentially troublesome characters from key
            ;; as it will be used as  a filename
            (setq key (replace-regexp-in-string   "\"\\|\\*\\|/\\|:\\|<\\|>\\|\\?\\|\\\\\\||\\|\\+\\|,\\|\\.\\|;\\|=\\|\\[\\|]\\|!\\|@"
                                                  "" key))
            ;; check if the key is in the buffer
            (when (save-excursion
                    (bibtex-search-entry key))
              (save-excursion
                (bibtex-search-entry key)
                (bibtex-copy-entry-as-kill)
                (switch-to-buffer-other-window "*duplicate entry*")
                (bibtex-yank))
              (setq key (bibtex-read-key "Duplicate Key found, edit: " key))))
        (setq key (bibtex-read-key "Key not found, insert: ")))
      (insert key)
      (arxiv-get-pdf arxiv-number (concat pdfdir key ".pdf")))))

(provide 'org-ref-arxiv)
;;; org-ref-arxiv.el ends here
