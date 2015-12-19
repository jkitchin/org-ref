;;; org-ref-pdf.el --- Drag-n-drop PDF onto bibtex files  -*- lexical-binding: t; -*-

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

;; This library provides functions to enable drag-n-drop of pdfs onto a bibtex
;; buffer to add bibtex entries to it.

;;; Code:

(unless (executable-find "pdftotext")
  (error "pdftotext not found."))

(defun org-ref-extract-doi-from-pdf (pdf)
  "Try to extract a doi from a PDF file.
There may be more than one doi in the file. This function returns
all the ones it finds based on two patterns: doi: up to a quote,
bracket, space or end of line. dx.doi.org/up to a quote, bracket,
space or end of line.

If there is a trailing . we chomp it off. Returns a list of doi
strings, or nil.
"
  (with-temp-buffer
    (insert (shell-command-to-string (format "pdftotext %s -" pdf)))
    (goto-char (point-min))
    (let ((matches '()))
      (while (re-search-forward "dx.doi.org/\\(?1:[^]\n} \"]*\\)\\|\\(?:doi\\|DOI\\):\\s-?\\(?1:[^]}\n \"]*\\)" nil t)
	;; I don't know how to avoid a trailing . on some dois with the
	;; expression above, so if it is there, I chomp it off here.
	(let ((doi (match-string 1)))
	  (when (s-ends-with? "." doi)
	    (setq doi (substring doi 0 (- (length doi) 1))))
	  (add-to-list 'matches doi)))
      matches)))


(defun org-ref-pdf-doi-candidates (dois)
  "Generate candidate list for helm source.
Used when multiple dois are found in a pdf file."
  (loop for doi in dois
	collect
	(cons
	 (plist-get (doi-utils-get-json-metadata doi) :title)
	 doi)))


(defun org-ref-pdf-add-dois (candidate)
  "Add all entries for CANDIDATE in `helm-marked-candidates'."
  (loop for doi in (helm-marked-candidates)
	do
	(doi-utils-add-bibtex-entry-from-doi
	 doi
	 (buffer-file-name))))


(defun org-ref-pdf-dnd-func (event)
  "Drag-n-drop support to add a bibtex entry from a pdf file."
  (interactive "e")
  (goto-char (nth 1 (event-start event)))
  (x-focus-frame nil)
  (let* ((payload (car (last event)))
         (pdf (cadr payload))
	 (dois (org-ref-extract-doi-from-pdf pdf)))
    (message "%s" dois)
    dois
    (cond
     ((null dois)
      (message "No doi found in %s" pdf))
     ((= 1 (length dois))
      (doi-utils-add-bibtex-entry-from-doi
       (car dois)
       (buffer-file-name)))
     ;; Multiple DOIs found
     (t
      (helm :sources `((name . "Select a DOI")
		       (candidates . ,(org-ref-pdf-doi-candidates dois))
		       (action . org-ref-pdf-add-dois)))))))

(define-key bibtex-mode-map (kbd "<drag-n-drop>") 'org-ref-pdf-dnd-func)

(provide 'org-ref-pdf)
;;; org-ref-pdf.el ends here
