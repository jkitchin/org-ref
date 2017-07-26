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

;; TODO: If no DOI is found, figure out a way to do a crossref/google query to
;; get a doi. This needs a reliable title/citation.

;;; Code:

(require 'f)
(require 'pdf-tools)
(eval-when-compile
  (require 'cl))

(declare-function org-ref-bibtex-key-from-doi "org-ref-bibtex.el")

(defgroup org-ref-pdf nil
  "Customization group for org-ref-pdf"
  :tag "Org Ref PDF"
  :group 'org-ref-pdf)

(defcustom pdftotext-executable
  "pdftotext"
  "Executable for pdftotext. Set if the executable is not on your
path, or you want to use another version."
  :type 'file
  :group 'org-ref-pdf)

(defcustom org-ref-pdf-doi-regex
  "dx.doi.org/\\(?1:[^]\n} \"]*\\)\\|\\(?:doi\\|DOI\\):?\\s-?\\(?1:[^]}\n \"]*\\)"
  "Regular expression to match DOIs in a pdf converted to text.
The DOI should be in group 1 of the regex.
The default pattern matches:
1. http://dx.do.org/doi
2. doi: doi, where the colon is optional"
  :type 'regexp
  :group 'org-ref-pdf)

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
    (insert (shell-command-to-string (format "%s %s -"
					     pdftotext-executable
					     (shell-quote-argument (dnd-unescape-uri pdf)))))
    (goto-char (point-min))
    (let ((matches '()))
      (while (re-search-forward org-ref-pdf-doi-regex nil t)
	;; I don't know how to avoid a trailing . on some dois with the
	;; expression above, so if it is there, I chomp it off here.
	(let ((doi (match-string 1)))
	  (when (s-ends-with? "." doi)
	    (setq doi (substring doi 0 (- (length doi) 1))))
	  (cl-pushnew doi matches :test #'equal)))
      matches)))


(defun org-ref-pdf-doi-candidates (dois)
  "Generate candidate list for helm source.
Used when multiple dois are found in a pdf file."
  (cl-loop for doi in dois
	collect
	(condition-case nil
	    (cons
	     (plist-get (doi-utils-get-json-metadata doi) :title)
	     doi)
	  (error (cons (format "%s read error" doi) doi)))))


(defun org-ref-pdf-add-dois (_)
  "Add all entries for CANDIDATE in `helm-marked-candidates'."
  (cl-loop for doi in (helm-marked-candidates)
	   do
	   (doi-utils-add-bibtex-entry-from-doi
	    doi
	    (buffer-file-name))))

;;;###autoload
(defun org-ref-pdf-to-bibtex ()
  "Add pdf of current buffer to bib file and save pdf to
`org-ref-default-bibliography'. The pdf should be open in Emacs
using the `pdf-tools' package."
  (interactive)
  (when (not (f-ext? (buffer-file-name) "pdf"))
    (error "Buffer is not a pdf file"))
  ;; Get doi from pdf of current buffer
  (let* ((dois (org-ref-extract-doi-from-pdf (buffer-file-name)))
         (doi-utils-download-pdf nil)
         (doi (if (= 1 (length dois))
                  (car dois)
                (completing-read "Select DOI: " dois))))
    ;; Add bib entry from doi:
    (doi-utils-add-bibtex-entry-from-doi doi)
    ;; Copy pdf to `org-ref-pdf-directory':
    (let ((key (org-ref-bibtex-key-from-doi doi)))
      (copy-file (buffer-file-name)
                 (expand-file-name (format "%s.pdf" key)
                                   org-ref-pdf-directory)))))

;;;###autoload
;; (defun org-ref-pdf-dnd-func (event)
;;   "Drag-n-drop support to add a bibtex entry from a pdf file."
;;   (interactive "e")
;;   (goto-char (nth 1 (event-start event)))
;;   (x-focus-frame nil)
;;   (let* ((payload (car (last event)))
;;          (pdf (cadr payload))
;; 	 (dois (org-ref-extract-doi-from-pdf pdf))) 
;;     (cond
;;      ((null dois)
;;       (message "No doi found in %s" pdf))
;;      ((= 1 (length dois))
;;       (doi-utils-add-bibtex-entry-from-doi
;;        (car dois)
;;        (buffer-file-name)))
;;      ;; Multiple DOIs found
;;      (t
;;       (helm :sources `((name . "Select a DOI")
;; 		       (candidates . ,(org-ref-pdf-doi-candidates dois))
;; 		       (action . org-ref-pdf-add-dois)))))))

;; This isn't very flexible, as it hijacks all drag-n-drop events. I switched to
;; using `dnd-protocol-alist'.
;; (define-key bibtex-mode-map (kbd "<drag-n-drop>") 'org-ref-pdf-dnd-func)

;; This is what the original dnd function was.
;; (define-key bibtex-mode-map (kbd "<drag-n-drop>") 'ns-drag-n-drop)


;; I replaced the functionality above with this new approach that leverages
;; ns-drag-n-drop. An alternative approach would be to adapt the function above
;; so that if the item dragged on wasn't a pdf, it would use another function.
;; that is essentially what ns-drag-n-drop enables, multiple handlers for
;; different uris that get dropped on the windwo.

(defun org-ref-pdf-dnd-protocol (uri action)
  "Drag-n-drop protocol.
PDF will be a string like file:path.
ACTION is what to do. It is required for `dnd-protocol-alist'.
This function should only apply when in a bibtex file." 
  (if (and (buffer-file-name)
	   (f-ext? (buffer-file-name) "bib"))
      (let* ((path (substring uri 5))
	     dois) 
	(cond
	 ((f-ext? path "pdf")
	  (setq dois (org-ref-extract-doi-from-pdf
		      path))
	  (cond
	   ((null dois)
	    (message "No doi found in %s" path)
	    nil)
	   ((= 1 (length dois))
	    ;; we do not need to get the pdf, since we have one.
	    (let ((doi-utils-download-pdf nil))
	      (doi-utils-add-bibtex-entry-from-doi
	       (car dois)
	       (buffer-file-name))
	      ;; we should copy the pdf to the pdf directory though
	      (let ((key (cdr (assoc "=key=" (bibtex-parse-entry)))))
	      	(copy-file (dnd-unescape-uri path) (expand-file-name (format "%s.pdf" key) org-ref-pdf-directory))))
	    action)
	   ;; Multiple DOIs found
	   (t
	    (helm :sources `((name . "Select a DOI")
			     (candidates . ,(org-ref-pdf-doi-candidates dois))
			     (action . org-ref-pdf-add-dois)))
	    action)))
	 ;; drag a bib file on and add contents to the end of the file.
	 ((f-ext? path "bib")
	  (goto-char (point-max))
	  (insert "\n")
	  (insert-file-contents path))))
    ;; ignoring. pass back to dnd. Copied from `org-download-dnd'. Apparently
    ;; returning nil does not do this.
    (let ((dnd-protocol-alist
           (rassq-delete-all
            'org-ref-pdf-dnd-protocol
            (copy-alist dnd-protocol-alist))))
      (dnd-handle-one-url nil action uri))))


(add-to-list 'dnd-protocol-alist '("^file:" . org-ref-pdf-dnd-protocol))


;;;###autoload
(defun org-ref-pdf-dir-to-bibtex (bibfile directory)
  "Create BIBFILE from pdf files in DIRECTORY."
  (interactive (list
		(read-file-name "Bibtex file: ")
		(read-directory-name "Directory: ")))
  (find-file bibfile)
  (goto-char (point-max))

  (cl-loop for pdf in (f-entries directory (lambda (f) (f-ext? f "pdf")))
	   do
	   (goto-char (point-max)) 
	   (let ((dois (org-ref-extract-doi-from-pdf pdf)))
	     (cond
	      ((null dois)
	       (insert (format "%% No doi found to create entry in %s.\n" pdf)))
	      ((= 1 (length dois))
	       (doi-utils-add-bibtex-entry-from-doi
		(car dois)
		(buffer-file-name))
	       (bibtex-beginning-of-entry) 
	       (insert (format "%% [[file:%s]]\n" pdf)))
	      ;; Multiple DOIs found
	      (t 
	       (insert (format "%% Multiple dois found in %s\n" pdf))
	       (helm :sources `((name . "Select a DOI")
				(candidates . ,(org-ref-pdf-doi-candidates dois))
				(action . org-ref-pdf-add-dois))))))))


;;;###autoload
(defun org-ref-pdf-debug-pdf (pdf-file)
  "Try to debug getting a doi from a pdf.
Opens a buffer with the pdf converted to text, and `occur' on the
variable `org-ref-pdf-doi-regex'."
  (interactive "fPDF: ")
  (switch-to-buffer (get-buffer-create "*org-ref-pdf debug*"))
  (erase-buffer)
  (insert (shell-command-to-string (format "%s %s -"
					   pdftotext-executable
					   (shell-quote-argument pdf-file))))
  (goto-char (point-min))
  (highlight-regexp org-ref-pdf-doi-regex)
  (occur org-ref-pdf-doi-regex)
  (switch-to-buffer-other-window "*Occur*"))


;;;###autoload
(defun org-ref-pdf-crossref-lookup ()
  "Lookup highlighted text in PDFView in CrossRef."
  (interactive)
  (pdf-view-assert-active-region)
  (let* ((txt (pdf-view-active-region-text)))
    (pdf-view-deactivate-region)
    (crossref-lookup (mapconcat 'identity txt "	 \n"))))


(provide 'org-ref-pdf)
;;; org-ref-pdf.el ends here
