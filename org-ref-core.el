;;; org-ref-core.el --- citations, cross-references and bibliographies in org-mode -*- lexical-binding: t; -*-

;; Copyright(C) 2014-2024 John Kitchin

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
;; This is the core library.
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'org)
;; the eval and compile seems especially necessary for native compilation on the newest Emacs.
(eval-and-compile (require 'org-macs))

(require 'org-element)

(require 'dash)
(require 'f)
(require 's)

(require 'parsebib)
(require 'bibtex-completion)
(require 'transient)

(require 'org-ref-bibliography-links)
(require 'org-ref-citation-links)
(require 'org-ref-ref-links)
(require 'org-ref-label-link)
(require 'org-ref-misc-links)
(require 'org-ref-export)

(require 'org-ref-utils)
(require 'org-ref-bibtex)
(require 'org-ref-glossary)
(require 'openalex)


;;* Custom variables

(defgroup org-ref nil
  "Customization group for org-ref."
  :tag "Org Ref"
  :group 'org)


(defcustom org-ref-insert-link-function
  nil
  "Generic function for inserting org-ref links.
The function should take a prefix arg.
No arg means insert a cite link
1 arg means insert a ref link
2 args means insert a label."
  :type 'function
  :group 'org-ref)


(defcustom org-ref-insert-cite-function
  nil
  "Function to call to insert citation links.
This function should prompt for keys with completion, and insert
the citation link into the buffer."
  :type 'function
  :group 'org-ref)


(defcustom org-ref-cite-completion-function
  nil
  "Function to prompt for keys with completion."
  :type '(choice (const nil)
                 (function))
  :group 'org-ref)


(defcustom org-ref-insert-label-function
  nil
  "Function to call to insert label links.
This function should prompt for a label, and insert the label
link."
  :type 'function
  :group 'org-ref)


(defcustom org-ref-insert-ref-function
  nil
  "Function to call to insert ref links.
This function should prompt for a label with completion, and
insert the ref link."
  :type 'function
  :group 'org-ref)


(defcustom org-ref-cite-onclick-function
  nil
  "Function that runs when you click on a cite link.
The function must take one argument which is the path of the link
that was clicked on."
  :type 'function
  :group 'org-ref)


(defvar org-ref-prefix-arg nil
  "Variable to store a prefix arg during completion.")


(defun org-ref-minibuffer-prefix ()
  "Hook function for `minibuffer-setup-hook'.
The idea is to locally bind C-u to a function that captures
prefix args in `org-ref-prefix-arg' so you can use them later."
  (setq org-ref-prefix-arg nil)
  (local-set-key (kbd "C-u") (lambda ()
			       (interactive)
			       (setq org-ref-prefix-arg
				     (if (null org-ref-prefix-arg)
					 '(4)
				       (list (* 4 (car org-ref-prefix-arg))))))))


;; * Bibliography related functions

(defun org-ref-find-bibliography ()
  "Find the bibliography in the buffer.
This function sets and returns a list of files either from internal bibliographies, from files in the
BIBINPUTS env var, and finally falling back to what the user has
set in `bibtex-completion-bibliography'"
  (let ((org-ref-bibliography-files ()))
    (catch 'result
      (save-excursion
	(when (eq major-mode 'org-mode)
	  (org-with-wide-buffer
           (goto-char (point-min))
	   ;; This just searches for these strings, and then checks if it
	   ;; is on a link. This is faster than parsing the org-file when
	   ;; it gets large.
           ;; look for org-ref bibliography
           (while (re-search-forward "\\(no\\)?bibliography:" nil t)
	     (let ((link (org-element-context)))
	       (when (and (eq (car link) 'link)
			  (member (org-element-property :type link) '("bibliography" "nobibliography")))
		 (setq org-ref-bibliography-files
		       (mapcar 'org-ref-get-bibfile-path
			       (mapcar 'string-trim (split-string
						     (org-element-property :path link)
						     ","))))
		 (throw 'result (nreverse (delete-dups org-ref-bibliography-files))))))

           (goto-char (point-min))
           (while (re-search-forward "\\\\addbibresource{\\(.*\\)}" nil t)
             (push (match-string 1) org-ref-bibliography-files))

           (when org-ref-bibliography-files
             (throw 'result (nreverse (delete-dups (mapcar 'org-ref-get-bibfile-path org-ref-bibliography-files)))))))
	;; we did not find anything. use defaults. Make sure we have a list in
        ;; case it is a single string. 
	(throw 'result (if (listp bibtex-completion-bibliography)
			   bibtex-completion-bibliography
			 (list bibtex-completion-bibliography)))))))


(defun org-ref-key-in-file-p (key filename)
  "Determine if the KEY is in the FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (hack-local-variables)
    (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
    (bibtex-search-entry key)))


(defun org-ref-possible-bibfiles ()
  "Make a unique list of possible bibliography files for completing-read"
  (-uniq
   (append
    ;; see if we should add it to a bib-file defined in the file
    (org-ref-find-bibliography)
    ;; or any bib-files that exist in the current directory
    (f-entries "." (lambda (f)
		     (and (not (string-match "#" f))
			  (f-ext? f "bib"))))
    ;; and last in the default bibliography
    (if (stringp bibtex-completion-bibliography)
	(list bibtex-completion-bibliography)
      bibtex-completion-bibliography))))


(defun org-ref-get-bibtex-key-and-file (&optional key)
  "Return a  a cons cell of (KEY . file) that KEY is in.
If no key is provided, get one under point."
  (unless key
    (setq key (org-ref-get-bibtex-key-under-cursor)))

  (cons key (save-window-excursion
	      (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
		(bibtex-completion-show-entry (list key))
		(buffer-file-name)))))


;; * Insert link functions

;;;###autoload
(defun org-ref-insert-link (arg)
  "Insert an org-ref link.
If no prefix ARG insert a cite.
If one prefix ARG insert a ref.
If two prefix ARGs insert a label.

This is a generic function. Specific backends might
provide their own version."
  (interactive "P")
  (cond
   ((eq arg nil)
    (funcall org-ref-insert-cite-function))
   ((equal arg '(4))
    (funcall org-ref-insert-ref-function))
   ((equal arg '(16))
    (funcall org-ref-insert-label-function))))


;; This is an alternative that doesn't rely on prefix args.
(defun org-ref-insert-link-menu--insert-citation ()
  (interactive)
  (funcall org-ref-insert-cite-function))

(defun org-ref-insert-link-menu--insert-reference ()
  (interactive)
  (funcall org-ref-insert-ref-function))

(defun org-ref-insert-link-menu--insert-label ()
  (interactive)
  (funcall org-ref-insert-label-function))

(defun org-ref-insert-link-menu--open-bibliography ()
  (interactive)
  (find-file (completing-read "Bibliography: " (org-ref-find-bibliography))))

(defun org-ref-insert-link-menu--ensure-table (name template)
  (org-mark-ring-push)
  (goto-char (point-min))
  (if (re-search-forward (format "#\\+name: %s" name) nil t)
      (progn
        (goto-char (org-element-property :contents-end (org-element-context)))
        (backward-char)
        (org-table-insert-row '(4)))
    (goto-char (point-max))
    (insert template)
    (beginning-of-line)
    (forward-char)))

(defun org-ref-insert-link-menu--new-glossary-term ()
  (interactive)
  (org-ref-insert-link-menu--ensure-table
   "glossary"
   "\n\n#+name: glossary
| label | term    | definition                    |
|-------+---------+-------------------------------|
|       |         |                               |"))

(defun org-ref-insert-link-menu--new-acronym-term ()
  (interactive)
  (org-ref-insert-link-menu--ensure-table
   "acronym"
   "\n\n#+name: acronyms
| label | abbreviation | full form                  |
|-------+--------------+----------------------------|
|       |              |                            |"))

(defun org-ref-insert-link-menu--insert-string (string)
  (insert string))

(defun org-ref-insert-link-menu--insert-index (prompt template)
  (org-ref-insert-link-menu--insert-string
   (format template (string-trim (read-string prompt)))))

(transient-define-prefix org-ref-insert-link-menu ()
  "Insert an org-ref link."
  [["org-ref"
    ("]" "Citation" org-ref-insert-link-menu--insert-citation :transient t)
    ("r" "Cross-reference" org-ref-insert-link-menu--insert-reference :transient t)
    ("\\" "Label" org-ref-insert-link-menu--insert-label :transient t)]
   ["Bibliography"
    ("bs" "Bibliographystyle" (lambda () (interactive)
            (org-ref-insert-link-menu--insert-string
             (org-ref-bibliographystyle-complete-link))))
    ("bf" "Bibliography" (lambda () (interactive)
            (org-ref-insert-link-menu--insert-string
             (org-ref-bibliography-complete))))
    ("nb" "Nobibliography" (lambda () (interactive)
            (org-ref-insert-link-menu--insert-string
             (org-ref-nobibliography-complete))))]
   ["Glossary"
    ("g" "Glossary link" org-ref-insert-glossary-link)
    ("a" "Acronym link" org-ref-insert-acronym-link)
    ("ng" "New glossary term" org-ref-insert-link-menu--new-glossary-term :transient t)
    ("na" "New acronym term" org-ref-insert-link-menu--new-acronym-term :transient t)]
   ["Bibtex"
    ("bd" "Add bibtex entry from a DOI" doi-add-bibtex-entry :transient t)
    ("bc" "Add bibtex entry from Crossref" crossref-add-bibtex-entry :transient t)
    ("bo" "Open bibtex file" org-ref-insert-link-menu--open-bibliography :transient t)]
   ["Misc"
    ("t" "List of tables" (lambda () (interactive)
           (org-ref-insert-link-menu--insert-string "[[list-of-tables:]]\n"))
     :transient t)
    ("f" "List of figures" (lambda () (interactive)
           (org-ref-insert-link-menu--insert-string "[[list-of-figures:]]\n"))
     :transient t)
    ("i" "Index entry" (lambda () (interactive)
           (org-ref-insert-link-menu--insert-index
            "Index entry: " "[[index:%s]]"))
     :transient t)
    ("pi" "Print index" (lambda () (interactive)
            (org-ref-insert-link-menu--insert-string "[[printindex:]]"))
     :transient t)
    ("pg" "Print glossary" (lambda () (interactive)
            (org-ref-insert-link-menu--insert-string "[[printglossaries:]]"))
     :transient t)
    ("q" "Quit" transient-quit-one)]])

(define-obsolete-function-alias 'org-ref-insert-link-hydra/body
  #'org-ref-insert-link-menu "3.1")


;;* org-ref-help
;;;###autoload
(defun org-ref-help ()
  "Open the `org-ref' manual."
  (interactive)
  (find-file (expand-file-name
              "org-ref.org"
              (file-name-directory
               (find-library-name "org-ref")))))


;;* org-ref menu

(defun org-ref-org-menu ()
  "Add `org-ref' menu to the Org menu."

  (easy-menu-change
   '("Org") "org-ref"
   `(["Insert citation" ,org-ref-insert-cite-function]
     ["Insert ref" ,org-ref-insert-ref-function]
     ["Insert label" ,org-ref-insert-label-function]
     "--"
     ["List of figures" org-ref-list-of-figures]
     ["List of tables" org-ref-list-of-tables]
     ["Extract bibtex entries" org-ref-extract-bibtex-entries]
     ["Check org-file" org-ref]
     "--"
     ["Help" org-ref-help]
     ["Customize org-ref" (customize-group 'org-ref)])
   "Show/Hide")

  (easy-menu-change '("Org") "--" nil "Show/Hide"))

(add-hook 'org-mode-hook 'org-ref-org-menu)



;;* The end
(provide 'org-ref-core)

;;; org-ref-core.el ends here

;; Local Variables:
;; byte-compile-warnings: (not docstrings docstrings-wide)
;; End:
