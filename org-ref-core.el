;;; org-ref-core.el --- citations, cross-references and bibliographies in org-mode

;; Copyright(C) 2014-2017 John Kitchin

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
;; Lisp code to setup bibliography, cite, ref and label org-mode links. The
;; links are clickable and do things that are useful. You should really read
;; org-ref.org in this package for details.
;;

;;; Code:

;; This suppresses showing the warning buffer. bibtex-completion seems to make this
;; pop up in an irritating way.
(unless (boundp 'warning-suppress-types)
  (require 'warnings))


(add-to-list 'warning-suppress-types '(:warning))


(eval-when-compile
  (require 'cl-lib))
(require 'dash)
(require 'f)
(require 'htmlize)
(require 's)
(require 'doi-utils)
(require 'seq)


(require 'org-ref-bibtex)
(require 'org-ref-utils)
(require 'org-ref-glossary)
(require 'org)
(require 'org-element)
(require 'ox)
(require 'parsebib)
(require 'bibtex-completion)

(defvar org-export-exclude-tags)
(defvar warning-suppress-types)
(declare-function bibtex-completion-get-entry "bibtex-completion")
(declare-function bibtex-completion-edit-notes "bibtex-completion")


;;* Custom variables

(defgroup org-ref nil
  "Customization group for org-ref."
  :tag "Org Ref"
  :group 'org)


(defcustom org-ref-completion-library
  'org-ref-ivy
  "Symbol for library to define completion functions.
The completion library should provide functions for
`org-ref-insert-link-function', `org-ref-insert-cite-function',
`org-ref-insert-label-function', `org-ref-insert-ref-function',
and `org-ref-cite-onclick-function', and set those variables to
the values of those functions."
  :type 'symbol
  :options '(org-ref-ivy)
  :group 'org-ref)


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
that was clicked on. This function is normally set by the
function in `org-ref-completion-library'."
  :type 'function
  :group 'org-ref)


;; TODO remove this, and a lot of related code I think
(defvar org-ref-bibliography-files
  nil
  "Variable to hold bibliography files to be searched.")



;;* font lock for org-ref


;; TODO : this could be a lot cleaner I think
(defun org-ref-find-bibliography ()
  "Find the bibliography in the buffer.
This function sets and returns a list of files either from internal bibliographies, from files in the
BIBINPUTS env var, and finally falling back to what the user has
set in `bibtex-completion-bibliography'"
  (catch 'result
    ;; If you call this in a bibtex file, assume we want this file
    (when (and buffer-file-name (f-ext? buffer-file-name "bib"))
      (throw 'result (setq org-ref-bibliography-files (list buffer-file-name))))

    ;; otherwise, check current file for a bibliography source
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
	(setq org-ref-bibliography-files ())

        ;; look for org-ref bibliography or addbibresource links
        (while (re-search-forward
                ;; This just searches for these strings, and then checks if it
                ;; is on a link. This is faster than parsing the org-file when
                ;; it gets large.
                "\\(bibliography\\|addbibresource\\):"
                nil t)
	  (let ((link (org-element-context)))
	    (when (and (eq (car link) 'link)
		       (or
			(string= (org-element-property :type link) "bibliography")
			(string= (org-element-property :type link) "addbibresource")))
	      (dolist (bibfile (org-ref-split-and-strip-string
				(org-element-property :path link)))
		(let ((bibf (org-ref-find-bibfile bibfile)))
		  (when bibf
		    (push bibf org-ref-bibliography-files)))))))

        (when org-ref-bibliography-files
          (throw 'result
                 (setq org-ref-bibliography-files
                       (nreverse (delete-dups org-ref-bibliography-files)))))

        ;; Try addbibresource as a latex command. It appears that reftex does
        ;; not do this correctly, it only finds the first one but there could be
        ;; many.
        (goto-char (point-min))
        (while (re-search-forward
                "\\\\addbibresource{\\(.*\\)}"
                nil t)
          (push (match-string 1) org-ref-bibliography-files))

        (when org-ref-bibliography-files
          (throw 'result (setq org-ref-bibliography-files
                               (nreverse org-ref-bibliography-files))))

        ;; we did not find org-ref links. now look for latex links
        (goto-char (point-min))
        (when org-ref-bibliography-files
          (throw 'result org-ref-bibliography-files)))


      ;; we did not find anything. use defaults
      (setq org-ref-bibliography-files bibtex-completion-bibliography)))

  org-ref-bibliography-files)


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
    bibtex-completion-bibliography)))


(defun org-ref-get-bibtex-key-and-file (&optional key)
  "Return a  a cons cell of (KEY . file) that KEY is in.
If no key is provided, get one under point."
  (let ((org-ref-bibliography-files (org-ref-find-bibliography))
        (file))
    (unless key
      (setq key (org-ref-get-bibtex-key-under-cursor)))
    (setq file (cl-loop for bib-file in org-ref-bibliography-files
			when (let* ((key "schuett-2018-schnet")
				    (buf (or (get-file-buffer bib-file)
					     (find-buffer-visiting bib-file)))
				    (found (with-current-buffer (find-file-noselect bib-file)
					     (widen)
					     (goto-char (point-min))
					     (prog1
						 (re-search-forward
						  (concat "^@\\(" parsebib--bibtex-identifier
							  "\\)[[:space:]]*[\(\{][[:space:]]*"
							  (regexp-quote key) "[[:space:]]*,")
						  nil t)
					       (unless buf
						 (kill-buffer))))))
			       found)
			return bib-file))
    (cons key (when (stringp file) (substring-no-properties file)))))


;;;###autoload
(defun org-ref-insert-link (arg)
  "Insert an org-ref link.
If no prefix ARG insert a cite.
If one prefix ARG insert a ref.
If two prefix ARGs insert a label.

This is a generic function. Specific completion engines might
provide their own version."
  (interactive "P")
  (cond
   ((eq arg nil)
    (funcall org-ref-insert-cite-function))
   ((equal arg '(4))
    (funcall org-ref-insert-ref-function))
   ((equal arg '(16))
    (funcall org-ref-insert-label-function))))


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
     ["Change completion backend" org-ref-change-completion]
     "--"
     ["Help" org-ref-help]
     ["Customize org-ref" (customize-group 'org-ref)])
   "Show/Hide")

  (easy-menu-change '("Org") "--" nil "Show/Hide"))

(add-hook 'org-mode-hook 'org-ref-org-menu)



;;* The end
(provide 'org-ref-core)

;;; org-ref-core.el ends here
