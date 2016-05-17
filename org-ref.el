;;; org-ref.el --- citations, cross-references and bibliographies in org-mode

;; Copyright(C) 2014,2015 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; URL: https://github.com/jkitchin/org-ref
;; Version: 0.8.1
;; Keywords: org-mode, cite, ref, label
;; Package-Requires: ((dash "2.11.0") (parsebib "1.0") (hydra "0.13.2") (key-chord "0") (s "1.10.0") (f "0.18.0") (emacs "24.4"))

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
;; Lisp code to setup bibliography, cite, ref and label org-mode links.
;; Also sets up reftex and helm for org-mode citations.  The links are
;; clickable and do things that are useful.  You should really read
;; org-ref.org in this package for details.
;;

;;; Code:
(eval-when-compile
  (require 'cl))
(require 'dash)
(require 'f)
(require 's)
(require 'doi-utils)
(require 'org-ref-bibtex)
(require 'org-ref-utils)
(require 'org-ref-glossary)
(require 'org)
(require 'org-element)
(require 'ox)
(require 'parsebib)
(require 'reftex-cite)

;;for byte-compile error avoidance
(defvar-local org-export-exclude-tags nil)
(declare-function 'org-ref-email-bibtex-entry "org-ref-bibtex.el")

;;* Custom variables
(defgroup org-ref nil
  "Customization group for org-ref."
  :tag "Org Ref"
  :group 'org)


(defcustom org-ref-bibliography-notes
  nil
  "Filename where you will put all your notes about an entry in the default bibliography.
Used by backends that append all notes as entries in a single file."
  :type 'file
  :group 'org-ref)


(defcustom org-ref-notes-directory
  nil
  "Directory where you will put all your notes about an entry in the default bibliography.
Used for backends that create a single file of notes per entry."
  :type 'directory
  :group 'org-ref)


(defcustom org-ref-default-bibliography
  nil
  "List of bibtex files to search for.
You should use full-paths for each file."
  :type '(repeat :tag "List of bibtex files" file)
  :group 'org-ref)


(defcustom org-ref-pdf-directory
  nil
  "Directory where pdfs are stored by key.
Put a trailing / in the name."
  :type 'directory
  :group 'org-ref)


(defcustom org-ref-default-citation-link
  "cite"
  "The default type of citation link to use."
  :type 'string
  :group 'org-ref)


(defcustom org-ref-insert-cite-key
  "C-c ]"
  "Keyboard shortcut to insert a citation."
  :type 'string
  :group 'org-ref)


(defcustom org-ref-completion-library
  'org-ref-helm-bibtex
  "String for library to define completion functions.
The completion library should provide functions for
`org-ref-insert-link-function', `org-ref-insert-cite-function',
`org-ref-insert-label-function', `org-ref-insert-ref-function',
and `org-ref-cite-onclick-function', and set those variables to
the values of those functions."
  :type 'symbol
  :options '(org-ref-helm-bibtex	; completion with helm + helm-bibtex
	     org-ref-helm-cite		; completion with helm in org-ref
	     org-ref-ivy-bibtex		; completion with ivy
	     org-ref-reftex		; org-completion
	     )
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
  "Function to prompt for keys with completion.")


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

;; now load the completion library.
;; (message-box "Requiring %s" org-ref-completion-library)
;; (load-file (format "%s.el" ))
(require org-ref-completion-library)

;; define key for inserting citations
(define-key org-mode-map
  (kbd org-ref-insert-cite-key)
  org-ref-insert-link-function)


(defcustom org-ref-bibliography-entry-format
  '(("article" . "%a, %t, <i>%j</i>, <b>%v(%n)</b>, %p (%y). <a href=\"%U\">link</a>. <a href=\"http://dx.doi.org/%D\">doi</a>.")

    ("book" . "%a, %t, %u (%y).")
    ("techreport" . "%a, %t, %i, %u (%y).")
    ("proceedings" . "%e, %t in %S, %u (%y).")
    ("inproceedings" . "%a, %t, %p, in %b, edited by %e, %u (%y)"))
  "String to format an entry.
Just the reference, no numbering at the beginning, etc... see the
`org-ref-reftex-format-citation' docstring for the escape codes."
  :type 'string
  :group 'org-ref)


(defcustom org-ref-note-title-format
  "** TODO %y - %t
 :PROPERTIES:
  :Custom_ID: %k
  :AUTHOR: %9a
  :JOURNAL: %j
  :YEAR: %y
  :VOLUME: %v
  :PAGES: %p
  :DOI: %D
  :URL: %U
 :END:
"
  "String to format the title and properties drawer of a note.
See the `org-ref-reftex-format-citation' docstring for the escape
codes."
  :type 'string
  :group 'org-ref)


(defcustom org-ref-notes-function
  (lambda (thekey)
    (let* ((results (org-ref-get-bibtex-key-and-file thekey))
           (key (car results))
           (bibfile (cdr results)))

      (save-excursion
        (with-temp-buffer
          (insert-file-contents bibfile)
          (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
          (bibtex-search-entry key)
          (org-ref-open-bibtex-notes)))))
  "Function to open the notes for the bibtex key in a cite link at point.

The default behavior adds entries to a long file with headlines
for each entry.  It also tries to be compatible with `org-bibtex'.

An alternative is
 (lambda ()
  (bibtex-completion-edit-notes (car (org-ref-get-bibtex-key-and-file thekey))))

Use that if you like the one file one note approach of `helm-bibtex'."
  :type 'function
  :group 'org-ref)


(defcustom org-ref-open-notes-function
  (lambda ()
    (org-show-entry)
    (show-branches)
    (show-children)
    (org-cycle '(64))
    (recenter-top-bottom 0))
  "User-defined way to open a notes entry.
This is executed after the entry is found in
`org-ref-open-bibtex-notes', with the cursor at the beginning of
the headline. The default setting fully expands the notes, and
moves the headline to the top of the buffer."
  :type 'function
  :group 'org-ref)


(defcustom org-ref-open-pdf-function
  'org-ref-open-pdf-at-point
  "User-defined function to open a pdf from a link.
The function must get the key at point, and derive a path to the pdf
file, then open it.  The default function is
`org-ref-open-pdf-at-point'."
  :type 'function
  :group 'org-ref)


(defcustom org-ref-get-pdf-filename-function
  'org-ref-get-pdf-filename
  "User-defined function to get a filename from a bibtex key.
The function must take a key as an argument, and return the path
to the corresponding filename.  The default is
`org-ref-get-pdf-filename'.  An alternative value is
`org-ref-get-mendeley-filename'."
  :type 'function
  :group 'org-ref)


(defcustom org-ref-clean-bibtex-key-function
  (lambda (key)
    (replace-regexp-in-string ":" "" key))
  "Function to modify a bibtex key.
The default behavior is to remove : from the key."
  :type 'function
  :group 'org-ref)


(defcustom org-ref-show-citation-on-enter t
  "If non-nil show the citation summary.
Uses a hook function to display the message in the minibuffer."
  :type 'boolean
  :group 'org-ref)


(defcustom org-ref-cite-types
  '("cite" "nocite" ;; the default latex cite commands
    ;; natbib cite commands, http://ctan.unixbrain.com/macros/latex/contrib/natbib/natnotes.pdf
    "citet" "citet*" "citep" "citep*"
    "citealt" "citealt*" "citealp" "citealp*"
    "citenum" "citetext"
    "citeauthor" "citeauthor*"
    "citeyear" "citeyear*"
    "Citet" "Citep" "Citealt" "Citealp" "Citeauthor"
    ;; biblatex commands
    ;; http://ctan.mirrorcatalogs.com/macros/latex/contrib/biblatex/doc/biblatex.pdf
    "Cite"
    "parencite" "Parencite"
    "footcite" "footcitetext"
    "textcite" "Textcite"
    "smartcite" "Smartcite"
    "cite*" "parencite*" "supercite"
    "autocite" "Autocite" "autocite*" "Autocite*"
    "Citeauthor*"
    "citetitle" "citetitle*"
    "citedate" "citedate*"
    "citeurl"
    "fullcite" "footfullcite"
    ;; "volcite" "Volcite" cannot support the syntax
    "notecite" "Notecite"
    "pnotecite" "Pnotecite"
    "fnotecite"
    ;; multicites. Very limited support for these.
    "cites" "Cites" "parencites" "Parencites"
    "footcites" "footcitetexts"
    "smartcites" "Smartcites" "textcites" "Textcites"
    "supercites" "autocites" "Autocites"
    ;; for the bibentry package
    "bibentry"
    )
  "List of citation types known in `org-ref'."
  :type '(repeat :tag "List of citation types" string)
  :group 'org-ref)


(defcustom org-ref-clean-bibtex-entry-hook
  '(orcb-key-comma
    org-ref-replace-nonascii
    orcb-&
    orcb-%
    org-ref-title-case-article
    orcb-clean-year
    orcb-key
    orcb-clean-doi
    orcb-clean-pages
    org-ref-sort-bibtex-entry)
  "Hook that is run in `org-ref-clean-bibtex-entry'.
The functions should have no arguments, and
operate on the bibtex entry at point. You can assume point starts
at the beginning of the entry. These functions are wrapped in
`save-restriction' and `save-excursion' so you do not need to
save the point position."
  :group 'org-ref
  :type 'hook)


(defcustom org-ref-bibtex-sort-order
  '(("article"  . ("author" "title" "journal" "volume" "number" "pages" "year" "doi" "url"))
    ("inproceedings" . ("author" "title" "booktitle" "year" "volume" "number" "pages" "doi" "url"))
    ("book" . ("author" "title" "year" "publisher" "url")))
  "A-list of bibtex entry fields and the order to sort an entry with.
\(entry-type . (list of fields). This is used in
`org-ref-sort-bibtex-entry'. Entry types not listed here will
have fields sorted alphabetically."
  :type '(alist :key-type 'string :value-type 'list)
  :group 'org-ref)


(defvar org-ref-bibliography-files
  nil
  "Variable to hold bibliography files to be searched.")

;;* Messages for link at cursor
(defvar org-ref-message-timer nil
  "Variable to store the link message timer in.")


;;;###autoload
(defun org-ref-show-link-messages ()
  "Turn on link messages.
You will see a message in the minibuffer when on a cite, ref or
label link."
  (interactive)
  (or org-ref-message-timer
      (setq org-ref-message-timer
            (run-with-idle-timer 0.5 t 'org-ref-link-message))))


;;;###autoload
(defun org-ref-cancel-link-messages ()
  "Stop showing messages in minibuffer when on a link."
  (interactive)
  (cancel-timer org-ref-message-timer)
  (setq org-ref-message-timer nil))


(when org-ref-show-citation-on-enter
  (org-ref-show-link-messages))


;;;###autoload
(defun org-ref-change-completion ()
  "Change the completion backend.
Options are \"org-ref-helm-bibtex\", \"org-ref-helm-cite\",
\"org-ref-ivy-bibtex\" and \"org-ref-reftex\"."
  (interactive)
  (require
   (intern
    (completing-read "Backend: " '("org-ref-helm-bibtex"
				   "org-ref-helm-cite"
				   "org-ref-ivy-bibtex"
				   "org-ref-reftex")
		     nil
		     t
		     "org-ref-helm-cite"))))

;;** Messages for context under mouse pointer

(defvar org-ref-last-mouse-pos nil
  "Stores last mouse position for use in `org-ref-mouse-message'.")


(defun org-ref-can-move-p ()
  "See if a character is under the mouse.
If so return the position for `goto-char'."
  (let* ((line (cddr org-ref-last-mouse-pos))
         (col  (cadr org-ref-last-mouse-pos)))
    (save-excursion
      (goto-char (window-start))
      (forward-line line)
      (if
          (> (- (line-end-position) (line-beginning-position)) col)
          (progn  (forward-char col) (point))
        nil))))


;;;###autoload
(defun org-ref-mouse-message ()
  "Display message for link under mouse cursor."
  (interactive)
  (when (not (equal (mouse-position) org-ref-last-mouse-pos))
    (setq org-ref-last-mouse-pos (mouse-position))
    (let ((p (org-ref-can-move-p)))
      (when p
        (save-excursion
          (goto-char p)
          (org-ref-link-message))))))


(defvar org-ref-message-timer-mouse nil
  "Store mouse timer.")


(defvar org-ref-mouse-message-interval 0.5
  "How often to run the mouse message timer in seconds.")


;;;###autoload
(defun org-ref-mouse-messages-on ()
  "Turn on mouse messages."
  (interactive)
  (or org-ref-message-timer-mouse
      (setq org-ref-message-timer-mouse
            (run-at-time "0.5 sec"
                         org-ref-mouse-message-interval
                         'org-ref-mouse-message))))


;;;###autoload
(defun org-ref-mouse-messages-off ()
  "Turn off mouse messages."
  (interactive)
  (cancel-timer org-ref-message-timer-mouse)
  (setq org-ref-message-timer-mouse nil)
  (message "Mouse messages are off"))



;;* font lock for org-ref

(defcustom org-ref-colorize-links
  t
  "When non-nil, change colors of links."
  :type 'boolean
  :group 'org-ref)


(defcustom org-ref-cite-color
  "forest green"
  "Color of cite like links."
  :type 'string
  :group 'org-ref)


(defcustom org-ref-ref-color
  "dark red"
  "Color of ref like links."
  :type 'string
  :group 'org-ref)


(defcustom org-ref-label-color
  "dark magenta"
  "Color of label links."
  :type 'string
  :group 'org-ref)


(defvar org-ref-cite-re
  (concat "\\(" (regexp-opt org-ref-cite-types) ":\\)"
          "\\(\\([a-zA-Z0-9-_:\\./]+,?\\)+\\)")
  "Regexp for cite links.")


(defvar org-ref-label-re
  "label:\\([a-zA-Z0-9-_:]+,?\\)+"
  "Regexp for label links.")


(defvar org-ref-ref-re
  "\\(eq\\)?ref:\\([a-zA-Z0-9-_:]+,?\\)+"
  "Regexp for ref links.")


(defface org-ref-cite-face
  `((t (:inherit org-link
                 :foreground ,org-ref-cite-color)))
  "Color for cite-like links in org-ref.")


(defface org-ref-label-face
  `((t (:inherit org-link :foreground ,org-ref-label-color)))
  "Color for ref links in org-ref.")


(defface org-ref-ref-face
  `((t (:inherit org-link :foreground ,org-ref-ref-color)))
  "Face for ref links in org-ref.")


;;** Font-lock org-ref links

;; We use functions to search for the next link, and then use org-mode to find
;; the boundaries. I wasn't able to figure out robust regexps for these links
;; that includes all possible link syntaxes eg. bare, [[cite:key]] and
;; [[cite:key] [text]]. Using regexps might be a bit more efficient, so if they
;; ever get figured out, we could eliminate the org-element code in these
;; functions.

(defun org-ref-match-next-cite-link (&optional limit)
  "Search forward to next cite link up to LIMIT
Add a tooltip to the match."
  (when (and (re-search-forward org-ref-cite-re limit t)
	     ;; make sure we are not in a comment
	     (save-excursion
	       (beginning-of-line)
	       (not (looking-at "# "))))
    ;; we think we are on a cite link lets get on it and make sure
    (forward-char -2)
    (let ((this-link (org-element-context)))
      (if (-contains? org-ref-cite-types (org-element-property :type this-link))
	  ;; we are on a cite link
	  (progn
	    (add-text-properties
	     (org-element-property :begin this-link)
	     (- (org-element-property :end this-link)
		(org-element-property :post-blank this-link))
	     (list
	      'help-echo (lambda (window object position)
			   (save-excursion
			     (goto-char position)
			     ;; Here we wrap the citation string to a reasonable size.
			     (let ((s (org-ref-get-citation-string-at-point)))
			       (with-temp-buffer
				 (insert s)
				 (fill-paragraph)
				 (buffer-string)))))))
	    (set-match-data
	     (list (org-element-property :begin this-link)
		   (- (org-element-property :end this-link)
		      (org-element-property :post-blank this-link))))
	    (goto-char (org-element-property :end this-link)))
	;; Must be a false match.
	;; somehow were not on a
	;; cite link, so we try
	;; again.
	(org-ref-match-next-cite-link limit)))))


(defun org-ref-match-next-label-link (limit)
  "Find next label link up to LIMIT.
Add tooltip."
  (if (and (re-search-forward "label:\\([[:alnum:]]\\)\\{2,\\}" limit t)
	   ;; make sure we are not in a comment
	   (save-excursion
	     (beginning-of-line)
	     (not (looking-at "# "))))
      (progn
	(forward-char -2)
	(let ((this-link (org-element-context)))
	  (if (string= "label" (org-element-property :type this-link))
	      ;; on a label
	      (progn
		(add-text-properties
		 (org-element-property :begin this-link)
		 (- (org-element-property :end this-link)
		    (org-element-property :post-blank this-link))
		 (list
		  'help-echo (lambda (window object position)
			       (save-excursion
				 (goto-char position)
				 (let ((s (org-ref-link-message)))
				   (with-temp-buffer
				     (insert s)
				     (fill-paragraph)
				     (buffer-string)))))))
		(set-match-data
		 (list (org-element-property :begin this-link)
		       (- (org-element-property :end this-link)
			  (org-element-property :post-blank this-link))))
		(goto-char (org-element-property :end this-link)))
	    ;; false match
	    (org-ref-match-next-label-link limit))))))


(defun org-ref-match-next-ref-link (limit)
  "Find next ref link up to LIMIT.
Add tooltip to the link. We avoid tags by not finding :ref: in
tags."
  (when (and (re-search-forward "[^:]ref:\\([[:alnum:]]\\)\\{2,\\}" limit t)
	     ;; make sure we are not on a comment
	     (save-excursion
	       (beginning-of-line)
	       (not (looking-at "# "))))
    ;; we think we are on a ref link, lets make sure.
    (forward-char -2)
    (let ((this-link (org-element-context)))
      (if (string= "ref" (org-element-property :type this-link))
	  ;; we are, so we do our business
	  (progn
	    (add-text-properties
	     (org-element-property :begin this-link)
	     (- (org-element-property :end this-link)
		(org-element-property :post-blank this-link))
	     (list
	      'help-echo (lambda (window object position)
			   (save-excursion
			     (goto-char position)
			     (let ((s (org-ref-link-message)))
			       (with-temp-buffer
				 (insert s)
				 (fill-paragraph)
				 (buffer-string)))))))
	    (set-match-data
	     (list (org-element-property :begin this-link)
		   (- (org-element-property :end this-link)
		      (org-element-property :post-blank this-link))))
	    (goto-char (org-element-property :end this-link)))
	;; False match, let's try again
	(org-ref-match-next-ref-link limit)))))


(defun org-ref-match-next-bibliography-link (limit)
  "Find next bibliography link up to LIMIT.
Add tooltip to the link."
  (when (re-search-forward "bibliography:\\([[:alnum:]]\\)\\{2,\\}" limit t)
    (forward-char -2)
    (let ((this-link (org-element-context)))
      (add-text-properties
       (org-element-property :begin this-link)
       (- (org-element-property :end this-link)
	  (org-element-property :post-blank this-link))
       (list
	'help-echo (lambda (window object position)
		     (save-excursion
		       (goto-char position)
		       (let ((s (org-ref-link-message)))
			 (with-temp-buffer
			   (insert s)
			   (fill-paragraph)
			   (buffer-string)))))))
      (set-match-data
       (list (org-element-property :begin this-link)
	     (- (org-element-property :end this-link)
		(org-element-property :post-blank this-link))))
      (goto-char (org-element-property :end this-link)))))


(defun org-ref-match-next-bibliographystyle-link (limit)
  "Find next bibliographystyle link up to LIMIT.
Add tooltip to the link."
  (when (re-search-forward "bibliographystyle:\\([[:alnum:]]\\)\\{2,\\}" limit t)
    (forward-char -2)
    (let* ((this-link (org-element-context))
	   (path (org-element-property :path this-link))
	   (msg (shell-command-to-string (format "kpsewhich %s.bst" path))))
      (add-text-properties
       (org-element-property :begin this-link)
       (- (org-element-property :end this-link)
	  (org-element-property :post-blank this-link))
       (list
	'help-echo msg))
      (set-match-data
       (list (org-element-property :begin this-link)
	     (- (org-element-property :end this-link)
		(org-element-property :post-blank this-link))))
      (goto-char (org-element-property :end this-link)))))


(defun org-ref-make-org-link-cite-key-visible (&rest _)
  "Make the org-ref cite link visible in descriptive links."
  (unless (string= (buffer-name) "*Org Agenda*")
    (save-match-data
      (let ((s (match-string 1))
	    (s-begin (match-beginning 1))
	    (s-end (match-end 1))
	    (beg (match-beginning 0))
	    (end (match-end 0))
	    (cite-re (format "^\\(%s:\\)"
			     (regexp-opt (-sort
					  (lambda (a b)
					    (> (length a) (length b)))
					  org-ref-cite-types))))
	    cite-type)

	(when (and s (string-match cite-re s))
	  (setq cite-type (match-string 1 s))
	  (remove-text-properties beg end
				  '(invisible))
	  (add-text-properties
	   beg end
	   `(face (:foreground ,org-ref-cite-color))))))))

(when org-ref-colorize-links
  (add-hook
   'org-mode-hook
   (lambda ()
     (advice-add 'org-activate-bracket-links :after #'org-ref-make-org-link-cite-key-visible)
     (font-lock-add-keywords
      nil
      '((org-ref-match-next-cite-link (0  'org-ref-cite-face t))
	(org-ref-match-next-label-link (0  'org-ref-label-face t))
	(org-ref-match-next-ref-link (0  'org-ref-ref-face t))
	(org-ref-match-next-bibliography-link (0  'org-link t))
	(org-ref-match-next-bibliographystyle-link (0  'org-link t)))
      t))))


;;* Links
;;** bibliography and bibliographystyle
(defun org-ref-open-bibliography-no-org (link-string)
  "Open a bibliography link when you are not in org-mode.
This means you cannot use the usual org-machinery to figure it
out. We don't try to be clever here. If there is only one file,
we open it, otherwise prompt for which one to open."
  (let ((bibfiles (split-string link-string ",")))
    (find-file (if (= 1 (length bibfiles))
		   (car bibfiles)
		 (ido-completing-read
		  "Bib file: " bibfiles nil t)))))

(defun org-ref-open-bibliography (link-string)
  "The click function for a bibliography link."
  ;; get link-string boundaries we have to go to the
  ;; beginning of the line, and then search forward
  (if (not (eq major-mode 'org-mode))
      (org-ref-open-bibliography-no-org link-string)
    (let* ((bibfile)
	   ;; object is the link you clicked on
	   (object (org-element-context))
	   (link-string-beginning)
	   (link-string-end)
	   (cp (point)))
      (save-excursion
	(goto-char (org-element-property :begin object))
	(search-forward link-string nil nil 1)
	(setq link-string-beginning (match-beginning 0))
	(setq link-string-end (match-end 0)))

      ;; Make sure point is in the link-path.
      (if (< cp link-string-beginning)
	  (goto-char link-string-beginning))
      ;; We set the reftex-default-bibliography
      ;; here. it should be a local variable only in
      ;; the current buffer. We need this for using
      ;; reftex to do citations.
      (set (make-local-variable 'reftex-default-bibliography)
	   (split-string
	    (org-element-property :path object) ","))

      (let (key-beginning key-end)
	;; now if we have comma separated bibliographies
	;; we find the one clicked on. we want to
	;; search forward to next comma from point
	(save-excursion
	  (if (search-forward "," link-string-end 1 1)
	      ;; we found a match
	      (setq key-end (- (match-end 0) 1))
	    ;; no comma found so take the point
	    (setq key-end (point))))
	;; and backward to previous comma from point
	(save-excursion
	  (if (search-backward "," link-string-beginning 1 1)
	      ;; we found a match
	      (setq key-beginning (+ (match-beginning 0) 1))
	    (setq key-beginning (point)))) ; no match found
	;; save the key we clicked on.
	(setq bibfile (org-ref-strip-string
		       (buffer-substring key-beginning key-end)))
	;; open file on click
	(find-file bibfile)))))


(org-add-link-type
 "bibliography"
 ;; this code is run on clicking. The bibliography
 ;; may contain multiple files. this code finds the
 ;; one you clicked on and opens it.
 'org-ref-open-bibliography
 ;; formatting code
 (lambda (keyword desc format)
   (cond
    ((eq format 'org) (org-ref-get-org-bibliography))
    ((eq format 'ascii) (org-ref-get-ascii-bibliography))
    ((eq format 'odt) (org-ref-get-odt-bibliography))
    ((eq format 'html) (org-ref-get-html-bibliography))
    ((eq format 'latex)
     ;; write out the latex bibliography command
     (format "\\bibliography{%s}"
	     (replace-regexp-in-string
	      "\\.bib" ""
	      (mapconcat
	       'identity
	       (mapcar 'file-relative-name
		       (split-string keyword ","))
	       ",")))))))



(org-add-link-type
 "nobibliography"
 ;; this code is run on clicking. The bibliography
 ;; may contain multiple files. this code finds the
 ;; one you clicked on and opens it.
 (lambda (link-string)
   ;; get link-string boundaries
   ;; we have to go to the beginning of the line, and then search forward

   (let* ((bibfile)
	  ;; object is the link you clicked on
	  (object (org-element-context))

	  (link-string-beginning)
	  (link-string-end))

     (save-excursion
       (goto-char (org-element-property :begin object))
       (search-forward link-string nil nil 1)
       (setq link-string-beginning (match-beginning 0))
       (setq link-string-end (match-end 0)))

     ;; We set the reftex-default-bibliography
     ;; here. it should be a local variable only in
     ;; the current buffer. We need this for using
     ;; reftex to do citations.
     (set (make-local-variable 'reftex-default-bibliography)
	  (split-string (org-element-property :path object) ","))

     (let (key-beginning key-end)
       ;; now if we have comma separated bibliographies
       ;; we find the one clicked on. we want to
       ;; search forward to next comma from point
       (save-excursion
	 (if (search-forward "," link-string-end 1 1)
	     (setq key-end (- (match-end 0) 1)) ; we found a match
	   (setq key-end (point)))) ; no comma found so take the point
       ;; and backward to previous comma from point
       (save-excursion
	 (if (search-backward "," link-string-beginning 1 1)
	     (setq key-beginning (+ (match-beginning 0) 1)) ; we found a match
	   (setq key-beginning (point))))		    ; no match found
       ;; save the key we clicked on.
       (setq bibfile (org-ref-strip-string
		      (buffer-substring key-beginning key-end)))
       (find-file bibfile))))		; open file on click

 ;; formatting code
 (lambda (keyword desc format)
   (cond
    ((eq format 'org) (org-ref-get-org-bibliography))
    ((eq format 'ascii) (org-ref-get-ascii-bibliography))
    ((eq format 'odt) (org-ref-get-ascii-bibliography))
    ((eq format 'html) (org-ref-get-html-bibliography))
    ((eq format 'latex)
     ;; write out the latex bibliography command
     (format "\\nobibliography{%s}"
	     (replace-regexp-in-string
	      "\\.bib" ""
	      (mapconcat 'identity
			 (mapcar 'file-relative-name
				 (split-string keyword ","))
			 ",")))))))


(org-add-link-type
 "printbibliography"
 (lambda (arg) (message "Nothing implemented for clicking here."))
 (lambda (keyword desc format)
   (cond
    ((eq format 'org) (org-ref-get-org-bibliography))
    ((eq format 'html) (org-ref-get-html-bibliography))
    ((eq format 'latex)
     ;; write out the biblatex bibliography command
     "\\printbibliography"))))


(org-add-link-type
 "bibliographystyle"
 (lambda (arg) (message "Nothing implemented for clicking here."))
 (lambda (keyword desc format)
   (cond
    ((eq format 'latex)
     ;; write out the latex bibliography command
     (format "\\bibliographystyle{%s}" keyword))
    ;; Other styles should not have an output for this
    (t
     ""))))


(defun org-bibliographystyle-complete-link (&optional arg)
  "Completion function for bibliographystyle link.
ARG does nothing."
  (format "bibliographystyle:%s" (ido-completing-read
                                  "style: "
                                  '("unsrt" "plain" "alpha"
                                    ;; natbib
                                    ;; https://www.sharelatex.com/learn/Natbib_bibliography_styles
                                    "dinat" "humannat" "plainnat"
                                    "abbrnat" "unsrtnat" "rusnat"
                                    "ksfhnat"))))


(defun org-bibliography-complete-link (&optional arg)
  "Completion function for bibliography link.
ARG does nothing."
  (format "bibliography:%s" (read-file-name "enter file: " nil nil t)))


;;;###autoload
(defun org-ref-insert-bibliography-link ()
  "Insert a bibliography with completion."
  (interactive)
  (insert (org-bibliography-complete-link)))

;;** addbibresource

(org-add-link-type
 "addbibresource"
 ;; this code is run on clicking. The addbibresource may
 ;; contain multiple files. this code finds the one you
 ;; clicked on and opens it.
 (lambda (link-string)
   ;; get link-string boundaries. we have to go to the
   ;; beginning of the line, and then search forward

   (let* ((bibfile)
	  ;; object is the link you clicked on
	  (object (org-element-context))

	  (link-string-beginning)
	  (link-string-end))

     (save-excursion
       (goto-char (org-element-property :begin object))
       (search-forward link-string nil nil 1)
       (setq link-string-beginning (match-beginning 0))
       (setq link-string-end (match-end 0)))

     ;; We set the reftex-default-bibliography
     ;; here. it should be a local variable only in
     ;; the current buffer. We need this for using
     ;; reftex to do citations.
     (set (make-local-variable 'reftex-default-bibliography)
	  (split-string (org-element-property :path object) ","))

     (let (key-beginning key-end)
       ;; now if we have comma separated bibliographies
       ;; we find the one clicked on. we want to
       ;; search forward to next comma from point
       (save-excursion
	 (if (search-forward "," link-string-end 1 1)
	     (setq key-end (- (match-end 0) 1)) ; we found a match
	   (setq key-end (point)))) ; no comma found so take the point
       ;; and backward to previous comma from point
       (save-excursion
	 (if (search-backward "," link-string-beginning 1 1)
	     (setq key-beginning (+ (match-beginning 0) 1)) ; we found a match
	   (setq key-beginning (point))))		    ; no match found
       ;; save the key we clicked on.
       (setq bibfile (org-ref-strip-string
		      (buffer-substring key-beginning key-end)))
       (find-file bibfile))))		; open file on click

 ;; formatting code
 (lambda (keyword desc format)
   (cond
    ((eq format 'html) (format ""))	; no output for html
    ((eq format 'latex)
     ;; write out the latex addbibresource command
     (format "\\addbibresource{%s}" keyword)))))

;;** List of figures

;; org-in-commented-heading-p was introduced in org commit 6d1d61f6. Org version
;; 8.3 was the first version to contain this function.  This is provided for
;; backward compatibility for the org-mode included with Emacs.
(unless (fboundp 'org-in-commented-heading-p)
  (defun org-in-commented-heading-p (&optional no-inheritance)
    "Non-nil if point is under a commented heading.
This function also checks ancestors of the current headline,
unless optional argument NO-INHERITANCE is non-nil."
    (cond
     ((org-before-first-heading-p) nil)
     ((let ((headline (nth 4 (org-heading-components))))
        (and headline
             (let ((case-fold-search nil))
               (org-string-match-p
		(concat
		 "^"
		 org-comment-string "\\(?: \\|$\\)")
		headline)))))
     (no-inheritance nil)
     (t
      (save-excursion (and (org-up-heading-safe) (org-in-commented-heading-p)))))))

;;;###autoload
(defun org-ref-list-of-figures (&optional arg)
  "Generate buffer with list of figures in them.
ARG does nothing.
Ignore figures in COMMENTED sections."
  (interactive)
  (save-excursion
    (widen)
    (let* ((c-b (buffer-name))
	   (counter 0)
	   (list-of-figures
	    (org-element-map (org-element-parse-buffer) 'link
	      (lambda (link)
		"create a link for to the figure"
		(when
		    (and (string= (org-element-property :type link) "file")
			 (string-match-p
			  "[^.]*\\.\\(png\\|jpg\\|eps\\|pdf\\)$"
			  (org-element-property :path link))
			 ;; ignore commented sections
			 (save-excursion
			   (goto-char (org-element-property :begin link))
			   (not (or (org-in-commented-heading-p)
				    (org-at-comment-p)
				    (-intersection (org-get-tags-at) org-export-exclude-tags)))))
		  (cl-incf counter)

		  (let* ((start (org-element-property :begin link))
			 (linenum (progn (goto-char start) (line-number-at-pos)))
			 (fname (org-element-property :path link))
			 (parent (car (cdr
				       (org-element-property :parent link))))
			 (caption (cl-caaar (plist-get parent :caption)))
			 (name (plist-get parent :name)))

		    (if caption
			(format "[[file:%s::%s][Figure %s:]] %s\n" c-b linenum counter caption)
		      ;; if it has no caption, try the name
		      ;; if it has no name, use the file name
		      (cond (name
			     (format "[[file:%s::%s][Figure %s:]] %s\n" c-b linenum counter name))
			    (fname
			     (format "[[file:%s::%s][Figure %s:]] %s\n"
				     c-b linenum counter fname))))))))))
      (switch-to-buffer "*List of Figures*")
      (setq buffer-read-only nil)
      (org-mode)
      (erase-buffer)
      (insert (mapconcat 'identity list-of-figures ""))
      (goto-char (point-min))
      ;; open links in the same window
      (setq-local org-link-frame-setup
		  '((file . find-file)))
      (setq buffer-read-only t)
      (use-local-map (copy-keymap org-mode-map))
      (local-set-key "q" #'(lambda () (interactive) (kill-buffer))))))


(org-add-link-type
 "list-of-figures"
 'org-ref-list-of-figures		; on click
 (lambda (keyword desc format)
   (cond
    ((eq format 'latex)
     (format "\\listoffigures")))))

;;** List of tables
;;;###autoload
(defun org-ref-list-of-tables (&optional arg)
  "Generate a buffer with a list of tables.
ARG does nothing."
  (interactive)
  (save-excursion
    (widen)
    (let* ((c-b (buffer-name))
           (counter 0)
           (list-of-tables
            (org-element-map (org-element-parse-buffer 'element) 'table
              (lambda (table)
                "create a link for to the table"
		(when
		    ;; ignore commented sections
		    (save-excursion
		      (goto-char (org-element-property :begin table))
		      (not (or (org-in-commented-heading-p)
        (-intersection (org-get-tags-at) org-export-exclude-tags))))
      (cl-incf counter)
		  (let ((start (org-element-property :begin table))
			(name  (org-element-property :name table))
			(caption (cl-caaar (org-element-property :caption table))))
		    (if caption
			(format
			 "[[elisp:(progn (switch-to-buffer \"%s\")(goto-char %s)(org-show-entry))][table %s: %s]] %s\n"
			 c-b start counter (or name "") caption)
		      (format
		       "[[elisp:(progn (switch-to-buffer \"%s\")(goto-char %s)(org-show-entry))][table %s: %s]]\n"
		       c-b start counter (or name "")))))))))
      (switch-to-buffer "*List of Tables*")
      (setq buffer-read-only nil)
      (org-mode)
      (erase-buffer)
      (insert (mapconcat 'identity list-of-tables ""))
      (setq buffer-read-only t)
      (use-local-map (copy-keymap org-mode-map))
      (local-set-key "q" #'(lambda () (interactive) (kill-buffer))))))


(org-add-link-type
 "list-of-tables"
 'org-ref-list-of-tables
 (lambda (keyword desc format)
   (cond
    ((eq format 'latex)
     (format "\\listoftables")))))


;;** label link
(defun org-ref-count-labels (label)
  "Count number of LABELs in the document."
  (+ (count-matches
      (format "label:%s\\b[^-:]" label)
      (point-min) (point-max))
     ;; for tblname, it is not enough to get word boundary
     ;; tab-little and tab-little-2 match then.
     (count-matches
      (format "^#\\+tblname:\\s-*%s\\b[^-:]" label)
      (point-min) (point-max))
     (count-matches (format "\\label{%s}" label)
                    (point-min) (point-max))
     ;; this is the org-format #+label:
     (count-matches (format "^#\\+label:\\s-*%s\\b[^-:]" label)
                    (point-min) (point-max))
     ;; #+name:
     (count-matches (format "^#\\+name:\\s-*%s\\b[^-:]" label)
		    (point-min) (point-max))
     (let ((custom-id-count 0))
       (when (and (buffer-file-name)
		  (file-exists-p (buffer-file-name)))
	 (save-buffer))
       (org-map-entries
        (lambda ()
          (when (string= label (org-entry-get (point) "CUSTOM_ID"))
            (setq custom-id-count (+ 1 custom-id-count)))))
       custom-id-count)))


(org-add-link-type
 "label"
 (lambda (label)
   "On clicking count the number of label tags used in the buffer.
A number greater than one means multiple labels!"
   (let ((count (org-ref-count-labels label)))
     (message (format "%s occurence%s"
                      count
                      (if (or (= count 0)
                              (> count 1))
                          "s"
                        ""))
              (org-ref-count-labels label))))
 (lambda (keyword desc format)
   (cond
    ((eq format 'html) (format "<div id=\"%s\">" keyword))
    ((eq format 'latex)
     (format "\\label{%s}" keyword)))))


(defun org-label-store-link ()
  "Store a link to a label.  The output will be a ref to that label."
  ;; First we have to make sure we are on a label link.
  (let* ((object (org-element-context)))
    (when (and (equal (org-element-type object) 'link)
               (equal (org-element-property :type object) "label"))
      (org-store-link-props
       :type "ref"
       :link (concat "ref:" (org-element-property :path object))))

    ;; Store link on table
    (when (equal (org-element-type object) 'table)
      (org-store-link-props
       :type "ref"
       :link (concat "ref:" (org-element-property :name object))))

    ;; store link on heading with custom_id
    ;; this is not a ref link, but it is still what you want
    (when (and (equal (org-element-type object) 'headline)
               (org-entry-get (point) "CUSTOM_ID"))
      (org-store-link-props
       :type "custom_id"
       :link (format "[[#%s]]" (org-entry-get (point) "CUSTOM_ID"))))

    ;; and to #+label: lines

    (when (and (equal (org-element-type object) 'paragraph)
               (org-element-property :name object))
      (org-store-link-props
       :type "ref"
       :link (concat "ref:" (org-element-property :name object))))))


(add-hook 'org-store-link-functions 'org-label-store-link)

;;** ref link
(org-add-link-type
 "ref"
 (lambda (label)
   "on clicking goto the label. Navigate back with C-c &"
   (org-mark-ring-push)
   ;; next search from beginning of the buffer it is possible you would not find
   ;; the label if narrowing is in effect
   (widen)
   (unless
       (or
        ;; our label links
        (progn
          (goto-char (point-min))
          (re-search-forward (format "label:%s\\b" label) nil t))

        ;; a latex label
        (progn
          (goto-char (point-min))
          (re-search-forward (format "\\label{%s}" label) nil t))

        ;; #+label: name  org-definition
        (progn
          (goto-char (point-min))
          (re-search-forward
           (format "^#\\+label:\\s-*\\(%s\\)\\b" label) nil t))

        ;; org tblname
        (progn
          (goto-char (point-min))
          (re-search-forward
           (format "^#\\+tblname:\\s-*\\(%s\\)\\b" label) nil t))

	;; a #+name
	(progn
          (goto-char (point-min))
          (re-search-forward
           (format "^#\\+name:\\s-*\\(%s\\)\\b" label) nil t))

	;; CUSTOM_ID
	(progn
	  (goto-char (point-min))
	  (let ((p (org-map-entries
		    (lambda ()
		      (point))
		    (format "CUSTOM_ID=\"%s\"" label))))
	    (if (not (= 1 (length p)))
		nil
	      (goto-char (car p))))))

     ;; we did not find anything, so go back to where we came
     (org-mark-ring-goto)
     (error "%s not found" label))
   (org-show-entry)
   (message "go back with (org-mark-ring-goto) `C-c &`"))
                                        ;formatting
 (lambda (keyword desc format)
   (cond
    ((eq format 'html) (format "<a href=\"#%s\">%s</a>" keyword keyword))
    ((eq format 'latex)
     (format "\\ref{%s}" keyword)))))


(defun org-ref-get-org-labels ()
  "Return a list of #+LABEL: labels."
  (save-excursion
    (goto-char (point-min))
    (let ((matches '()))
      (while (re-search-forward "^#\\+label:\\s-+\\(.*\\)\\b" (point-max) t)
        ;; do not do this for tables. We get those in `org-ref-get-tblnames'.
        ;; who would have thought you have save match data here? Trust me. When
        ;; I wrote this, you did.
        (unless (save-match-data  (equal (car (org-element-at-point)) 'table))
          (add-to-list 'matches (match-string-no-properties 1) t)))
      matches)))


(defun org-ref-get-custom-ids ()
  "Return a list of custom_id properties in the buffer."
  (when (and (buffer-file-name) (file-exists-p (buffer-file-name)))
    (save-buffer))
  (let ((results '()) custom_id)
    (org-map-entries
     (lambda ()
       (let ((custom_id (org-entry-get (point) "CUSTOM_ID")))
         (when (not (null custom_id))
           (setq results (append results (list custom_id)))))))
    results))


(defun org-ref-get-latex-labels ()
  "Return list of matchin LaTeX defined labels in buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((matches '()))
      (while (re-search-forward "\\\\label{\\([a-zA-z0-9:-]*\\)}"
				(point-max) t)
        (add-to-list 'matches (match-string-no-properties 1) t))
      matches)))


(defun org-ref-get-tblnames ()
  "Return list of table names in the buffer."
  (org-element-map (org-element-parse-buffer 'element) 'table
    (lambda (table)
      (org-element-property :name table))))


(defun org-ref-get-names ()
  "Return list of names in the buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((matches '()))
	(while (re-search-forward "^#\\+name:\\s-+\\(.*\\)" nil t)
	  (pushnew (match-string 1) matches))
	matches))))


(defun org-ref-get-labels ()
  "Return a list of labels in the buffer that you can make a ref link to.
This is used to complete ref links."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((matches '()))
        ;; these are the org-ref label:stuff  kinds
        (while (re-search-forward
                "[^#+]label:\\([a-zA-z0-9:-]*\\)" (point-max) t)
	  (setq matches (append matches
				(list
				 (match-string-no-properties 1)))))
        ;; now add all the other kinds of labels.
        (append matches
                ;; #+label:
                (org-ref-get-org-labels)
                ;; \label{}
                (org-ref-get-latex-labels)
                ;; #+tblname: and actually #+label
                (org-ref-get-tblnames)
                ;; CUSTOM_IDs
                (org-ref-get-custom-ids)
		;; names
		(org-ref-get-names))))))


(defun org-ref-complete-link (&optional arg)
  "Completion function for ref links.
Optional argument ARG Does nothing."
  (let ((label))
    (setq label (completing-read "label: " (org-ref-get-labels)))
    (format "ref:%s" label)))


;;;###autoload
(defun org-ref-insert-ref-link ()
  "Completion function for a ref link."
  (interactive)
  (insert (org-ref-complete-link)))


;;*** pageref link
(org-add-link-type
 "pageref"
 (lambda (label)
   "on clicking goto the label. Navigate back with C-c &"
   (org-mark-ring-push)
   ;; next search from beginning of the buffer
   (widen)
   (unless
       (or
        ;; our label links
        (progn
          (goto-char (point-min))
          (re-search-forward (format "label:%s\\b" label) nil t))

        ;; a latex label
        (progn
          (goto-char (point-min))
          (re-search-forward (format "\\label{%s}" label) nil t))

        ;; #+label: name  org-definition
        (progn
          (goto-char (point-min))
          (re-search-forward
           (format "^#\\+label:\\s-*\\(%s\\)\\b" label) nil t))

        ;; org tblname
        (progn
          (goto-char (point-min))
          (re-search-forward
           (format "^#\\+tblname:\\s-*\\(%s\\)\\b" label) nil t)))
     ;; we did not find anything, so go back to where we came
     (org-mark-ring-goto)
     (error "%s not found" label))
   (message "go back with (org-mark-ring-goto) `C-c &`"))
                                        ;formatting
 (lambda (path desc format)
   (cond
    ((eq format 'html) (format "(<pageref>%s</pageref>)" path))
    ((eq format 'latex)
     (format "\\pageref{%s}" path)))))


(defun org-pageref-complete-link (&optional arg)
  "Completion function for ref links.
Optional argument ARG Does nothing."
  (let ((label))
    (setq label (completing-read "label: " (org-ref-get-labels)))
    (format "ref:%s" label)))


;;;###autoload
(defun org-pageref-insert-ref-link ()
  "Insert a pageref link with completion."
  (interactive)
  (insert (org-pageref-complete-link)))


;;*** nameref link
(org-add-link-type
 "nameref"
 (lambda (label)
   "on clicking goto the label. Navigate back with C-c &"
   (org-mark-ring-push)
   ;; next search from beginning of the buffer
   (widen)
   (unless
       (or
        ;; a latex label
        (progn
          (goto-char (point-min))
          (re-search-forward (format "\\label{%s}" label) nil t))
        )
     ;; we did not find anything, so go back to where we came
     (org-mark-ring-goto)
     (error "%s not found" label))
   (message "go back with (org-mark-ring-goto) `C-c &`"))
                                        ;formatting
 (lambda (path desc format)
   (cond
    ((eq format 'html) (format "(<nameref>%s</nameref>)" path))
    ((eq format 'latex)
     (format "\\nameref{%s}" path)))))

;;*** eqref link

(org-add-link-type
 "eqref"
 (lambda (label)
   "on clicking goto the label. Navigate back with C-c &"
   (org-mark-ring-push)
   ;; next search from beginning of the buffer
   (widen)
   (goto-char (point-min))
   (unless
       (or
        ;; search forward for the first match
        ;; our label links
        (re-search-forward (format "label:%s" label) nil t)
        ;; a latex label
        (re-search-forward (format "\\label{%s}" label) nil t)
        ;; #+label: name  org-definition
        (re-search-forward (format "^#\\+label:\\s-*\\(%s\\)\\b" label) nil t))
     (org-mark-ring-goto)
     (error "%s not found" label))
   (message "go back with (org-mark-ring-goto) `C-c &`"))
                                        ;formatting
 (lambda (keyword desc format)
   (cond
    ((eq format 'latex) (format "\\eqref{%s}" keyword))
    ;;considering the fact that latex's the standard of math formulas, just use mathjax to render the html
    ;;customize the variable 'org-html-mathjax-template' and 'org-html-mathjax-options' refering to  'autonumber'
    ((eq format 'html) (format "\\eqref{%s}" keyword)))))

;;** cite link

(defun org-ref-get-bibtex-key-under-cursor ()
  "Return key under the bibtex cursor.
We search forward from
point to get a comma, or the end of the link, and then backwards
to get a comma, or the beginning of the link.  that delimits the
keyword we clicked on.  We also strip the text properties."
  (let* ((object (org-element-context))
         (link-string (org-element-property :path object)))
    ;; you may click on the part before the citations. here we make
    ;; sure to move to the beginning so you get the first citation.
    (let ((cp (point)))
      (goto-char (org-element-property :begin object))
      (search-forward link-string (org-element-property :end object))
      (goto-char (match-beginning 0))
      ;; check if we clicked before the path and move as needed.
      (unless (< cp (point))
        (goto-char cp)))

    (if (not (org-element-property :contents-begin object))
        ;; this means no description in the link
        (progn
          ;; we need the link path start and end
          (let (link-string-beginning link-string-end)
            (save-excursion
              (goto-char (org-element-property :begin object))
              (search-forward link-string nil nil 1)
              (setq link-string-beginning (match-beginning 0))
              (setq link-string-end (match-end 0)))

            (let (key-beginning key-end)
              ;; The key is the text between commas, or the link boundaries
              (save-excursion
                (if (search-forward "," link-string-end t 1)
                    (setq key-end (- (match-end 0) 1)) ; we found a match
                  (setq key-end link-string-end))) ; no comma found so take the end
              ;; and backward to previous comma from point which defines the start character
              (save-excursion
                (if (search-backward "," link-string-beginning 1 1)
                    (setq key-beginning (+ (match-beginning 0) 1)) ; we found a match
                  (setq key-beginning link-string-beginning))) ; no match found
              ;; save the key we clicked on.
              (let ((bibtex-key
                     (org-ref-strip-string
                      (buffer-substring key-beginning key-end))))
                (set-text-properties 0 (length bibtex-key) nil bibtex-key)
                bibtex-key))))
      ;; link with description. assume only one key
      link-string)))


(defun org-ref-find-bibliography ()
  "Find the bibliography in the buffer.
This function sets and returns cite-bibliography-files, which is
a list of files either from bibliography:f1.bib,f2.bib
\bibliography{f1,f2}, internal bibliographies, from files in the
BIBINPUTS env var, and finally falling back to what the user has
set in `org-ref-default-bibliography'"
  (catch 'result
    ;; If you call this in a bibtex file, assume we want this file
    (when (string= (or (f-ext (or (buffer-file-name) "")) "")  "bib")
      (setq org-ref-bibliography-files (list (buffer-file-name)))
      (throw 'result org-ref-bibliography-files))

    ;; otherwise, check current file for a bibliography source
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))

	;; look for org-ref bibliography or addbibresource links
	(setq org-ref-bibliography-files nil)
	(while (re-search-forward
		"\\<\\(bibliography\\|addbibresource\\):\\([^\]\|\n]+\\)"
		nil t)
	  (setq org-ref-bibliography-files
		(append org-ref-bibliography-files
			(mapcar 'org-ref-strip-string
				(split-string (match-string 2) ",")))))
	;; locate the corresponding bib files
	(setq org-ref-bibliography-files
	      (reftex-locate-bibliography-files default-directory
						org-ref-bibliography-files))
	(when org-ref-bibliography-files
	  (throw 'result org-ref-bibliography-files))

	;; we did not find org-ref links. now look for latex links
	(goto-char (point-min))
	(setq org-ref-bibliography-files
	      (reftex-locate-bibliography-files default-directory))
	(when org-ref-bibliography-files
	  (throw 'result org-ref-bibliography-files))))

    ;; we did not find anything. use defaults
    (setq org-ref-bibliography-files org-ref-default-bibliography))


  ;; set reftex-default-bibliography so we can search
  (set (make-local-variable 'reftex-default-bibliography) org-ref-bibliography-files)
  org-ref-bibliography-files)


(defun org-ref-key-in-file-p (key filename)
  "Determine if the KEY is in the FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
    (bibtex-search-entry key)))


(defun org-ref-get-bibtex-key-and-file (&optional key)
  "Return the bibtex KEY and file that it is in.
If no key is provided, get one under point."
  (let ((org-ref-bibliography-files (org-ref-find-bibliography))
        (file))
    (unless key
      (setq key (org-ref-get-bibtex-key-under-cursor)))
    (setq file (catch 'result
		 (cl-loop for file in org-ref-bibliography-files do
			  (if (org-ref-key-in-file-p
			       key
			       (file-truename file))
			      (throw 'result file)))))
    (cons key (when (stringp file) (substring-no-properties file)))))


;;*** Generation of the cite links
(defmacro org-ref-make-completion-function (type)
  "Macro to make a link completion function for a link of TYPE."
  `(defun ,(intern (format "org-%s-complete-link" type)) (&optional arg)
     (format
      "%s:%s"
      ,type
      (completing-read
       "bibtex key: "
       (let ((bibtex-files (org-ref-find-bibliography)))
	 (bibtex-global-key-alist))))))


(defmacro org-ref-make-format-function (type)
  "Macro to make a format function for a link of TYPE."
  `(defun ,(intern (format "org-ref-format-%s" type)) (keyword desc format)
     ,(format "Formatting function for %s links.
[[%s:KEYWORD][DESC]]
FORMAT is a symbol for the export backend.
Supported backends: 'html, 'latex, 'ascii, 'org, 'md" type type)
     (cond
      ((eq format 'org)
       (mapconcat
  (lambda (key)
    (format "[[#%s][%s]]" key key))
  (org-ref-split-and-strip-string keyword) ","))

      ((eq format 'ascii)
       (concat "["
         (mapconcat
    (lambda (key)
      (format "%s" key))
    (org-ref-split-and-strip-string keyword) ",") "]"))

      ((eq format 'html)
       (mapconcat
  (lambda (key)
    (format "<a class='org-ref-reference' href=\"#%s\">%s</a>" key key))
  (org-ref-split-and-strip-string keyword) ","))

      ((eq format 'latex)
       (if (string= (substring ,type -1) "s")
	   ;; biblatex format for multicite commands, which all end in s. These
	   ;; are formated as \cites{key1}{key2}...
	   (concat "\\" ,type
		   (mapconcat (lambda (key) (format "{%s}" key))
			      (org-ref-split-and-strip-string keyword) ""))
	 ;; bibtex format
	 (concat "\\" ,type
		 (when desc (org-ref-format-citation-description desc)) "{"
		 (mapconcat
		  (lambda (key) key)
		  (org-ref-split-and-strip-string keyword) ",")
		 "}")))
      ;; simple format for odt.
      ((eq format 'odt)
       (format "[%s]" keyword))

      ;; for markdown we generate pandoc citations
      ((eq format 'md)
       (cond
  (desc ;; pre and or post text
   (let* ((text (split-string desc "::"))
    (pre (car text))
    (post (cadr text)))
     (concat
      (format "[@%s," keyword)
      (when pre (format " %s" pre))
      (when post (format ", %s" post))
      "]")))
  (t
   (format "[%s]"
     (mapconcat
      (lambda (key) (concat "@" key))
      (org-ref-split-and-strip-string keyword)
      "; "))))))))



(defun org-ref-format-citation-description (desc)
  "Return formatted citation description.
If the cite link has a DESC (description), it is optional text
for the citation command.  You can specify pre and post text by
separating these with ::, for example [[cite:key][pre text::post
text]]."
  (cond
   ((string-match "::" desc)
    (let ((results (split-string desc "::")))
      (format "[%s][%s]" (nth 0 results) (nth 1 results))))
   (t (format "[%s]" desc))))


;;;###autoload
(defun org-ref-define-citation-link (type &optional key)
  "Add a citation link of TYPE for `org-ref'.
With optional KEY, set the reftex binding.  For example:
\(org-ref-define-citation-link \"citez\" ?z) will create a new
citez link, with reftex key of z, and the completion function."
  (interactive "sCitation Type: \ncKey: ")

  ;; create the formatting function
  (eval `(org-ref-make-format-function ,type))

  (eval
   `(org-add-link-type
     ,type
     (lambda (_path) (funcall org-ref-cite-onclick-function nil))
     (quote ,(intern (format "org-ref-format-%s" type)))))

  ;; create the completion function
  (eval `(org-ref-make-completion-function ,type))

  ;; store new type so it works with adding citations, which checks
  ;; for existence in this list
  (add-to-list 'org-ref-cite-types type)

  ;; and finally if a key is specified, we modify the reftex menu
  (when key
    (setf (nth 2 (assoc 'org reftex-cite-format-builtin))
          (append (nth 2 (assoc 'org reftex-cite-format-builtin))
                  `((,key  . ,(concat type ":%l")))))))

;; create all the link types and their completion functions
(dolist (type org-ref-cite-types)
  (org-ref-define-citation-link type))


;;;###autoload
(defun org-ref-insert-cite-with-completion (type)
  "Insert a cite link of TYPE with completion."
  (interactive (list (ido-completing-read "Type: " org-ref-cite-types)))
  (insert (funcall (intern (format "org-%s-complete-link" type)))))


;;;###autoload
(defun org-ref-store-bibtex-entry-link ()
  "Save a citation link to the current bibtex entry.
Save in the default link type."
  (interactive)
  (let ((link (concat org-ref-default-citation-link
                      ":"
                      (save-excursion
                        (bibtex-beginning-of-entry)
                        (reftex-get-bib-field
			 "=key=" (bibtex-parse-entry))))))
    (message "saved %s" link)
    (push (list link) org-stored-links)
    (car org-stored-links)))

;;** Index link
(org-add-link-type
 "index"
 (lambda (path)
   (occur path))

 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (format "\\index{%s}" path)))))

;; this will generate a temporary index of entries in the file when clicked on.
;;;###autoload
(defun org-ref-index (&optional path)
  "Open an *index* buffer with links to index entries.
PATH is required for the org-link, but it does nothing here."
  (interactive)
  (let ((*index-links* '())
	(*initial-letters* '()))

    ;; get links
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
	(let ((type (nth 0 link))
	      (plist (nth 1 link)))

	  (when (equal (plist-get plist ':type) "index")
	    (add-to-list
	     '*index-links*
	     (cons (plist-get plist :path)
		   (format
		    "[[elisp:(progn (switch-to-buffer \"%s\") (goto-char %s) (org-cycle '(64)))][%s]] "
		    (current-buffer)
		    (plist-get plist :begin) ;; position of link
		    ;; grab a description
		    (save-excursion
		      (goto-char (plist-get plist :begin))
		      (if (thing-at-point 'sentence)
			  ;; get a sentence
			  (let ((s (thing-at-point 'sentence)))
			    (cl-loop for char in '("[" "]" "\n")
				  do
				  (setq s (replace-regexp-in-string
					   (regexp-quote char) " " s)))
			    (concat s " "))
			;; or call it a link
			"link")))))))))

    ;; sort the links
    (setq *index-links* (cl-sort *index-links* 'string-lessp :key 'car))

    ;; now separate out into chunks first letters
    (dolist (link *index-links*)
      (add-to-list '*initial-letters* (substring (car link) 0 1) t))

    ;; now create the index
    (switch-to-buffer (get-buffer-create "*index*"))
    (org-mode)
    (erase-buffer)
    (insert "#+TITLE: Index\n\n")
    (dolist (letter *initial-letters*)
      (insert (format "* %s\n" (upcase letter)))
      ;; now process the links
      (while (and
	      *index-links*
	      (string= letter (substring (car (car *index-links*)) 0 1)))
	(let ((link (pop *index-links*)))
	  (insert (format "%s %s\n\n" (car link) (cdr link))))))
    (switch-to-buffer "*index*")))


(org-add-link-type
 "printindex"
 'org-ref-index
 ;; formatting
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (format "\\printindex")))))

;;* Utilities
;;** create text citations from a bibtex entry
(defun org-ref-bib-citation ()
  "From a bibtex entry, create and return a simple citation string.
This assumes you are in an article."
  (bibtex-set-dialect nil t)
  (bibtex-beginning-of-entry)
  (let* ((cb (current-buffer))
         (bibtex-expand-strings t)
         (entry (cl-loop for (key . value) in (bibtex-parse-entry t)
                         collect (cons (downcase key) value)))
         (title (replace-regexp-in-string
		 "\n\\|\t\\|\s+" " "
		 (reftex-get-bib-field "title" entry)))
         (year  (reftex-get-bib-field "year" entry))
         (author (replace-regexp-in-string
		  "\n\\|\t\\|\s+" " "
		  (reftex-get-bib-field "author" entry)))
         (key (reftex-get-bib-field "=key=" entry))
         (journal (let ((jt (reftex-get-bib-field "journal" entry)))
                    (if (string= "" jt)
                        (reftex-get-bib-field "journaltitle" entry)
                      jt)))
         (volume (reftex-get-bib-field "volume" entry))
         (pages (reftex-get-bib-field "pages" entry))
         (doi (reftex-get-bib-field "doi" entry))
         (url (reftex-get-bib-field "url" entry))
         )
    ;;authors, "title", Journal, vol(iss):pages (year).
    (format "%s, \"%s\", %s, %s:%s (%s)"
            author title journal  volume pages year)))


(defun org-ref-bib-html-citation ()
  "From a bibtex entry, create and return a simple citation with html links."
  (bibtex-beginning-of-entry)
  (let* ((cb (current-buffer))
         (bibtex-expand-strings t)
         (entry (cl-loop for (key . value) in (bibtex-parse-entry t)
                         collect (cons (downcase key) value)))
         (title (replace-regexp-in-string
		 "\n\\|\t\\|\s+" " "
		 (reftex-get-bib-field "title" entry)))
         (year  (reftex-get-bib-field "year" entry))
         (author (replace-regexp-in-string
		  "\n\\|\t\\|\s+" " "
		  (reftex-get-bib-field "author" entry)))
         (key (reftex-get-bib-field "=key=" entry))
         (journal (reftex-get-bib-field "journal" entry))
         (volume (reftex-get-bib-field "volume" entry))
         (pages (reftex-get-bib-field "pages" entry))
         (doi (reftex-get-bib-field "doi" entry))
         (url (reftex-get-bib-field "url" entry)))
    ;;authors, "title", Journal, vol(iss):pages (year).
    (concat (format "%s, \"%s\", %s, %s:%s (%s)."
                    author title journal  volume pages year)
            (when url (format " <a href=\"%s\">link</a>" url))
            (when doi
              (format " <a href=\"http://dx.doi.org/%s\">doi</a>" doi)))))

;;** Open pdf in bibtex entry
;;;###autoload
(defun org-ref-open-bibtex-pdf ()
  "Open pdf for a bibtex entry, if it exists.
assumes point is in
the entry of interest in the bibfile.  but does not check that."
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((bibtex-expand-strings t)
           (entry (bibtex-parse-entry t))
           (key (reftex-get-bib-field "=key=" entry))
           (pdf (format (concat
			 (file-name-as-directory org-ref-pdf-directory)
			 "%s.pdf")
			key)))
      (message "%s" pdf)
      (if (file-exists-p pdf)
          (org-open-link-from-string (format "[[file:%s]]" pdf))
        (ding)))))

;;** Open notes from bibtex entry
;;;###autoload
(defun org-ref-open-bibtex-notes ()
  "From a bibtex entry, open the notes if they exist.
If the notes do not exist, then create a heading.

I never did figure out how to use reftex to make this happen
non-interactively.  the `reftex-format-citation' function did not
work perfectly; there were carriage returns in the strings, and
it did not put the key where it needed to be.  so, below I replace
the carriage returns and extra spaces with a single space and
construct the heading by hand."
  (interactive)

  (bibtex-beginning-of-entry)
  (let* ((cb (current-buffer))
         (bibtex-expand-strings t)
         (entry (cl-loop for (key . value) in (bibtex-parse-entry t)
                         collect (cons (downcase key) value)))
         (key (reftex-get-bib-field "=key=" entry))
	 pdf)

    ;; save key to clipboard to make saving pdf later easier by pasting.
    (with-temp-buffer
      (insert key)
      (kill-ring-save (point-min) (point-max)))

    ;; now look for entry in the notes file
    (save-restriction
      (if  org-ref-bibliography-notes
          (find-file-other-window org-ref-bibliography-notes)
        (error "Org-ref-bib-bibliography-notes is not set to anything"))

      (widen)
      (goto-char (point-min))
      (let* ((headlines (org-element-map
			    (org-element-parse-buffer)
			    'headline 'identity))
	     (keys (mapcar
		    (lambda (hl) (org-element-property :CUSTOM_ID hl))
		    headlines)))
	;; put new entry in notes if we don't find it.
	(if (-contains? keys key)
	    (progn
	      (org-open-link-from-string (format "[[#%s]]" key))
	      (funcall org-ref-open-notes-function))
	  ;; no entry found, so add one
	  (goto-char (point-max))
	  (insert (org-ref-reftex-format-citation
		   entry (concat "\n" org-ref-note-title-format)))

	  (insert (format "[[cite:%s]]" key))

	  (setq pdf (expand-file-name
		     (format "%s.pdf" key) org-ref-pdf-directory))
	  (if (file-exists-p pdf)
	      (insert (format
		       " [[file:%s][pdf]]\n\n"
		       pdf))
	    ;; no pdf found. Prompt for a path, but allow no pdf to be inserted.
	    (let ((pdf (read-file-name "PDF: " nil "no pdf" nil "no pdf")))
	      (when (not (string= pdf "no pdf"))
		(insert (format
			 " [[file:%s][pdf]]\n\n"
			 pdf)))))
	  (save-buffer))))))


;;** Open bibtex entry in browser
;;;###autoload
(defun org-ref-open-in-browser ()
  "Open the bibtex entry at point in a browser using the url field or doi field."
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (catch 'done
      (let ((url (bibtex-autokey-get-field "url")))
        (when  url
          (browse-url url)
          (throw 'done nil)))

      (let ((doi (bibtex-autokey-get-field "doi")))
        (when doi
          (if (string-match "^http" doi)
              (browse-url doi)
            (browse-url (format "http://dx.doi.org/%s" doi)))
          (throw 'done nil)))
      (message "No url or doi found"))))


;;** Build a pdf of the bibtex file
;;;###autoload
(defun org-ref-build-full-bibliography ()
  "Build pdf of all bibtex entries, and open it."
  (interactive)
  (let* ((bibfile (file-name-nondirectory (buffer-file-name)))
         (bib-base (file-name-sans-extension bibfile))
         (texfile (concat bib-base ".tex"))
         (pdffile (concat bib-base ".pdf")))
    (find-file texfile)
    (erase-buffer)
    (insert (format "\\documentclass[12pt]{article}
\\usepackage[version=3]{mhchem}
\\usepackage{url}
\\usepackage[numbers]{natbib}
\\usepackage[colorlinks=true, linkcolor=blue, urlcolor=blue, pdfstartview=FitH]{hyperref}
\\usepackage{doi}
\\begin{document}
\\nocite{*}
\\bibliographystyle{unsrtnat}
\\bibliography{%s}
\\end{document}" bib-base))
    (save-buffer)
    (shell-command (concat "pdflatex " bib-base))
    (shell-command (concat "bibtex " bib-base))
    (shell-command (concat "pdflatex " bib-base))
    (shell-command (concat "pdflatex " bib-base))
    (kill-buffer texfile)
    (org-open-file pdffile)
    ))

;;** Extract bibtex entries in org-file

;;;###autoload
;; (defun org-ref-extract-bibtex-entries ()
;;   "Extract the bibtex entries in the current buffer into a src block.

;; If no bibliography is in the buffer the variable
;; `reftex-default-bibliography' is used."
;;   (interactive)
;;   (let* ((temporary-file-directory (if (buffer-file-name)
;;				       (file-name-directory
;;					(buffer-file-name))
;;				     "."))
;;          (tempname (make-temp-file "extract-bib"))
;;          (contents (buffer-string))
;;          (cb (current-buffer))
;;          basename texfile bibfile results)

;;     ;; open tempfile and insert org-buffer contents
;;     (find-file tempname)
;;     (insert contents)
;;     (setq basename (file-name-sans-extension
;;                     (file-name-nondirectory buffer-file-name))
;;           texfile (concat tempname ".tex")
;;           bibfile (concat tempname ".bib"))

;;     ;; see if we have a bibliography, and insert the default one if not.
;;     (save-excursion
;;       (goto-char (point-min))
;;       (unless (re-search-forward "^bibliography:" (point-max) 'end)
;;         (insert (format "\nbibliography:%s"
;;                         (mapconcat 'identity
;;				   reftex-default-bibliography ",")))))
;;     (when (buffer-file-name)
;;       (save-buffer))

;;     ;; get a latex file and extract the references
;;     (org-latex-export-to-latex)
;;     (find-file texfile)
;;     (reftex-parse-all)
;;     (reftex-create-bibtex-file bibfile)
;;     (when (buffer-file-name)
;;       (save-buffer))
;;     ;; save results of the references
;;     (setq results (buffer-string))

;;     ;; kill buffers. these are named by basename, not full path
;;     (kill-buffer (concat basename ".bib"))
;;     (kill-buffer (concat basename ".tex"))
;;     (kill-buffer basename)

;;     (delete-file bibfile)
;;     (delete-file texfile)
;;     (delete-file tempname)

;;     ;; Now back to the original org buffer and insert the results
;;     (switch-to-buffer cb)
;;     (when (not (string= "" results))
;;       (save-excursion
;;         (goto-char (point-max))
;;         (insert "\n\n")
;;         (org-insert-heading)
;;         (insert (format " Bibtex entries

;; #+BEGIN_SRC text :tangle %s
;; %s
;; #+END_SRC" (concat (file-name-sans-extension
;;		    (file-name-nondirectory
;;		     (buffer-file-name))) ".bib") results))))))

(defun org-ref-extract-bibtex-entries ()
  "Extract the bibtex entries in the current buffer into a src block."
  (interactive)
  (let ((bibtex-files (org-ref-find-bibliography))
	(bibtex-entry-kill-ring '()))

    (save-window-excursion
      (cl-loop for key in (reverse (org-ref-get-bibtex-keys))
	       do
	       (bibtex-search-entry key t)
	       (bibtex-kill-entry t)))


    (goto-char (point-max))
    (insert "\n\n")
    (org-insert-heading)
    (insert (format " Bibtex entries

#+BEGIN_SRC text
%s
#+END_SRC"
		    (mapconcat
		     'identity
		     bibtex-entry-kill-ring
		     "\n\n")))))


;;** Find bad citations
(defun org-ref-list-index (substring list)
  "Return the index of SUBSTRING in a LIST of strings."
  (let ((i 0)
        (found nil))
    (dolist (arg list i)
      (if (string-match (concat "^" substring "$") arg)
          (progn
            (setq found t)
            (cl-return i)))
      (setq i (+ i 1)))
    ;; return counter if found, otherwise return nil
    (if found i nil)))


;;;###autoload
(defun org-ref-find-bad-citations ()
  "Create a list of citation keys that do not have a matching bibtex entry.
List is displayed in an `org-mode' buffer using the known bibtex
file.  Makes a new buffer with clickable links."
  (interactive)
  ;; generate the list of bibtex-keys and cited keys
  (let* ((bibtex-files (mapcar
			'file-name-nondirectory
			(org-ref-find-bibliography)))
         (bibtex-file-path (mapconcat
			    (lambda (x)
			      (file-name-directory (file-truename x)))
			    (org-ref-find-bibliography)
			    ":"))
         (bibtex-keys (mapcar (lambda (x)
				(car x))
			      (bibtex-global-key-alist)))
         (bad-citations '()))

    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (let ((plist (nth 1 link)))
          (when (-contains? org-ref-cite-types
			    (plist-get plist :type))
            (dolist (key (org-ref-split-and-strip-string
			  (plist-get plist :path)))
              (when (not (org-ref-list-index key bibtex-keys))
                (setq
                 bad-citations
                 (append
                  bad-citations
                  `(,(format "%s [[elisp:(progn (switch-to-buffer-other-frame \"%s\")(goto-char %s))][not found here]]\n"
                             key
                             (buffer-name)
                             (plist-get plist :begin))))))))))
      ;; set with-affilates to t to get citations in a caption
      nil nil nil t)

    (if bad-citations
        (progn
          (switch-to-buffer-other-window "*Missing citations*")
          (org-mode)
          (erase-buffer)
          (insert "* List of bad cite links\n")
          (insert (mapconcat 'identity bad-citations ""))
          (use-local-map (copy-keymap org-mode-map))
          (local-set-key "q" #'(lambda () (interactive) (kill-buffer))))

      (when (get-buffer "*Missing citations*")
        (kill-buffer "*Missing citations*"))
      (message "No bad cite links found"))))

;;** bad citations, labels, refs and files in orgfile
(defun org-ref-bad-cite-candidates ()
  "Return a list of conses (key . marker) where key does not exist in the known bibliography files, and marker points to the key."
  (let* ((cp (point))			; save to return to later
         (bibtex-files (cl-loop for f in (org-ref-find-bibliography)
				if (file-exists-p f)
				collect f))
         (bibtex-file-path (mapconcat
                            (lambda (x)
                              (file-name-directory (file-truename x)))
                            bibtex-files ":"))
         (bibtex-keys (mapcar (lambda (x) (car x))
                              (bibtex-global-key-alist)))
         (bad-citations '()))

    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (let ((plist (nth 1 link)))
          (when (-contains? org-ref-cite-types (plist-get plist :type))
            (dolist (key (org-ref-split-and-strip-string
			  (plist-get plist :path)))
              (when (not (org-ref-list-index key bibtex-keys))
                (goto-char (plist-get plist :begin))
                (re-search-forward key)
                (push (cons key (point-marker)) bad-citations))))))
      ;; add with-affiliates to get cites in caption
      nil nil nil t)
    (goto-char cp)
    bad-citations))


(defun org-ref-bad-ref-candidates ()
  "Return a list of conses (ref . marker) where ref is a ref link that does not point to anything (i.e. a label)."
  ;; first get a list of legitimate labels
  (let ((cp (point))
        (labels (org-ref-get-labels))
        (bad-refs '()))
    ;; now loop over ref links
    (goto-char (point-min))
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (let ((plist (nth 1 link)))
          (when (or  (equal (plist-get plist ':type) "ref")
                     (equal (plist-get plist ':type) "eqref")
                     (equal (plist-get plist ':type) "pageref")
                     (equal (plist-get plist ':type) "nameref"))
            (unless (-contains? labels (plist-get plist :path))
              (goto-char (plist-get plist :begin))
              (add-to-list
               'bad-refs
               (cons (plist-get plist :path)
                     (point-marker))))))))
    (goto-char cp)
    bad-refs))


(defun org-ref-bad-label-candidates ()
  "Return a list of labels where label is multiply defined."
  (let ((labels (org-ref-get-labels))
        (multiple-labels '()))
    (when (not (= (length labels)
                  (length (-uniq labels))))
      (dolist (label labels)
        (when (> (-count (lambda (a)
                           (equal a label))
                         labels)
		 1)
          ;; this means there are multiply defined labels. now we find them.
          (let ((cp (point)))
            (goto-char (point-min))
	    ;; regular org label:tag links
            (while (re-search-forward
                    (format  "[^#+]label:%s\\s-" label) nil t)
              (cl-pushnew (cons label (point-marker)) multiple-labels
			  :test (lambda (a b)
				  (and (string= (car a) (car b))
				       (= (marker-position (cdr a))
					  (marker-position (cdr b)))))))

            (goto-char (point-min))
	    ;; latex style
            (while (re-search-forward
                    (format  "\\label{%s}\\s-?" label) nil t)
              (cl-pushnew (cons label (point-marker)) multiple-labels
			  :test (lambda (a b)
				  (and (string= (car a) (car b))
				       (= (marker-position (cdr a))
					  (marker-position (cdr b)))))))

	    ;; keyword style
            (goto-char (point-min))
            (while (re-search-forward
                    (format  "^#\\+label:\\s-*%s" label) nil t)
              (cl-pushnew (cons label (point-marker)) multiple-labels
			  :test (lambda (a b)
				  (and (string= (car a) (car b))
				       (= (marker-position (cdr a))
					  (marker-position (cdr b)))))))

            (goto-char (point-min))
            (while (re-search-forward
                    (format   "^#\\+tblname:\\s-*%s" label) nil t)
              (cl-pushnew (cons label (point-marker)) multiple-labels
			  :test (lambda (a b)
				  (and (string= (car a) (car b))
				       (= (marker-position (cdr a))
					  (marker-position (cdr b)))))))
            (goto-char cp)))))
    multiple-labels))


(defun org-ref-bad-file-link-candidates ()
  "Return list of conses (link . marker) where the file in the link does not exist."
  (let* ((bad-files '()))
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (let ((type (org-element-property :type link)))
          (when (or  (string= "file" type)
                     (string= "attachfile" type))
            (unless (file-exists-p (org-element-property :path link))
              (add-to-list 'bad-files
                           (cons (org-element-property :path link)
                                 (save-excursion
                                   (goto-char
                                    (org-element-property :begin link))
                                   (point-marker)))))))))
    ;; Let us also check \attachfile{fname}
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\attachfile{\\([^}]*\\)}" nil t)
        (unless (file-exists-p (match-string 1))
          (add-to-list 'bad-files (cons (match-string 1) (point-marker))))))
    bad-files))



;;** Find non-ascii charaters
;;;###autoload
(defun org-ref-find-non-ascii-characters ()
  "Find non-ascii characters in the buffer.  Useful for cleaning up bibtex files."
  (interactive)
  (occur "[^[:ascii:]]"))


;;** Sort fields in a bibtex entry
;;;###autoload
(defun org-ref-sort-bibtex-entry ()
  "Sort fields of entry in standard order."
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
         (entry-fields)
         (other-fields)
         (type (cdr (assoc "=type=" entry)))
         (key (cdr (assoc "=key=" entry)))
	 (field-order (cdr (assoc type org-ref-bibtex-sort-order))))

    ;; these are the fields we want to order that are in this entry
    (setq entry-fields (mapcar (lambda (x) (car x)) entry))
    ;; we do not want to reenter these fields
    (setq entry-fields (remove "=key=" entry-fields))
    (setq entry-fields (remove "=type=" entry-fields))

    ;;these are the other fields in the entry, and we sort them alphabetically.
    (setq other-fields
	  (sort (-remove (lambda(x) (member x field-order)) entry-fields)
		'string<))

    (bibtex-kill-entry)
    (insert
     (concat "@" type "{" key ",\n"
	     (mapconcat
	      (lambda (field)
		(when (member field entry-fields)
		  (format "%s = %s,"
			  field
			  (cdr (assoc field entry))))) field-order "\n")
	     (mapconcat
	      (lambda (field)
		(format "%s = %s,"
			field
			(cdr (assoc field entry))))
	      other-fields "\n")
	     "\n}\n\n"))
    (bibtex-find-entry key)
    (bibtex-fill-entry)
    (bibtex-clean-entry)))

;; downcase entries
;;;###autoload
(defun org-ref-downcase-bibtex-entry ()
  "Downcase the entry type and fields."
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
         (entry-fields)
         (type (downcase (cdr (assoc "=type=" entry))))
         (key (cdr (assoc "=key=" entry))))

    (setq entry-fields (mapcar (lambda (x) (car x)) entry))
    ;; we do not want to reenter these fields
    (setq entry-fields (remove "=key=" entry-fields))
    (setq entry-fields (remove "=type=" entry-fields))

    (bibtex-kill-entry)
    (insert
     (concat "@" (downcase type) "{" key ",\n"
	     (mapconcat
	      (lambda (field)
		(format "%s = %s,"
			(downcase field)
			(cdr (assoc field entry))))
	      entry-fields "\n")
	     "\n}\n\n"))
    (bibtex-find-entry key)
    (bibtex-fill-entry)
    (bibtex-clean-entry)))


;;** Clean a bibtex entry
;; These functions operate on a bibtex entry and "clean" it in some way.
(defun orcb-clean-doi ()
  "Remove http://dx.doi.org/ in the doi field."
  (let ((doi (bibtex-autokey-get-field "doi")))
    (when (string-match "^http://dx.doi.org/" doi)
      (bibtex-beginning-of-entry)
      (goto-char (car (cdr (bibtex-search-forward-field "doi" t))))
      (bibtex-kill-field)
      (bibtex-make-field "doi")
      (backward-char)
      (insert (replace-regexp-in-string "^http://dx.doi.org/" "" doi)))))


(defun orcb-clean-year (&optional new-year)
  "Fix years set to 0.
If optional NEW-YEAR set it to that, otherwise prompt for it."
  ;; asap articles often set year to 0, which messes up key
  ;; generation. fix that.
  (let ((year (bibtex-autokey-get-field "year")))
    (when (string= "0" year)
      (bibtex-beginning-of-entry)
      (goto-char (car (cdr (bibtex-search-forward-field "year" t))))
      (bibtex-kill-field)
      (bibtex-make-field "year")
      (backward-char)
      (insert (or new-year (read-string "Enter year: "))))))


(defun orcb-clean-pages ()
  "Check for empty pages, and put eid in its place if it exists."
  (let ((pages (bibtex-autokey-get-field "pages"))
	(eid (bibtex-autokey-get-field "eid")))
    (when (and (not (string= "" eid))
	       (or (string= "" pages)))
      (bibtex-set-field "pages" eid))))


(defun orcb-& ()
  "Replace naked & with \& in a bibtex entry."
  (save-restriction
    (bibtex-narrow-to-entry)
    (bibtex-beginning-of-entry)
    (while (re-search-forward " & " nil t)
      (replace-match " \\\\& "))))


(defun orcb-% ()
  "Replace naked % with % in a bibtex entry."
  (save-restriction
    (bibtex-narrow-to-entry)
    (bibtex-beginning-of-entry)
    (while (re-search-forward "%" nil t)
      (replace-match " \\\\%"))))


(defun orcb-key-comma ()
  "Make sure there is a comma at the end of the first line."
  (bibtex-beginning-of-entry)
  (end-of-line)
  ;; some entries do not have a key or comma in first line. We check and add it,
  ;; if needed.
  (unless (string-match ", *$" (thing-at-point 'line))
    (end-of-line)
    (insert ",")))


(defun orcb-key ()
  "Replace the key in the entry."
  (let ((key (funcall org-ref-clean-bibtex-key-function
		      (bibtex-generate-autokey))))
    ;; first we delete the existing key
    (bibtex-beginning-of-entry)
    (re-search-forward bibtex-entry-maybe-empty-head)
    (if (match-beginning bibtex-key-in-head)
	(delete-region (match-beginning bibtex-key-in-head)
		       (match-end bibtex-key-in-head)))
    ;; check if the key is in the buffer
    (when (save-excursion
	    (bibtex-search-entry key))
      (save-excursion
	(bibtex-search-entry key)
	(bibtex-copy-entry-as-kill)
	(switch-to-buffer-other-window "*duplicate entry*")
	(bibtex-yank))
      (setq key (bibtex-read-key "Duplicate Key found, edit: " key)))

    (insert key)
    (kill-new key)))


;;;###autoload
(defun org-ref-clean-bibtex-entry ()
  "Clean and replace the key in a bibtex entry.
See functions in `org-ref-clean-bibtex-entry-hook'."
  (interactive)
  (bibtex-beginning-of-entry)
  ;; run hooks. each of these operates on the entry with no arguments.
  ;; this did not work like  i thought, it gives a symbolp error.
  ;; (run-hooks org-ref-clean-bibtex-entry-hook)
  (mapc (lambda (x)
	  (save-restriction
	    (save-excursion
	      (funcall x))))
	org-ref-clean-bibtex-entry-hook))


(defun org-ref-get-citation-year (key)
  "Get the year of an entry with KEY.  Return year as a string."
  (let* ((results (org-ref-get-bibtex-key-and-file key))
         (bibfile (cdr results)))
    (with-temp-buffer
      (insert-file-contents bibfile)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (bibtex-search-entry key nil 0)
      (prog1 (reftex-get-bib-field "year" (bibtex-parse-entry t))))))

;;** Sort cite in cite link
;;;###autoload
(defun org-ref-sort-citation-link ()
  "Replace link at point with sorted link by year."
  (interactive)
  (let* ((object (org-element-context))
         (type (org-element-property :type object))
         (begin (org-element-property :begin object))
         (end (org-element-property :end object))
         (link-string (org-element-property :path object))
         keys years data)
    (setq keys (org-ref-split-and-strip-string link-string))
    (setq years (mapcar 'org-ref-get-citation-year keys))
    (setq data (-zip-with 'cons years keys))
    (setq data (cl-sort data (lambda (x y) (< (string-to-number (car x)) (string-to-number (car y))))))
    ;; now get the keys separated by commas
    (setq keys (mapconcat (lambda (x) (cdr x)) data ","))
    ;; and replace the link with the sorted keys
    (cl--set-buffer-substring begin end (concat type ":" keys))))

;;** Shift-arrow sorting of keys in a cite link
(defun org-ref-swap-keys (i j keys)
  "Swap index I and J in the list KEYS."
  (let ((tempi (nth i keys)))
    (setf (nth i keys) (nth j keys))
    (setf (nth j keys) tempi))
  keys)


;;;###autoload
(defun org-ref-swap-citation-link (direction)
  "Move citation at point in DIRECTION +1 is to the right, -1 to the left."
  (interactive)
  (let* ((object (org-element-context))
         (type (org-element-property :type object))
         (begin (org-element-property :begin object))
         (end (org-element-property :end object))
         (link-string (org-element-property :path object))
         key keys i)
    ;;   We only want this to work on citation links
    (when (-contains? org-ref-cite-types type)
      (setq key (org-ref-get-bibtex-key-under-cursor))
      (setq keys (org-ref-split-and-strip-string link-string))
      (setq i (org-ref-list-index key keys))  ;; defined in org-ref
      (if (> direction 0) ;; shift right
          (org-ref-swap-keys i (+ i 1) keys)
        (org-ref-swap-keys i (- i 1) keys))
      (setq keys (mapconcat 'identity keys ","))
      ;; and replace the link with the sorted keys
      (cl--set-buffer-substring
       begin end
       (concat
        type ":" keys
        ;; It seems the space at the end can get consumed, so we see if there
        ;; is a space, and add it if so. Sometimes there is a comma or period,
        ;; then we do not want a space.
        (when
            (save-excursion
              (goto-char end)
              (char-equal (char-before) (string-to-char " ")))
          " ")))
      ;; now go forward to key so we can move with the key
      (re-search-forward key)
      (goto-char (match-beginning 0)))))

;; add hooks to make it work
(add-hook 'org-shiftright-hook (lambda () (org-ref-swap-citation-link 1)))
(add-hook 'org-shiftleft-hook (lambda () (org-ref-swap-citation-link -1)))

;;** C-arrow navigation of cite keys
(defun org-ref-parse-cite ()
  "Parse link to get cite keys, and start and end of the keys."
  (interactive)
  (let ((link (org-element-context))
	path begin end
	keys)

    (unless (-contains? org-ref-cite-types
			(org-element-property :type link))
      (error "Not on a cite link"))
    (setq path (org-element-property :path link)
	  begin	  (org-element-property :begin link)
	  end (org-element-property :end link))

    (setq keys (org-ref-split-and-strip-string path))
    (save-excursion
      (loop for key in keys
	    do
	    (goto-char begin)
	    (re-search-forward key end)
	    collect
	    (list key (match-beginning 0) (match-end 0))))))

;;;###autoload
(defun org-ref-next-key ()
  "Move cursor to the next cite key when on a cite link.
Otherwise run `right-word'. If the cursor moves off the link,
move to the beginning of the next cite link after this one."
  (interactive)
  (if (-contains? org-ref-cite-types
		  (org-element-property :type (org-element-context)))
      ;; We are on a link, go to next key or cite link
      (let ((cps (org-ref-parse-cite))
	    (p (point)))
	(cond
	 ;; point is before first key
	 ((< (point) (nth 1 (car cps)))
	  (goto-char (nth 1 (car cps))))
	 ;; point is on a single key, or on the last key
	 ((or (= 1 (length cps))
	      (> p (nth 1 (car (last cps)))))
	  (re-search-forward org-ref-cite-re nil t)
	  (goto-char (match-end 1))
	  (forward-char 1))
	 ;; in a link with multiple keys. We need to figure out if there is a
	 ;; next key and go to beginning
	 (t
	  (goto-char (min
		      (point-max)
		      (+ 1
			 (loop for (k s e) in cps
			       if (and (>= p s)
				       (<= p e))
			       return e))))))
	;; if we get off a link,jump to the next one.
	(when
	    (not (-contains? org-ref-cite-types
			     (org-element-property
			      :type
			      (org-element-context))))
	  (when  (re-search-forward org-ref-cite-re nil t)
	    (goto-char (match-beginning 0))
	    (re-search-forward ":"))))
    (right-word)))


;;;###autoload
(defun org-ref-previous-key ()
  "Move cursor to the previous cite key when on a cite link.
Otherwise run `left-word'. If the cursor moves off the link,
move to the beginning of the previous cite link after this one."
  (interactive)
  (if (-contains? org-ref-cite-types
		  (org-element-property :type (org-element-context)))
      ;; We are on a link, go to next key or cite link
      (let ((cps (org-ref-parse-cite))
	    (p (point))
	    index)
	(cond
	 ;; point is on or before first key, go to previous link.
	 ((<= (point) (nth 1 (car cps)))
	  (unless (re-search-backward org-ref-cite-re nil t)
	    (left-word))
	  (when (re-search-backward org-ref-cite-re nil t)
	    (goto-char (match-end 0))
	    (re-search-backward ",\\|:")
	    (forward-char)))
	 ;; point is less than end of first key, goto beginning
	 ((< p (nth 2 (car cps)))
	  ;; we do this twice. the first one just goes to the beginning of the
	  ;; current link
	  (goto-char (nth 1 (car cps))))
	 ;; in a link with multiple keys. We need to figure out if there is a
	 ;; previous key and go to beginning
	 (t
	  (setq index (loop
		       for i from 0
		       for (k s e) in cps
		       if (and (>= p s)
			       (<= p e))
		       return i))
	  (goto-char (nth 1 (nth (- index 1) cps))))))
    (left-word)))


(define-key org-mode-map (kbd "<C-right>") 'org-ref-next-key)
(define-key org-mode-map (kbd "<C-left>") 'org-ref-previous-key)

;;** context around org-ref links
(defun org-ref-get-label-context (label)
  "Return a string of context around a LABEL."
  (save-excursion
    (catch 'result
      (goto-char (point-min))
      (when (re-search-forward
             (format "label:%s\\b" label) nil t)
        (throw 'result (buffer-substring
                        (progn
                          (forward-line -1)
                          (beginning-of-line)
                          (point))
                        (progn
                          (forward-line 4)
                          (point)))))

      (goto-char (point-min))
      (when (re-search-forward
             (format "\\label{%s}" label) nil t)
        (throw 'result (buffer-substring
                        (progn
                          (forward-line -1)
                          (beginning-of-line)
                          (point))
                        (progn
                          (forward-line 4)
                          (point)))))

      (goto-char (point-min))
      (when (re-search-forward
             (format "^#\\+label:\\s-*\\(%s\\)\\b" label) nil t)
        (throw 'result (buffer-substring
                        (progn
                          (forward-line -1)
                          (beginning-of-line)
                          (point))
                        (progn
                          (forward-line 4)
                          (point)))))

      (goto-char (point-min))
      (when (re-search-forward
             (format "^#\\+tblname:\\s-*\\(%s\\)\\b" label) nil t)
        (throw 'result (buffer-substring
                        (progn
                          (forward-line -1)
                          (beginning-of-line)
                          (point))
                        (progn
                          (forward-line 4)
                          (point)))))

      (goto-char (point-min))
      (when (re-search-forward
             (format "^#\\+name:\\s-*\\(%s\\)\\b" label) nil t)
        (throw 'result (buffer-substring
                        (progn
                          (forward-line -1)
                          (beginning-of-line)
                          (point))
                        (progn
                          (forward-line 4)
                          (point)))))
      ;; ;; CUSTOM_ID
      (goto-char (point-min))
      ;; do we have a CUSTOM-ID?
      (let ((heading (org-map-entries
		      (lambda ()
			(buffer-substring
			 (progn
			   (forward-line -1)
			   (beginning-of-line)
			   (point))
			 (progn
			   (forward-line 4)
			   (point))))
		      (format  "CUSTOM_ID=\"%s\"" label))))
	;; (message-box heading)
	(when heading
	  (throw 'result (car heading))))


      (throw 'result "!!! NO CONTEXT FOUND !!!"))))


;;;###autoload
(defun org-ref-link-message ()
  "Print a minibuffer message about the link that point is on."
  (interactive)
  ;; the way links are recognized in org-element-context counts blank spaces
  ;; after a link and the closing brackets in literal links. We don't try to get
  ;; a message if the cursor is on those, or if it is on a blank line.
  (when (not (or (looking-at " ")	;looking at a space
		 (looking-at "^$")	;looking at a blank line
		 (looking-at "]")	;looking at a bracket at the end
		 (looking-at "$"	;looking at the end of the line.
			     )))

    (save-restriction
      (widen)
      (when (eq major-mode 'org-mode)
        (let* ((object (org-element-context))
               (type (org-element-property :type object)))
          (save-excursion
            (cond
             ;; cite links
             ((-contains? org-ref-cite-types type)
              (message (org-ref-get-citation-string-at-point)))

             ;; message some context about the label we are referring to
             ((string= type "ref")
              (message "%scount: %s"
                       (org-ref-get-label-context
                        (org-element-property :path object))
                       (org-ref-count-labels
                        (org-element-property :path object))))

             ((string= type "eqref")
              (message "%scount: %s"
                       (org-ref-get-label-context
                        (org-element-property :path object))
                       (org-ref-count-labels
                        (org-element-property :path object))))

             ;; message the count
             ((string= type "label")
              (let ((count (org-ref-count-labels
                            (org-element-property :path object))))
                ;; get plurality on occurrence correct
                (message (concat
                          (number-to-string count)
                          " occurrence"
                          (when (or (= count 0)
                                    (> count 1))
                            "s")))))

             ((string= type "custom-id")
              (save-excursion
                (org-open-link-from-string
                 (format "[[#%s]]" (org-element-property :path object)))
                (message "%s" (org-get-heading))))

             ;; check if the bibliography files exist.
             ((string= type "bibliography")
              (let* ((bibfile)
                     ;; object is the link you clicked on
                     (object (org-element-context))
                     (link-string (org-element-property :path object))
                     (link-string-beginning)
                     (link-string-end))
                (save-excursion
                  (goto-char (org-element-property :begin object))
                  (search-forward link-string nil nil 1)
                  (setq link-string-beginning (match-beginning 0))
                  (setq link-string-end (match-end 0)))

                ;; make sure we are in link and not before the :
                (when (> link-string-beginning (point))
                  (goto-char link-string-beginning))

                (let (key-beginning key-end)
                  ;; now if we have comma separated bibliographies
                  ;; we find the one clicked on. we want to
                  ;; search forward to next comma from point
                  (save-excursion
                    (if (search-forward "," link-string-end 1 1)
                        (setq key-end (- (match-end 0) 1)) ; we found a match
                      (setq key-end (point)))) ; no comma found so take the point

                  ;; and backward to previous comma from point
                  (save-excursion
                    (if (search-backward "," link-string-beginning 1 1)
                        (setq key-beginning (+ (match-beginning 0) 1)) ; we found a match
                      (setq key-beginning (point)))) ; no match found
                  ;; save the key we clicked on.
                  (setq bibfile
                        (org-ref-strip-string
                         (buffer-substring key-beginning key-end)))
                  (if (file-exists-p bibfile)
                      (message "%s exists." bibfile)
                    (message "!!! %s NOT FOUND !!!" bibfile))))))))))))

;;** aliases
(defalias 'oro 'org-ref-open-citation-at-point)
(defalias 'orc 'org-ref-citation-at-point)
(defalias 'orp 'org-ref-open-pdf-at-point)
(defalias 'oru 'org-ref-open-url-at-point)
(defalias 'orn 'org-ref-open-notes-at-point)


(defalias 'orib 'org-ref-insert-bibliography-link)
(defalias 'oric 'org-ref-insert-cite-link)
(defalias 'orir 'org-ref-insert-ref-link)
(defalias 'orsl 'org-ref-store-bibtex-entry-link)

(defalias 'orcb 'org-ref-clean-bibtex-entry)


(defun org-ref-get-citation-string-at-point (&optional key)
  "Get a string of a formatted citation for the KEY.
If no KEY is provided, get the KEY at point."
  (let* ((results (org-ref-get-bibtex-key-and-file key))
         (key (car results))
         (bibfile (cdr results)))
    (if bibfile
        (save-excursion
          (with-temp-buffer
            (insert-file-contents bibfile)
            (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
            (bibtex-search-entry key)
            (org-ref-bib-citation)))
      "!!! No entry found !!!" )))


(defun org-ref-delete-cite-at-point ()
  "Delete the citation link at point."
  (let* ((cite (org-element-context))
	 (type (org-element-property :type cite)))
    (when (-contains? org-ref-cite-types type)
      (cl--set-buffer-substring
       (org-element-property :begin cite)
       (org-element-property :end cite)
       ""))))


(defun org-ref-delete-key-at-point ()
  "Delete the key at point."
  (save-excursion
    (let* ((cite (org-element-context))
	   (path (org-element-property :path cite))
	   (keys (org-ref-split-and-strip-string path))
	   (key (org-ref-get-bibtex-key-under-cursor))
	   (begin (org-element-property :begin cite))
	   (end (org-element-property :end cite))
	   (type (org-element-property :type cite))
	   (bracketp (string= "[[" (buffer-substring begin (+ 2 begin))))
	   (trailing-space (if (save-excursion
				 (goto-char end)
				 (string= (string (preceding-char)) " "))
			       " " "")))

      (setq keys (-remove-item key keys))
      (setf (buffer-substring begin end)
	    (concat
	     (when bracketp "[[")
	     type ":" (mapconcat 'identity keys ",")
	     (when bracketp "]]")
	     trailing-space)))))


(defun org-ref-insert-key-at-point (keys)
  "Insert KEYS at point.
KEYS is a list of bibtex keys. If point is at : or earlier,
insert at the beginning. Otherwise, insert after the key at
point. Leaves point at end of added keys."
  (interactive
   (list
    (funcall org-ref-cite-completion-function)))
  (let* ((cite (org-element-context))
	 (type (org-element-property :type cite))
	 (p (point))
	 begin end
	 opath
	 okey okeys
	 ikey
	 bracket-p
	 trailing-space
	 newkeys
	 new-cite)

    (cond
     ;; on a link, and before the keys. Insert keys at the beginning.
     ((and (-contains? org-ref-cite-types type)
	   (< (point) (+ (org-element-property :begin cite)
			 (length type) 1)))
      (setq
       begin (org-element-property :begin cite)
       end (org-element-property :end cite)
       opath (org-element-property :path cite)
       okeys (org-ref-split-and-strip-string opath)
       newkeys (append keys okeys)
       new-cite (concat
		 (when bracket-p "[[")
		 type
		 ":"
		 (mapconcat 'identity newkeys ",")
		 (when bracket-p "]]")
		 trailing-space)))

     ;; on a link, stick new keys after current key
     ((or (-contains? org-ref-cite-types type)
	  (and (not (bobp))
	       (save-excursion
		 (forward-char -1)
		 (-contains?
		  org-ref-cite-types
		  (org-element-property :type (org-element-context))))))

      ;; we are after a cite. get back on it
      (when (save-excursion
	      (forward-char -1)
	      (-contains?
	       org-ref-cite-types
	       (org-element-property :type (org-element-context))))
	(forward-char -1))

      (setq
       cite (org-element-context)
       type (org-element-property :type cite)
       begin (org-element-property :begin cite)
       end (org-element-property :end cite)
       opath (org-element-property :path cite)
       okeys (org-ref-split-and-strip-string opath)
       okey (org-ref-get-bibtex-key-under-cursor)
       ikey (org-ref-list-index okey okeys)
       bracket-p (string= "[" (buffer-substring begin (+ 1 begin)))
       trailing-space (if (save-excursion
			    (goto-char end)
			    (string= (string (preceding-char)) " "))
			  " " "")
       newkeys (-flatten (-insert-at (+ 1 ikey) keys okeys))
       new-cite (concat
		 (when bracket-p "[[")
		 type
		 ":"
		 (mapconcat 'identity newkeys ",")
		 (when bracket-p "]]")
		 trailing-space)))
     ;; a new cite
     (t
      (setq
       begin (point)
       end (point)
       type org-ref-default-citation-link
       newkeys keys
       new-cite (concat
		 (when bracket-p "[[")
		 type
		 ":"
		 (mapconcat 'identity newkeys ",")
		 (when bracket-p "]]")
		 trailing-space))))

    (delete-region begin end)
    (goto-char begin)
    (insert new-cite)
    (goto-char begin)
    (re-search-forward (mapconcat 'identity keys ","))))


(defun org-ref-replace-key-at-point (&optional replacement-keys)
  "Replace the key at point.
Optional REPLACEMENT-KEYS should be a string of comma-separated
keys. if it is not specified, find keys interactively."
  (save-excursion
    (let* ((cite (org-element-context))
	   (opath (org-element-property :path cite))
	   (okeys (org-ref-split-and-strip-string opath))
	   (okey (org-ref-get-bibtex-key-under-cursor))
	   (end (org-element-property :end cite)))
      ;; First, insert new keys at end
      (save-excursion
	(goto-char end)
	(skip-chars-backward " ")
	(if replacement-keys
	    (insert (format ",%s" replacement-keys))
	  (funcall org-ref-insert-cite-function)))

      ;; Now get the new keys, delete the old one and put the new ones in
      (let* ((cite (org-element-context))
	     (type (org-element-property :type cite))
	     (path (org-element-property :path cite))
	     (keys (org-ref-split-and-strip-string path))
	     (new-keys (-difference keys okeys))
	     (key (org-ref-get-bibtex-key-under-cursor))
	     (begin (org-element-property :begin cite))
	     (end (org-element-property :end cite))
	     (bracketp (string= "[[" (buffer-substring begin (+ 2 begin))))
	     (trailing-space (if (save-excursion
				   (goto-char end)
				   (string= (string (preceding-char)) " "))
				 " " ""))
	     (index (org-ref-list-index key keys)))
	;; keys here has the old key at index, and the new keys at the end.
	;; delete old key
	(setq keys (-remove-at index keys))
	(dolist (nkey (reverse new-keys))
	  (setq keys (-insert-at index nkey keys)))

	;; now remove off the end keys which are now duplicated.
	(setq keys (nbutlast keys (length new-keys)))

	(setf (buffer-substring begin end)
	      (concat
	       (when bracketp "[[")
	       type ":" (mapconcat 'identity keys ",")
	       (when bracketp "]]")
	       trailing-space))))))


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

;;* Make bibliography links at end not get folded so you can see where they are..
;; Adapted from http://stackoverflow.com/questions/9134956/avoid-printbibliography-being-swallowed-by-org-mode-headings
(defvar org-ref-biblink-re "^\\(bibliography\\(style\\)?\\|\\(printbibliography\\)\\):.*"
  "Regex for bibliography links used for showing biblinks at the end of the buffer.")

;; (defun org-ref-show-biblinks (&optional _)
;;   "Flag a region containing biblinks so they do not get folded.
;; We assume these are at the end of the buffer, and do not look
;; past the last headline."
;;   (save-excursion
;;     (goto-char (point-max))
;;     (when (re-search-backward org-ref-biblink-re nil t)
;;       (outline-flag-region (1- (point)) (point-max) nil))))

;; (add-hook 'org-cycle-hook 'org-ref-show-biblinks)
;; (add-hook 'org-occur-hook 'org-ref-show-biblinks)

;; (remove-hook 'org-cycle-hook 'org-ref-show-biblinks)
;; (remove-hook 'org-occur-hook 'org-ref-show-biblinks)

;; This seems to redefine where the end of subtree is. We do not do this in an
;; export though.
;; (defadvice org-end-of-subtree (after always-show-org-footer
;;                                      ()
;;                                      activate)
;;   (unless org-export-current-backend
;;     (when (>= (point) (1- (point-max)))
;;       (re-search-backward org-ref-biblink-re nil t)
;;       (setq ad-return-value (point)))))

;; (ad-remove-advice 'org-end-of-subtree  'after 'always-show-org-footer)

;;* The end
(provide 'org-ref)

;;; org-ref.el ends here
