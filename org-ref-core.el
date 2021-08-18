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
(require 'reftex-cite)

(defvar org-export-exclude-tags)
(defvar warning-suppress-types)
(declare-function bibtex-completion-get-entry "bibtex-completion")
(declare-function bibtex-completion-edit-notes "bibtex-completion")


;;* Custom variables
(defgroup org-ref nil
  "Customization group for org-ref."
  :tag "Org Ref"
  :group 'org)


(defcustom org-ref-default-bibliography
  nil
  "List of bibtex files to search for.
You should use full-paths for each file. Note that you must
include a bibliography link in your document if you will be
exporting it to pdf; org-ref-default-bibliography is not
used by the LaTeX exporter."
  :type '(repeat :tag "List of bibtex files" file)
  :group 'org-ref)


(defcustom org-ref-pdf-directory
  nil
  "Directory where pdfs are stored by key.
Put a trailing / in the name."
  :type '(choice directory (repeat directory))
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
  'org-ref-ivy-cite
  "Symbol for library to define completion functions.
The completion library should provide functions for
`org-ref-insert-link-function', `org-ref-insert-cite-function',
`org-ref-insert-label-function', `org-ref-insert-ref-function',
and `org-ref-cite-onclick-function', and set those variables to
the values of those functions."
  :type 'symbol
  :options '(org-ref-ivy-cite		; completion with ivy
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


(defcustom org-ref-prefer-bracket-links nil
  "If non-nil use bracketed links when inserting them."
  :type 'boolean
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


;; define key for inserting citations
(define-key org-mode-map
  (kbd org-ref-insert-cite-key)
  org-ref-insert-link-function)


(defcustom org-ref-cite-keymap
  (let ((map (copy-keymap org-mouse-map)))
    (define-key map (kbd "H-o") 'org-ref-cite-hydra/body)
    (define-key map (kbd "H-b") 'org-ref-open-citation-at-point)
    (define-key map (kbd "H-u") 'org-ref-open-url-at-point)
    (define-key map (kbd "H-p") (lambda ()
				  (interactive)
				  (funcall org-ref-open-pdf-function)))
    (define-key map (kbd "H-n") 'org-ref-open-notes-at-point)
    (define-key map (kbd "H-r") 'org-ref-wos-related-at-point)
    (define-key map (kbd "H-c") 'org-ref-wos-citing-at-point)
    (define-key map (kbd "H-e") (lambda ()
				  "Email entry at point"
				  (interactive)
				  (org-ref-open-citation-at-point)
				  (org-ref-email-bibtex-entry)))
    (define-key map (kbd "H-g") 'org-ref-google-scholar-at-point)
    (define-key map (kbd "H-f") (lambda ()
				  (interactive)
				  (save-excursion
				    (org-ref-open-citation-at-point)
				    (kill-new
				     (org-ref-format-bibtex-entry-at-point)))))
    (define-key map (kbd "H-w") (lambda ()
				  (interactive)
				  (kill-new (car (org-ref-get-bibtex-key-and-file)))))
    (define-key map (kbd "H-W") (lambda ()
				  "Copy all the keys at point."
				  (interactive)
				  (kill-new (org-element-property :path (org-element-context)))))
    (define-key map (kbd "H-y") (lambda ()
				  "Paste key at point. Assumes the first thing in the killring is a key."
				  (interactive)
				  (org-ref-insert-key-at-point (car kill-ring))))

    ;; Navigation keys
    (define-key map (kbd "C-<left>") 'org-ref-previous-key)
    (define-key map (kbd "C-<right>") 'org-ref-next-key)

    ;; rearrangement keys
    (define-key map (kbd "S-<left>") (lambda () (interactive) (org-ref-swap-citation-link -1)))
    (define-key map (kbd "S-<right>") (lambda () (interactive) (org-ref-swap-citation-link 1)))
    (define-key map (kbd "S-<up>") 'org-ref-sort-citation-link)
    (define-key map (kbd "<tab>") (lambda () (interactive)
				    (funcall org-ref-insert-cite-function)))
    map)
  "Keymap for cite links."
  :type 'symbol
  :group 'org-ref)


(defcustom org-ref-bibliography-entry-format
  '(("article" . "%a, %t, <i>%j</i>, <b>%v(%n)</b>, %p (%y). <a href=\"%U\">link</a>. <a href=\"http://dx.doi.org/%D\">doi</a>.")

    ("book" . "%a, %t, %u (%y).")
    ("techreport" . "%a, %t, %i, %u (%y).")
    ("proceedings" . "%e, %t in %S, %u (%y).")
    ("inproceedings" . "%a, %t, %p, in %b, edited by %e, %u (%y)"))
  "String to format an entry.
Just the reference, no numbering at the beginning, etc... see the
`org-ref-reftex-format-citation' docstring for the escape codes."
  :type '(alist :key-type (string) :value-type (string))
  :group 'org-ref)



(defcustom org-ref-ref-html "<a class='org-ref-reference' href=\"#%s\">%s</a>"
  "HTML code to represent a reference.
Note: you can't really change this, it is used in a format later
with two arguments that are both the key. I don't know a way to
make this more flexible at the moment. It is only used in the
export of cite links right now."
  :type 'string
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
to the corresponding filename. The default is
`org-ref-get-pdf-filename'. Alternative values are
`org-ref-get-mendeley-filename' or
`org-ref-get-pdf-filename-bibtex-completion'."
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


(defcustom org-ref-natbib-types
  '("citet" "citet*" "citep" "citep*"
    "citealt" "citealt*" "citealp" "citealp*"
    "citenum" "citetext"
    "citeauthor" "citeauthor*"
    "citeyear" "citeyear*" "citeyearpar"
    "Citet" "Citep" "Citealt" "Citealp" "Citeauthor")
  "natbib cite commands, http://tug.ctan.org/macros/latex/contrib/natbib/natnotes.pdf"
  :type '(repeat :tag "List of citation types" string)
  :group 'org-ref)


(defcustom org-ref-biblatex-types
  '("Cite"
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
    "supercites" "autocites" "Autocites")
  "biblatex commands
http://ctan.mirrorcatalogs.com/macros/latex/contrib/biblatex/doc/biblatex.pdf"
  :type '(repeat :tag "List of citation types" string)
  :group 'org-ref)


(defcustom org-ref-cite-types
  (append
   '("cite" "nocite") ;; the default latex cite commands
   org-ref-natbib-types
   org-ref-biblatex-types
   ;; for the bibentry package
   '("bibentry"))
  "List of citation types known in `org-ref'."
  :type '(repeat :tag "List of citation types" string)
  :group 'org-ref)



(defcustom org-ref-clean-bibtex-entry-hook
  '(org-ref-bibtex-format-url-if-doi
    orcb-key-comma
    org-ref-replace-nonascii
    orcb-&
    orcb-%
    org-ref-title-case-article
    orcb-clean-year
    orcb-key
    orcb-clean-doi
    orcb-clean-pages
    orcb-check-journal
    org-ref-sort-bibtex-entry
    orcb-fix-spacing)
  "Hook that is run in `org-ref-clean-bibtex-entry'.
The functions should have no arguments, and
operate on the bibtex entry at point. You can assume point starts
at the beginning of the entry. These functions are wrapped in
`save-restriction' and `save-excursion' so you do not need to
save the point position.

Org ref contains some functions that are not included by default
such as `orcb-clean-nil' or `orcb-clean-nil-opinionated' that
users may be interested in adding themselves."
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
  :type '(alist :key-type (string) :value-type (repeat string))
  :group 'org-ref)





(defvar org-ref-bibliography-files
  nil
  "Variable to hold bibliography files to be searched.")


(defcustom org-ref-show-broken-links t
  "If non-nil show bad org-ref links in a warning face."
  :type 'boolean
  :group 'org-ref)


(defcustom org-ref-enable-colon-insert nil
  "If non-nil enable colon to insert cites, labels, and ref links."
  :type 'booleanp
  :group 'org-ref)








(defun org-ref-change-cite-type (new-type)
  "Change the cite type to NEW-TYPE."
  (interactive (list (completing-read "Type: " org-ref-cite-types)))
  (let* ((cite-link (org-element-context))
	 (old-type (org-element-property :type cite-link))
	 (begin (org-element-property :begin cite-link))
	 (end (org-element-property :end cite-link))
	 (bracketp (eq 'bracket (org-element-property :format cite-link)))
	 (path (org-element-property :path cite-link))
	 (deltap (- (point) begin)))
    ;; note this does not respect brackets
    (setf (buffer-substring begin end)
	  (concat
	   (if bracketp "[[" "")
	   new-type ":" path
	   (if bracketp "]]" "")))
    ;; try to preserve the character the point is on.
    (goto-char (+ begin deltap (- (length new-type) (length old-type))))))






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
            (run-with-idle-timer 0.5 t 'org-ref-link-message)
	    org-ref-show-citation-on-enter t)))


;;;###autoload
(defun org-ref-cancel-link-messages ()
  "Stop showing messages in minibuffer when on a link."
  (interactive)
  (cancel-timer org-ref-message-timer)
  (setq org-ref-message-timer nil
	org-ref-show-citation-on-enter nil))


(when org-ref-show-citation-on-enter
  (org-ref-show-link-messages))


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


(defvar org-ref-cite-re
  (concat "\\(" (mapconcat
                 (lambda (x)
		   (replace-regexp-in-string "\\*" "\\\\*" x))
                 org-ref-cite-types "\\|") "\\):"
                 "\\([a-zA-Z0-9_:\\./-]+,?\\)+")
  "Regexp for cite links.
Group 1 contains the cite type.
Group 2 contains the keys.")


(defvar org-ref-label-re
  "label:\\([a-zA-Z0-9_:-]+,?\\)+"
  "Regexp for label links.")


(defface org-ref-cite-face
  `((t (:inherit org-link
                 :foreground ,org-ref-cite-color)))
  "Color for cite-like links in org-ref.")


;;* Links

;;** cite link

(defun org-ref-get-bibtex-key-under-cursor ()
  "Return key under the cursor in org-mode.
We search forward from point to get a comma, or the end of the link,
and then backwards to get a comma, or the beginning of the link. that
delimits the keyword we clicked on. We also strip the text
properties."
  (let* ((object (org-element-context))
	 (link-string (if (eq (org-element-type object) 'link)
                          (org-element-property :path object)
                        (org-in-regexp org-link-any-re)
			;; this is clunkier than I prefer, but some keys have
			;; colons in them, and this gets rid of the link type,
			;; then rejoins the rest of the keys
			(s-join ":" (cdr (split-string
					  (match-string-no-properties 0) ":"))))))
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

      ;; link with description and multiple keys
      (if (and (org-element-property :contents-begin object)
	       (string-match "," link-string)
	       (equal (org-element-type object) 'link))
	  ;; point is not on the link description
	  (if (not (>= (point) (org-element-property :contents-begin object)))
	      (let (link-string-beginning link-string-end)
		(save-excursion
		  (goto-char (org-element-property :begin object))
		  (search-forward link-string nil t 1)
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
		    bibtex-key)))
	    ;; point is on the link description, assume we want the
	    ;; last key
	    (let ((last-key (replace-regexp-in-string "[a-zA-Z0-9_-]*," "" link-string)))
	      last-key))
	;; link with description. assume only one key
	link-string))))


(defun org-ref-find-bibliography ()
  "Find the bibliography in the buffer.
This function sets and returns cite-bibliography-files, which is
a list of files either from }, internal bibliographies, from files in the
BIBINPUTS env var, and finally falling back to what the user has
set in `org-ref-default-bibliography'"
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
        (setq org-ref-bibliography-files
              (reftex-locate-bibliography-files default-directory))
        (when org-ref-bibliography-files
          (throw 'result org-ref-bibliography-files)))


      ;; we did not find anything. use defaults
      (setq org-ref-bibliography-files org-ref-default-bibliography)))


  ;; set reftex-default-bibliography so we can search
  (set (make-local-variable 'reftex-default-bibliography) org-ref-bibliography-files)
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
    org-ref-default-bibliography)))


(defun org-ref-get-bibtex-key-and-file (&optional key)
  "Return a  a cons cell of (KEY . file) that KEY is in.
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
     ,(format "Formatting function for %s links.\n[[%s:KEYWORD][DESC]]
FORMAT is a symbol for the export backend.
Supported backends: 'html, 'latex, 'ascii, 'org, 'md, 'pandoc" type type)
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
	  (format org-ref-ref-html key key))
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

      ((eq format 'md)
       (mapconcat (lambda (key)
                    ;; this is an html link that has an anchor to jump back to,
                    ;; and links to the entry in the bibliography. Also contains
                    ;; a tooltip.
		    (format "<sup id=\"%s\"><a href=\"#%s\" title=\"%s\">%s</a></sup>"
                            ;; this makes an anchor to return to
			    (md5 key)
			    key
                            ;; awful way to get a simple tooltip... I just need
                            ;; a simple formatted string, but the default has
                            ;; too much html stuff in it, and this needs to be
                            ;; cleaned of quotes and stuff,
			    (let ((org-ref-bibliography-files (org-ref-find-bibliography))
				  (file) (entry) (bibtex-entry) (entry-type) (format)
				  (org-ref-bibliography-entry-format
				   '(("article" . "%a, %t, %j, v(%n), %p (%y).")
				     ("book" . "%a, %t, %u (%y).")
				     ("techreport" . "%a, %t, %i, %u (%y).")
				     ("proceedings" . "%e, %t in %S, %u (%y).")
				     ("inproceedings" . "%a, %t, %p, in %b, edited by %e, %u (%y)"))))
			      (setq file (catch 'result
					   (cl-loop for file in org-ref-bibliography-files do
						    (if (org-ref-key-in-file-p key (file-truename file))
							(throw 'result file)
						      (message "%s not found in %s"
							       key (file-truename file))))))
			      (if file
				  (with-temp-buffer
				    (insert-file-contents file)
				    (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
				    (bibtex-search-entry key nil 0)
				    (setq bibtex-entry (bibtex-parse-entry))
                                    ;; downcase field names so they work in the format-citation code
				    (dolist (cons-cell bibtex-entry)
				      (setf (car cons-cell) (downcase (car cons-cell))))
				    (setq entry-type (downcase (cdr (assoc "=type=" bibtex-entry))))

				    (setq format (cdr (assoc entry-type org-ref-bibliography-entry-format)))
				    (if format
					(setq entry  (org-ref-reftex-format-citation bibtex-entry format))
				      ;; if no format, we use the bibtex entry itself as a fallback
				      (save-restriction
					(bibtex-narrow-to-entry)
					(setq entry (buffer-string)))))
				"Key not found")
			      (replace-regexp-in-string "\"" "" (htmlize-escape-or-link entry)))
			    key))
		  (s-split "," keyword) "<sup>,</sup>"))
      ;; for  pandoc we generate pandoc citations
      ((eq format 'pandoc)
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


(defun org-ref-bibtex-store-link ()
  "Store a link from a bibtex file. Only supports the cite link.
This essentially the same as the store link in org-bibtex, but it
creates a cite link."
  (when (eq major-mode 'bibtex-mode)
    (let* ((entry (mapcar
		   ;; repair strings enclosed in "..." or {...}
		   (lambda(c)
		     (if (string-match
			  "^\\(?:{\\|\"\\)\\(.*\\)\\(?:}\\|\"\\)$" (cdr c))
			 (cons (car c) (match-string 1 (cdr c))) c))
		   (save-excursion
		     (bibtex-beginning-of-entry)
		     (bibtex-parse-entry))))
	   (link (concat "cite:" (cdr (assoc "=key=" entry)))))
      (org-store-link-props
       :key (cdr (assoc "=key=" entry))
       :author (or (cdr (assoc "author" entry)) "[no author]")
       :editor (or (cdr (assoc "editor" entry)) "[no editor]")
       :title (or (cdr (assoc "title" entry)) "[no title]")
       :booktitle (or (cdr (assoc "booktitle" entry)) "[no booktitle]")
       :journal (or (cdr (assoc "journal" entry)) "[no journal]")
       :publisher (or (cdr (assoc "publisher" entry)) "[no publisher]")
       :pages (or (cdr (assoc "pages" entry)) "[no pages]")
       :url (or (cdr (assoc "url" entry)) "[no url]")
       :year (or (cdr (assoc "year" entry)) "[no year]")
       :month (or (cdr (assoc "month" entry)) "[no month]")
       :address (or (cdr (assoc "address" entry)) "[no address]")
       :volume (or (cdr (assoc "volume" entry)) "[no volume]")
       :number (or (cdr (assoc "number" entry)) "[no number]")
       :annote (or (cdr (assoc "annote" entry)) "[no annotation]")
       :series (or (cdr (assoc "series" entry)) "[no series]")
       :abstract (or (cdr (assoc "abstract" entry)) "[no abstract]")
       :btype (or (cdr (assoc "=type=" entry)) "[no type]")
       :type "bibtex"
       :link link
       :description (let ((bibtex-autokey-names 1)
			  (bibtex-autokey-names-stretch 1)
			  (bibtex-autokey-name-case-convert-function 'identity)
			  (bibtex-autokey-name-separator " & ")
			  (bibtex-autokey-additional-names " et al.")
			  (bibtex-autokey-year-length 4)
			  (bibtex-autokey-name-year-separator " ")
			  (bibtex-autokey-titlewords 3)
			  (bibtex-autokey-titleword-separator " ")
			  (bibtex-autokey-titleword-case-convert-function 'identity)
			  (bibtex-autokey-titleword-length 'infty)
			  (bibtex-autokey-year-title-separator ": "))
		      (setq org-bibtex-description (bibtex-generate-autokey)))))))


;; This suppresses showing the warning buffer. bibtex-completion seems to make this
;; pop up in an irritating way.
(unless (boundp 'warning-suppress-types)
  (require 'warnings))


(add-to-list 'warning-suppress-types '(:warning))


(defvar org-ref-buffer-hacked nil
  "If non-nil this buffer has already been hacked and we don't need to do it again.
I use this so we only hack the variables once. This was added
because when you have local file/directory variables, it seems
like they don't get defined when font-lock is occurring, and it
results in warnings from `bibtex-completion' because it cannot
find the keys in the bibliographies. Doing this hack and the one
in `org-ref-cite-link-face-fn' makes the warnings go away. It
seems hacky, but the functions that fix it start with hack
so...")

(make-variable-buffer-local 'org-ref-buffer-hacked)

(defun org-ref-cite-link-face-fn (keys)
  "Return a face for a cite link.
KEYS may be a comma-separated list of keys.
This is not smart enough yet to only highlight the bad key. If any key is bad, the whole cite will be red."
  (unless org-ref-buffer-hacked
    (hack-dir-local-variables)
    (hack-local-variables-apply)
    (setq org-ref-buffer-hacked t))

  (save-match-data
    (cond
     ((or (not org-ref-show-broken-links)
	  (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
	    (-every?
	     'identity
	     (mapcar
	      (lambda (key)
		(if (string= key "*")
		    t
		  (assoc "=key="
			 (bibtex-completion-get-entry key))))
	      (split-string keys ",")))))
      'org-ref-cite-face)
     (t
      'font-lock-warning-face))))


;;;###autoload
(defun org-ref-define-citation-link (type &optional key)
  "Add a citation link of TYPE for `org-ref'.
With optional KEY, set the reftex binding.  For example:
\(org-ref-define-citation-link \"citez\" ?z) will create a new
citez link, with reftex key of z, and the completion function."
  (interactive "sCitation Type: \ncKey: ")

  ;; create the formatting function
  (eval `(org-ref-make-format-function ,type))

  (org-link-set-parameters
   type
   :follow (lambda (_) (funcall org-ref-cite-onclick-function nil))
   :export (quote (intern (format "org-ref-format-%s" type)))
   :complete (quote (intern (format "org-%s-complete-link" type)))
   :help-echo (lambda (window object position)
		(when org-ref-show-citation-on-enter
		  (save-excursion
		    (goto-char position)
		    ;; Here we wrap the citation string to a reasonable size.
		    (let ((s (org-ref-format-entry
			      (org-ref-get-bibtex-key-under-cursor))))
		      (with-temp-buffer
			(insert s)
			(fill-paragraph)
			(buffer-string))))))
   :face 'org-ref-cite-link-face-fn
   :display 'full
   :keymap org-ref-cite-keymap)



  ;; create the completion function
  (eval `(org-ref-make-completion-function ,type))

  ;; store new type so it works with adding citations, which checks
  ;; for existence in this list
  (add-to-list 'org-ref-cite-types type)

  (unless (assoc 'org reftex-cite-format-builtin)
    (add-to-list 'reftex-cite-format-builtin '(org "org-ref citations" ())))

  ;; and finally if a key is specified, we modify the reftex menu
  (when key
    (setf (nth 2 (assoc 'org reftex-cite-format-builtin))
          (append (nth 2 (assoc 'org reftex-cite-format-builtin))
                  `((,key  . ,(concat type ":%l")))))))


(defun org-ref-generate-cite-links ()
  "Create all the link types and their completion functions."
  (interactive)
  (dolist (type org-ref-cite-types)
    (org-ref-define-citation-link type))
  (when (fboundp 'org-link-set-parameters)
    (org-link-set-parameters "cite" :store #'org-ref-bibtex-store-link)))


;; This is what actually generated the cite links
(org-ref-generate-cite-links)


;;;###autoload
(defun org-ref-insert-cite-with-completion (type)
  "Insert a cite link of TYPE with completion."
  (interactive (list (completing-read "Type: " org-ref-cite-types)))
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


;;* Utilities

;;** Extract bibtex entries in org-file

;;;###autoload
(defun org-ref-extract-bibtex-entries ()
  "Extract the bibtex entries in the current buffer into a bibtex src block."
  (interactive)
  (let* ((bibtex-files (org-ref-find-bibliography))
	 (keys (reverse (org-ref-get-bibtex-keys)))
	 (bibtex-entry-kill-ring-max (length keys))
	 (bibtex-entry-kill-ring '()))

    (save-window-excursion
      (cl-loop for key in keys
	       do
	       (bibtex-search-entry key t)
	       (bibtex-kill-entry t)))

    (goto-char (point-max))
    (insert "\n\n")
    (org-insert-heading)
    (insert (format " Bibtex entries

#+BEGIN_SRC bibtex :tangle %s
%s
#+END_SRC"
		    (let ((bibfile (concat (file-name-base
					    (or (buffer-file-name) "references"))
					   ".bib")))
		      (if (file-exists-p bibfile)
			  (file-name-nondirectory
			   (read-file-name "Bibfile: " nil nil nil bibfile))
			bibfile))
		    (mapconcat
		     'identity
		     bibtex-entry-kill-ring
		     "\n\n")))))

;;;###autoload
(defun org-ref-extract-bibtex-to-file (bibfile &optional clobber)
  "Extract all bibtex entries for citations buffer to BIBFILE.
If BIBFILE exists, append, unless you use a prefix arg (C-u),
which will CLOBBER the file."
  (interactive
   (list (read-file-name "Bibfile: " nil nil nil
			 (file-name-nondirectory
			  (concat (file-name-sans-extension
				   (buffer-file-name))
				  ".bib")))
	 current-prefix-arg))

  (let* ((bibtex-files (org-ref-find-bibliography))
	 (keys (reverse (org-ref-get-bibtex-keys)))
	 (bibtex-entry-kill-ring-max (length keys))
	 (bibtex-entry-kill-ring '())
	 (kill-cb (not (find-buffer-visiting bibfile)))
	 (cb (find-file-noselect bibfile))
	 (current-bib-entries (with-current-buffer cb
				(prog1
				    (buffer-string)
				  (when kill-cb (kill-buffer cb))))))

    (save-window-excursion
      (cl-loop for key in keys
	       do
	       (bibtex-search-entry key t)
	       (bibtex-kill-entry t)))

    (with-temp-file bibfile
      (unless clobber (insert current-bib-entries))
      (insert (mapconcat
	       'identity
	       bibtex-entry-kill-ring
	       "\n\n")))))



;;** Find non-ascii charaters
;;;###autoload
(defun org-ref-find-non-ascii-characters ()
  "Find non-ascii characters in the buffer.  Useful for cleaning up bibtex files."
  (interactive)
  (occur "[^[:ascii:]]"))



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
    (setq data (cl-sort data (lambda (x y)
			       (< (string-to-number (car x))
				  (string-to-number (car y))))))
    ;; now get the keys separated by commas
    (setq keys (mapconcat (lambda (x) (cdr x)) data ","))
    (save-excursion
      (goto-char begin)
      (re-search-forward link-string)
      (replace-match keys))))


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
      (setq i (org-ref-list-index key keys)) ;; defined in org-ref
      (if (> direction 0)		     ;; shift right
          (org-ref-swap-keys i (+ i 1) keys)
        (org-ref-swap-keys i (- i 1) keys))
      (setq keys (mapconcat 'identity keys ","))
      ;; and replace the link with the sorted keys
      (save-excursion
	(goto-char begin)
	(re-search-forward link-string)
	(replace-match keys))
      ;; now go forward to key so we can move with the key
      (re-search-forward key)
      (goto-char (match-beginning 0)))))


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
      (cl-loop for key in keys
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
		     (cl-loop for (k s e) in cps
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
	(re-search-forward ":")))))


;;;###autoload
(defun org-ref-previous-key ()
  "Move cursor to the previous cite key when on a cite link.
Otherwise run `left-word'. If the cursor moves off the link,
move to the beginning of the previous cite link after this one."
  (interactive)
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
      (setq index (cl-loop
		   for i from 0
		   for (k s e) in cps
		   if (and (>= p s)
			   (<= p e))
		   return i))
      (goto-char (nth 1 (nth (- index 1) cps)))))))

(defvar org-ref-equation-environments
  '("equation"
    "equation*"
    "align"
    "align*"
    "multline"
    "multline*")
  "LaTeX environments that should be treated as equations when referencing.")

(defvar org-ref-ref-type-inference-alist
  '((org-ref-equation-label-p . "eqref"))
  "Alist of predicate functions taking a label name and the
  desired reference type if the predicate returns true.")

(defun org-ref-enclosing-environment (label)
  "Returns the name of the innermost LaTeX environment containing
the first instance of the label, or nil of there is none."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((label-point (search-forward (format "\\label{%s}" label) nil t)))
       (when label-point
         (catch 'return
           (let (last-begin-point last-env)
             (while (setq
                     last-begin-point (re-search-backward "\\\\begin{\\([^}]+\\)}" nil t)
                     last-env (match-string-no-properties 1))
               (let ((env-end-point
                      (search-forward (format "\\end{%s}" last-env) nil t)))
                 (if (and env-end-point
                          (> env-end-point label-point))
                     (throw 'return last-env)
                   (goto-char last-begin-point)))))))))))

(defun org-ref-equation-label-p (label)
  "Return non-nil if LABEL is an equation label."
  (let ((maybe-env (org-ref-enclosing-environment label)))
    (when maybe-env
      (member maybe-env org-ref-equation-environments))))

(defun org-ref-infer-ref-type (label)
  "Return inferred type for LABEL."
  (or (cl-dolist (pred-pair org-ref-ref-type-inference-alist)
	(when (funcall (car pred-pair) label)
	  (cl-return (eval (cdr pred-pair)))))
      org-ref-default-ref-type))

;;** context around org-ref links
(defun org-ref-get-label-context (label)
  "Return a string of context around a LABEL."
  (save-excursion
    (save-restriction
      (widen)
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
	       (format "^\\( \\)*#\\+label:\\s-*\\(%s\\)\\b" label) nil t)
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
	       (format "^\\( \\)*#\\+tblname:\\s-*\\(%s\\)\\b" label) nil t)
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
	       (format "^\\( \\)*#\\+name:\\s-*\\(%s\\)\\b" label) nil t)
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
	;; radio target
	(goto-char (point-min))
	(when (re-search-forward (format "<<%s>>" (regexp-quote label)) nil t)
	  (throw 'result (match-string 0)))


	(throw 'result "!!! NO CONTEXT FOUND !!!")))))


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
					;looking at the end of the line.
		 (looking-at "$")))

    (save-restriction
      (widen)
      (when (eq major-mode 'org-mode)
        (let* ((object (org-element-context))
               (type (org-element-property :type object)))
          (save-excursion
            (cond
             ;; cite links
             ((-contains? org-ref-cite-types type)
	      (let ((key (org-ref-get-bibtex-key-under-cursor)))
		(if (string= "*" key)
		    "*"
		  (message (org-ref-format-entry key)))))

             ;; message some context about the label we are referring to
             ((or (string= type "ref")
		  (string= type "cref")
		  (string= type "eqref")
		  (string= type "pageref")
		  (string= type "nameref")
		  (string= type "autoref"))
	      (if
		  (= (org-ref-count-labels
		      (org-element-property :path object))
		     0)
		  (message "!!! NO CONTEXT FOUND !!!count: 0")
		(message "%scount: %s"
			 (org-ref-get-label-context
			  (org-element-property :path object))
			 (org-ref-count-labels
			  (org-element-property :path object)))))

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
                  (let ((file (org-ref-find-bibfile bibfile)))
                    (message (if file "%s exists." "!!! %s NOT FOUND !!!")
                             (or file bibfile)))))))))))))

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

(defun org-ref-delete-cite-at-point ()
  "Delete the citation link at point."
  (let* ((cite (org-element-context))
	 (type (org-element-property :type cite)))
    (when (-contains? org-ref-cite-types type)
      (cl--set-buffer-substring
       (org-element-property :begin cite)
       (org-element-property :end cite)
       ""))))


(defun org-ref-update-pre-post-text ()
  "Prompt for pre/post text and update link accordingly.
A blank string deletes pre/post text."
  (save-excursion
    (let* ((cite (org-element-context))
	   (type (org-element-property :type cite))
	   (key (org-element-property :path cite))
	   (text (read-from-minibuffer "Pre/post text: ")))
      ;; First we delete the citation
      (when (-contains? org-ref-cite-types type)
	(cl--set-buffer-substring
	 (org-element-property :begin cite)
	 (org-element-property :end cite)
	 ""))
      ;; Then we reformat the citation
      (if (string= text "")
	  (progn
	    (insert (format "%s:%s " type key))
	    ;; Avoid space before punctuation
	    (when (looking-at "[[:punct:]]")
	      (delete-char 1)))
	(insert (format "[[%s:%s][%s]] " type key text))
	;; (when (looking-at "[[:punct:]]")
	;;   (delete-char 1))
	))))


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
	     trailing-space))
      (kill-new key))))


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
       bracket-p (string= "[" (buffer-substring begin (+ 1 begin)))
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
     ;; Looking back at a link beginning that a user has typed in
     ((save-excursion
	(backward-word 1)
	(looking-at (regexp-opt org-ref-cite-types)))
      (setq begin (point)
	    end (point)
	    newkeys keys
	    new-cite (mapconcat 'identity keys ",")))
     ;; a new cite
     (t
      (setq
       begin (point)
       end (point)
       type org-ref-default-citation-link
       newkeys keys
       bracket-p org-ref-prefer-bracket-links
       new-cite (concat
		 (when bracket-p "[[")
		 type
		 ":"
		 (mapconcat 'identity newkeys ",")
		 (when bracket-p "]]")
		 trailing-space))))
    ;; post link processing after all the variables habe been defined for each
    ;; case
    (delete-region begin end)
    (goto-char begin)
    (insert new-cite)
    (goto-char begin)
    (re-search-forward (mapconcat 'identity keys ","))
    (when (looking-at "]")
      (forward-char 2))))


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
