;;; org-ref.el --- cite and cross-reference in org-mode

;; Copyright(C) 2014,2015 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; URL: https://github.com/jkitchin/org-ref
;; Version: 0.3.0
;; Keywords: org-mode, cite, ref, label
;; Package-Requires: ((org) (dash) (helm) (helm-bibtex) (hydra))

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
;; Lisp code to setup bibliography cite, ref and label org-mode links.
;; also sets up reftex and helm for org-mode citations.  The links are
;; clickable and do things that are useful.  You should really read
;; org-ref.org for details.
;;

;;; Code:
(eval-when-compile
  (require 'cl))
(require 'dash)
(require 'doi-utils)
(require 'helm)
(require 'helm-bibtex)
(require 'helm-config)
(require 'org)
(require 'org-element)
(require 'reftex)
(require 'reftex-cite)

;; * Custom variables
(defgroup org-ref nil
  "Customization group for org-ref."
  :tag "Org Ref"
  :group 'org)


(defcustom org-ref-bibliography-notes
  nil
  "Filename where you will put all your notes about an entry in the default bibliography."
  :type 'file
  :group 'org-ref)


(defcustom org-ref-default-bibliography
  nil
  "List of bibtex files to search for.
You should use full-paths for each file."
  :type '(repeat :tag "List of bibtex files" file)
  :group 'org-ref)


(defcustom org-ref-pdf-directory
  nil
  "Directory where pdfs are stored by key.  put a trailing / in."
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


(defcustom org-ref-bibliography-entry-format
  '(("article" . "%a, %t, <i>%j</i>, <b>%v(%n)</b>, %p (%y). <a href=\"%U\">link</a>. <a href=\"http://dx.doi.org/%D\">doi</a>.")

    ("book" . "%a, %t, %u (%y).")
    ("techreport" . "%a, %t, %i, %u (%y).")
    ("proceedings" . "%e, %t in %S, %u (%y).")
    ("inproceedings" . "%a, %t, %p, in %b, edited by %e, %u (%y)"))
  "String to format an entry.  Just the reference, no numbering at the beginning, etc... see the `org-ref-reftex-format-citation' docstring for the escape codes."
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
  (lambda ()
    (let* ((results (org-ref-get-bibtex-key-and-file thekey))
           (key (car results))
           (bibfile (cdr results)))

      (save-excursion
        (with-temp-buffer
          (insert-file-contents bibfile)
          (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
          (bibtex-search-entry key)
          (org-ref-open-bibtex-notes)))))
  "Function to open the notes for the bibtex key at point.

The default behavior adds entries to a long file with headlines
for each entry.  It also tries to be compatible with ‘org-bibtex’.

An alternative is
 (lambda ()
  (helm-bibtex-edit-notes (car (org-ref-get-bibtex-key-and-file thekey))))

Use that if you like the one file one note approach of ‘helm-bibtex’."
  :type 'function
  :group 'org-ref)


(defcustom org-ref-open-notes-function
  (lambda ()
    (org-show-entry)
    (show-branches)
    (show-children)
    (org-cycle '(64))
    ;;(org-tree-to-indirect-buffer)
    (outline-previous-visible-heading 1)
    (recenter-top-bottom 0))
  "User-defined way to open a notes entry.
This is executed after the entry is found, with the cursor at the
beginning of the headline.  The default setting fully expands the
notes, and moves the headline to the top of the buffer."
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


(defcustom org-ref-helm-bibtex-format-org
  'org-ref-helm-bibtex-format-org
  "Function for how ‘helm-bibtex’ inserts citations."
  :type 'function
  :group 'org-ref)

(setf (cdr (assoc 'org-mode helm-bibtex-format-citation-functions))
      org-ref-helm-bibtex-format-org)


(defcustom org-ref-helm-bibtex-actions
  '(("Insert citation" . helm-bibtex-insert-citation)
    ("Open PDF file (if present)" . helm-bibtex-open-pdf)
    ("Open URL or DOI in browser" . helm-bibtex-open-url-or-doi)
    ("Insert reference" . helm-bibtex-insert-reference)
    ("Insert BibTeX key" . helm-bibtex-insert-key)
    ("Insert BibTeX entry" . helm-bibtex-insert-bibtex)
    ("Attach PDF to email" . helm-bibtex-add-PDF-attachment)
    ("Edit notes" . helm-bibtex-edit-notes)
    ("Show entry" . helm-bibtex-show-entry)
    ("Add keywords to entries" . org-ref-helm-tag-entries)
    ("Copy entry to clipboard" . helm-bibtex-copy-candidate)
    ("Add keywords to entries" . org-ref-helm-tag-entries)
    ("Add keywords to entries" . org-ref-helm-tag-entries))
  "Cons cells of string and function to set the actions of ‘helm-bibtex’ to.
The car of cons cell is the string describing the function.
The cdr of the the cons cell is the function to use."
  :type 'list
  :group 'org-ref)

(loop for i from 0 to (length org-ref-helm-bibtex-actions)
      for ccell in org-ref-helm-bibtex-actions
      do
      (helm-delete-action-from-source (car ccell) helm-source-bibtex)
      (helm-add-action-to-source
       (car ccell)
       (cdr ccell)
       helm-source-bibtex
       i))


(defcustom org-ref-insert-cite-function
  'org-ref-helm-insert-cite-link
  "Function to call to insert citation links.
The default is `org-ref-helm-insert-cite-link' which uses `helm-bibtex'.
`org-ref' modifies `helm-bibtex' a little bit to give `org-mode'
citations, and to reorder default actions.  You may use
`org-ref-insert-cite-link' if you like the reftex interface."
  :type 'function
  :group 'org-ref)


(defcustom org-ref-cite-onclick-function
  'org-ref-cite-click-helm
  "Function that runs when you click on a cite link.
The function must take no arguments.  You may also use
`org-ref-cite-onclick-minibuffer-menu' if you do not like helm.
If you like `hydra', consider using `org-ref-cite-hydra'."
  :type 'function
  :group 'org-ref)


(defcustom org-ref-show-citation-on-enter t
  "If non-nil show the citation summary.
Uses a hook function to display the message in the minibuffer."
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


(defcustom org-ref-clean-bibtex-entry-hook nil
  "Hook that is run in `org-ref-clean-bibtex-entry'.
The functions should take no arguments, and operate on the bibtex
entry at point."
  :group 'org-ref
  :type 'hook)


(defvar org-ref-bibliography-files
  nil
  "Variable to hold bibliography files to be searched.")

;; * org-mode / reftex setup
(defun org-mode-reftex-setup ()
  "Setup `org-mode' and reftex for ‘org-ref’."
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (global-auto-revert-mode t))
  (make-local-variable 'reftex-cite-format)
  (setq reftex-cite-format 'org))

;; define key for inserting citations
(define-key org-mode-map
  (kbd org-ref-insert-cite-key)
  org-ref-insert-cite-function)

(add-hook 'org-mode-hook 'org-mode-reftex-setup)

(eval-after-load 'reftex-vars
  '(progn
     (add-to-list 'reftex-cite-format-builtin
                  '(org "Org-mode citation"
                        ((?\C-m . "cite:%l")     ; default
                         (?d . ",%l")            ; for appending
                         (?a . "autocite:%l")
                         (?t . "citet:%l")
                         (?T . "citet*:%l")
                         (?p . "citep:%l")
                         (?P . "citep*:%l")
                         (?h . "citeauthor:%l")
                         (?H . "citeauthor*:%l")
                         (?y . "citeyear:%l")
                         (?x . "citetext:%l")
                         (?n . "nocite:%l")
                         )))))

;; * Messages for link at cursor
(defvar org-ref-message-timer nil
  "Variable to store the link message timer in.")


(defun org-ref-show-link-messages ()
  "Turn on link messages.
You will see a message in the minibuffer when on a cite, ref or
label link."
  (interactive)
  (or org-ref-message-timer
      (setq org-ref-message-timer
            (run-with-idle-timer 0.5 t 'org-ref-link-message))))


(defun org-ref-cancel-link-messages ()
  "Stop showing messages in minibuffer when on a link."
  (interactive)
  (cancel-timer org-ref-message-timer)
  (setq org-ref-message-timer nil))


(when org-ref-show-citation-on-enter
  (org-ref-show-link-messages))

;; ** Messages for context under mouse pointer

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


(defun org-ref-mouse-messages-on ()
  "Turn on mouse messages."
  (interactive)
  (or org-ref-message-timer-mouse
      (setq org-ref-message-timer-mouse
            (run-at-time "0.5 sec"
                         org-ref-mouse-message-interval
                         'org-ref-mouse-message))))


(defun org-ref-mouse-messages-off ()
  "Turn off mouse messages."
  (interactive)
  (cancel-timer org-ref-message-timer-mouse)
  (setq org-ref-message-timer-mouse nil)
  (message "Mouse messages are off"))


;; Colorizing org-ref links
(defcustom org-ref-colorize-links
  t
  "When non-nil, change colors of links."
  :group 'org-ref)


(defcustom org-ref-cite-color
  "forest green"
  "Color of cite like links."
  :group 'org-ref)


(defcustom org-ref-ref-color
  "dark red"
  "Color of ref like links."
  :group 'org-ref)


(defcustom org-ref-label-color
  "dark magenta"
  "Color of label links."
  :group 'org-ref)


(defvar org-ref-cite-re
  (concat "\\(" (mapconcat
                 (lambda (x)
                   (replace-regexp-in-string "\*" "\\\\*" x))
                 org-ref-cite-types "\\|") "\\)"
                 ":\\([a-zA-Z0-9-_:\\./]*,?\\)*")
  "Regexp for cite links.")


(defvar org-ref-label-re
  "label:\\([a-zA-Z0-9-_:]*,?\\)*"
  "Regexp for label links.")


(defvar org-ref-ref-re
  "\\(eq\\)?ref:\\([a-zA-Z0-9-_:]*,?\\)*"
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


(defun org-ref-colorize-links ()
  "Colorize ‘org-ref’ links."
  (hi-lock-mode 1)
  (highlight-regexp org-ref-cite-re 'org-ref-cite-face)
  (highlight-regexp org-ref-label-re 'org-ref-label-face)
  (highlight-regexp org-ref-ref-re 'org-ref-ref-face))


(when org-ref-colorize-links
  (add-hook 'org-mode-hook 'org-ref-colorize-links))


;; * General org-ref utilities
(defun org-ref-strip-string (string)
  "Strip leading and trailing whitespace from the STRING."
  (replace-regexp-in-string
   (concat search-whitespace-regexp "$" ) ""
   (replace-regexp-in-string
    (concat "^" search-whitespace-regexp ) "" string)))


(defun org-ref-split-and-strip-string (string)
  "Split key-string and strip keys in STRING.
Assumes the key-string is comma delimited."
  (mapcar 'org-ref-strip-string (split-string string ",")))


(defun org-ref-reftex-get-bib-field (field entry &optional format)
  "Get FIELD from a bibtex ENTRY in optional FORMAT.
Similar to `reftex-get-bib-field', but removes enclosing braces
and quotes in FIELD in the bibtex ENTRY."
  (let ((result))
    (setq result (reftex-get-bib-field field entry format))
    (when (and (not (string= result "")) (string= "{" (substring result 0 1)))
      (setq result (substring result 1 -1)))
    (when (and (not (string= result "")) (string= "\"" (substring result 0 1)))
      (setq result (substring result 1 -1)))
    result))


(defun org-ref-reftex-format-citation (entry format)
  "Format the bibtex ENTRY according to the FORMAT argument.
ENTRY is from `bibtex-parse-entry'
The FORMAT is a string with these percent escapes.

In the format, the following percent escapes will be expanded.

%l   The BibTeX label of the citation.
%a   List of author names, see also `reftex-cite-punctuation'.
%2a  Like %a, but abbreviate more than 2 authors like Jones et al.
%A   First author name only.
%e   Works like %a, but on list of editor names.  (%2e and %E work a well)

It is also possible to access all other BibTeX database fields:
%b booktitle     %c chapter        %d edition    %h howpublished
%i institution   %j journal        %k key        %m month
%n number        %o organization   %p pages      %P first page
%r address       %s school         %u publisher  %t title
%v volume        %y year
%B booktitle, abbreviated          %T title, abbreviated
%U url
%D doi
%S series

Usually, only %l is needed.  The other stuff is mainly for the echo area
display, and for (setq reftex-comment-citations t).

%< as a special operator kills punctuation and space around it after the
string has been formatted.

A pair of square brackets indicates an optional argument, and RefTeX
will prompt for the values of these arguments.

Beware that all this only works with BibTeX database files.  When
citations are made from the \bibitems in an explicit thebibliography
environment, only %l is available."
  ;; Format a citation from the info in the BibTeX ENTRY
  (unless (stringp format) (setq format "\\cite{%l}"))

  (if (and reftex-comment-citations
           (string-match "%l" reftex-cite-comment-format))
      (error "Reftex-cite-comment-format contains invalid %%l"))

  (while (string-match
          "\\(\\`\\|[^%]\\)\\(\\(%\\([0-9]*\\)\\([a-zA-Z]\\)\\)[.,;: ]*\\)"
          format)
    (let ((n (string-to-number (match-string 4 format)))
          (l (string-to-char (match-string 5 format)))
          rpl b e)
      (save-match-data
        (setq rpl
              (cond
               ((= l ?l) (concat
                          (org-ref-reftex-get-bib-field "&key" entry)
                          (if reftex-comment-citations
                              reftex-cite-comment-format
                            "")))
               ((= l ?a) (replace-regexp-in-string
                          "\n\\|\t\\|\s+" " "
                          (reftex-format-names
                           (reftex-get-bib-names "author" entry)
                           (or n 2))))
               ((= l ?A) (replace-regexp-in-string
                          "\n\\|\t\\|\s+" " "
                          (car (reftex-get-bib-names "author" entry))))
               ((= l ?b) (org-ref-reftex-get-bib-field "booktitle" entry "in: %s"))
               ((= l ?B) (reftex-abbreviate-title
                          (org-ref-reftex-get-bib-field "booktitle" entry "in: %s")))
               ((= l ?c) (org-ref-reftex-get-bib-field "chapter" entry))
               ((= l ?d) (org-ref-reftex-get-bib-field "edition" entry))
               ((= l ?D) (org-ref-reftex-get-bib-field "doi" entry))
               ((= l ?e) (reftex-format-names
                          (reftex-get-bib-names "editor" entry)
                          (or n 2)))
               ((= l ?E) (car (reftex-get-bib-names "editor" entry)))
               ((= l ?h) (org-ref-reftex-get-bib-field "howpublished" entry))
               ((= l ?i) (org-ref-reftex-get-bib-field "institution" entry))
               ((= l ?j) (let ((jt (reftex-get-bib-field "journal" entry)))
                           (if (string= "" jt)
                               (reftex-get-bib-field "journaltitle" entry)
                             jt)))
               ((= l ?k) (org-ref-reftex-get-bib-field "=key=" entry))
               ((= l ?m) (org-ref-reftex-get-bib-field "month" entry))
               ((= l ?n) (org-ref-reftex-get-bib-field "number" entry))
               ((= l ?o) (org-ref-reftex-get-bib-field "organization" entry))
               ((= l ?p) (org-ref-reftex-get-bib-field "pages" entry))
               ((= l ?P) (car (split-string
                               (org-ref-reftex-get-bib-field "pages" entry)
                               "[- .]+")))
               ((= l ?s) (org-ref-reftex-get-bib-field "school" entry))
               ((= l ?S) (org-ref-reftex-get-bib-field "series" entry))
               ((= l ?u) (org-ref-reftex-get-bib-field "publisher" entry))
               ((= l ?U) (org-ref-reftex-get-bib-field "url" entry))
               ((= l ?r) (org-ref-reftex-get-bib-field "address" entry))
               ;; strip enclosing brackets from title if they are there
               ((= l ?t) (replace-regexp-in-string
                          "\n\\|\t\\|\s+" " "
                          (org-ref-reftex-get-bib-field "title" entry)))
               ((= l ?T) (reftex-abbreviate-title
                          ((replace-regexp-in-string
                            "\n\\|\t\\|\s+" " "
                            (org-ref-reftex-get-bib-field "title" entry)))))
               ((= l ?v) (org-ref-reftex-get-bib-field "volume" entry))
               ((= l ?y) (org-ref-reftex-get-bib-field "year" entry)))))

      (if (string= rpl "")
          (setq b (match-beginning 2) e (match-end 2))
        (setq b (match-beginning 3) e (match-end 3)))
      (setq format (concat (substring format 0 b) rpl (substring format e)))))
  (while (string-match "%%" format)
    (setq format (replace-match "%" t t format)))
  (while (string-match "[ ,.;:]*%<" format)
    (setq format (replace-match "" t t format)))
  format)


(defun org-ref-get-bibtex-entry-citation (key)
  "Return a string for the bibliography entry corresponding to KEY.
Format according to the type in `org-ref-bibliography-entry-format'."

  (let ((org-ref-bibliography-files (org-ref-find-bibliography))
        (file) (entry) (bibtex-entry) (entry-type) (format))

    (setq file (catch 'result
                 (cl-loop for file in org-ref-bibliography-files do
                          (if (org-ref-key-in-file-p key (file-truename file))
                              (throw 'result file)
                            (message "%s not found in %s"
                                     key (file-truename file))))))

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
    entry))


(defun org-ref-get-bibtex-keys (&optional sort)
  "Return a list of unique keys in the buffer.
Use SORT to specify alphabetical order by key."
  (let ((keys '()))
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (let ((plist (nth 1 link)))
          (when (-contains? org-ref-cite-types (plist-get plist ':type))
            (dolist
                (key
                 (org-ref-split-and-strip-string (plist-get plist ':path)))
              (when (not (-contains? keys key))
                (setq keys (append keys (list key))))))))
      ;; set with-affiliated to get keys in captions
      nil nil nil t)
    (when sort
      ;; Sort keys alphabetically
      (setq keys (cl-sort keys 'string-lessp :key 'downcase)))
    keys))


(defun org-ref-bibliography (&optional sort)
  "Create a new buffer with a bibliography.
If SORT is non-nil it is alphabetically sorted by key
This is mostly for convenience to see what has been cited.
Entries are formatted according to the bibtex entry type in
`org-ref-bibliography-entry-format', and the actual entries are
generated by `org-ref-reftex-format-citation'."
  (interactive)
  (let ((bib (mapconcat
              'identity
              (loop for i from 1
                    for citation in
                    (mapcar
                     (lambda (key)
                       (let* ((results (org-ref-get-bibtex-key-and-file key))
                              (key (car results))
                              (bibfile (cdr results)))
                         (format "cite:%s %s" key
                                 (if bibfile
                                     (save-excursion
                                       (with-temp-buffer
                                         (insert-file-contents bibfile)
                                         (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
                                         (bibtex-search-entry key)
                                         (org-ref-bib-citation)))
                                   "!!! No entry found !!!"))))
                     (org-ref-get-bibtex-keys sort))
                    collect (format "%3s. %s" i citation))
              "\n\n")))

    (switch-to-buffer-other-window (format "%s-bibliography" (buffer-file-name)))
    (erase-buffer)
    (insert bib)
    (org-mode)))


(defun org-ref-get-bibtex-entry-html (key)
  "Return an html string for the bibliography entry corresponding to KEY."
  (let ((output))
    (setq output (org-ref-get-bibtex-entry-citation key))
    ;; unescape the &
    (setq output (replace-regexp-in-string "\\\\&" "&" output))
    ;; hack to replace {} around text
    (setq output (replace-regexp-in-string "{" "" output))
    (setq output (replace-regexp-in-string "}" "" output))
    ;; get rid of empty parens
    (setq output (replace-regexp-in-string "()" "" output))
    ;; get rid of empty link and doi
    (setq output (replace-regexp-in-string " <a href=\"\">link</a>\\." "" output))
    ;; change double dash to single dash
    (setq output (replace-regexp-in-string "--" "-" output))
    (setq output (replace-regexp-in-string " <a href=\"http://dx\\.doi\\.org/\">doi</a>\\." "" output))
    (format "<li><a id=\"%s\">[%s] %s</a></li>"
            key key output)))


(defun org-ref-get-html-bibliography (&optional sort)
  "Create an html bibliography when there are keys.
If SORT is non-nil the bibliography is alphabetically sorted."
  (let ((keys (org-ref-get-bibtex-keys sort)))
    (when keys
      (concat "<h1 class='org-ref-bib-h1'>Bibliography</h1>
<ul class='org-ref-bib'>"
              (mapconcat (lambda (x) (org-ref-get-bibtex-entry-html x)) keys "\n")
              "\n</ul>"))))


(defun org-ref-get-bibtex-entry-org (key)
  "Return an org string for the bibliography entry corresponding to KEY."
  (let ((org-ref-bibliography-files (org-ref-find-bibliography))
        (file) (entry) (bibtex-entry) (entry-type) (format))

    (setq file (catch 'result
                 (cl-loop for file in org-ref-bibliography-files do
                          (if (org-ref-key-in-file-p key (file-truename file))
                              (throw 'result file)
                            (message "%s not found in %s" key (file-truename file))))))

    (with-temp-buffer
      (insert-file-contents file)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (bibtex-search-entry key nil 0)
      (setq entry (bibtex-parse-entry))
      (format "** %s - %s
  :PROPERTIES:
  %s
  :END:
" (org-ref-reftex-get-bib-field "author" entry)
(org-ref-reftex-get-bib-field "title" entry)
(concat "   :CUSTOM_ID: " (org-ref-reftex-get-bib-field "=key=" entry) "\n"
        (mapconcat (lambda (element) (format "   :%s: %s"
                                             (upcase (car element))
                                             (cdr element)))
                   entry
                   "\n"))))))


(defun org-ref-get-org-bibliography (&optional sort)
  "Create an org bibliography when there are keys.
If SORT is non-nil the bibliography is sorted alphabetically by key."
  (let ((keys (org-ref-get-bibtex-keys sort)))
    (when keys
      (concat "* Bibliography
"
              (mapconcat (lambda (x) (org-ref-get-bibtex-entry-org x)) keys "\n")
              "\n"))))


(defun org-ref-get-bibtex-entry-ascii (key)
  "Return an ascii string for the bibliography entry corresponding to KEY."

  (format "[%s] %s" key (org-ref-get-bibtex-entry-citation key)))


(defun org-ref-get-ascii-bibliography (&optional sort)
  "Create an ascii bibliography when there are keys.
if SORT is non-nil the bibliography is sorted alphabetically by key."
  (let ((keys (org-ref-get-bibtex-keys sort)))
    (when keys
      (concat
       "Bibliography
=============
"
       (mapconcat (lambda (x) (org-ref-get-bibtex-entry-ascii x)) keys "\n")
       "\n"))))

(defun org-ref-get-odt-bibliography (&optional sort)
  "Create an ascii bibliography ofr odt export when there are keys.
if SORT is non-nil the bibliography is sorted alphabetically by
key.  This is a variant of `org-ref-get-ascii-bibliography' where
some things are escaped since odt is an xml format."
  (let ((keys (org-ref-get-bibtex-keys sort)))
    (when keys
      (concat
       "Bibliography
=============
"
       (mapconcat (lambda (x)
		    (xml-escape-string (org-ref-get-bibtex-entry-ascii x)))
		  keys "\n")
       "\n"))))

;; * Links
;; ** bibliography and bibliographystyle
(org-add-link-type "bibliography"
                   ;; this code is run on clicking. The bibliography
                   ;; may contain multiple files. this code finds the
                   ;; one you clicked on and opens it.
                   (lambda (link-string)
                     ;; get link-string boundaries we have to go to the
                     ;; beginning of the line, and then search forward
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
                         (find-file bibfile))))

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
				 (mapcar 'expand-file-name
					 (split-string keyword ","))
				 ",")))))))



(org-add-link-type "nobibliography"
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
                             (setq key-beginning (point)))) ; no match found
                         ;; save the key we clicked on.
                         (setq bibfile (org-ref-strip-string (buffer-substring key-beginning key-end)))
                         (find-file bibfile)))) ; open file on click

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
                                           (mapcar 'expand-file-name
                                                   (split-string keyword ","))
                                           ",")))))))


(org-add-link-type "printbibliography"
                   (lambda (arg) (message "Nothing implemented for clicking here."))
                   (lambda (keyword desc format)
                     (cond
                      ((eq format 'org) (org-ref-get-org-bibliography))
                      ((eq format 'html) (org-ref-get-html-bibliography))
                      ((eq format 'latex)
                       ;; write out the biblatex bibliography command
                       "\\printbibliography"))))


(org-add-link-type "bibliographystyle"
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


(defun org-ref-insert-bibliography-link ()
  "Insert a bibliography with completion."
  (interactive)
  (insert (org-bibliography-complete-link)))

;; ** addbibresource

(org-add-link-type "addbibresource"
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

                       ;; We set the reftex-default-addbibresource
                       ;; here. it should be a local variable only in
                       ;; the current buffer. We need this for using
                       ;; reftex to do citations.
                       (set (make-local-variable 'reftex-default-addbibresource)
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
                             (setq key-beginning (point)))) ; no match found
                         ;; save the key we clicked on.
                         (setq bibfile (org-ref-strip-string
                                        (buffer-substring key-beginning key-end)))
                         (find-file bibfile)))) ; open file on click

                   ;; formatting code
                   (lambda (keyword desc format)
                     (cond
                      ((eq format 'html) (format "")) ; no output for html
                      ((eq format 'latex)
                       ;; write out the latex addbibresource command
                       (format "\\addbibresource{%s}" keyword)))))

;; ** List of figures
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
				    (intersection (org-get-tags-at) org-export-exclude-tags
						  :test 'equal)))))
		  (cl-incf counter)

		  (let* ((start (org-element-property :begin link))
			 (parent (car (cdr (org-element-property :parent link))))
			 (caption (cl-caaar (plist-get parent :caption)))
			 (name (plist-get parent :name)))
		    (if caption
			(format
			 "[[elisp:(progn (switch-to-buffer \"%s\")(goto-char %s)(org-show-entry))][figure %s: %s]] %s\n"
			 c-b start counter (or name "") caption)
		      (format
		       "[[elisp:(progn (switch-to-buffer \"%s\")(goto-char %s)(org-show-entry))][figure %s: %s]]\n"
		       c-b start counter (or name "")))))))))
      (switch-to-buffer "*List of Figures*")
      (setq buffer-read-only nil)
      (org-mode)
      (erase-buffer)
      (insert (mapconcat 'identity list-of-figures ""))
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

;; ** List of tables
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
				(intersection (org-get-tags-at) org-export-exclude-tags 
				               :test 'equal))))
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


;; ** label link
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
     (let ((custom-id-count 0))
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

;; ** ref link
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
           (format "^#\\+tblname:\\s-*\\(%s\\)\\b" label) nil t)))

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
      (while (re-search-forward "\\\\label{\\([a-zA-z0-9:-]*\\)}" (point-max) t)
        (add-to-list 'matches (match-string-no-properties 1) t))
      matches)))


(defun org-ref-get-tblnames ()
  "Return list of table names in the buffer."
  (org-element-map (org-element-parse-buffer 'element) 'table
    (lambda (table)
      (org-element-property :name table))))


(defun org-ref-get-labels ()
  "Return a list of labels in the buffer that you can make a ref link to.
This is used to complete ref links and in helm menus."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((matches '()))
        ;; these are the org-ref label:stuff  kinds
        (while (re-search-forward
                "[^#+]label:\\([a-zA-z0-9:-]*\\)" (point-max) t)
          (add-to-list 'matches (match-string-no-properties 1) t))
        ;; now add all the other kinds of labels.
        (append matches
                ;; #+label:
                (org-ref-get-org-labels)
                ;; \label{}
                (org-ref-get-latex-labels)
                ;; #+tblname: and actually #+label
                (org-ref-get-tblnames)
                ;; CUSTOM_IDs
                (org-ref-get-custom-ids))))))


(defun org-ref-helm-insert-label-link ()
  "Insert a label link.
Helm just shows you what labels already exist.  If you are on a
label link, replace it."
  (interactive)
  (let* ((labels (org-ref-get-labels))
         (cb (current-buffer)))
    (helm :sources `(((name . "Existing labels")
                      (candidates . ,labels)
                      ;; default action is to open to the label
                      (action . (lambda (label)
                                  ;; unfortunately I do not have markers here
                                  (org-open-link-from-string
                                   (format "ref:%s" label))))
                      ;; if you select a label, replace current one
                      (action . (lambda (label)
                                  (switch-to-buffer ,cb)
                                  (cond
                                   ;;  no prefix or on a link
                                   ((equal helm-current-prefix-arg nil)
                                    (let* ((object (org-element-context))
                                           (last-char (save-excursion
                                                        (goto-char (org-element-property :end object))
                                                        (backward-char)
                                                        (if (looking-at " ")
                                                            " "
                                                          ""))))
                                      (when (-contains? '("label")
                                                        (org-element-property :type object))
                                        ;; we are on a link, so replace it.
                                        (setf
                                         (buffer-substring
                                          (org-element-property :begin object)
                                          (org-element-property :end object))
                                         (concat
                                          (replace-regexp-in-string
                                           (org-element-property :path object)
                                           label
                                           (org-element-property :raw-link object))
                                          last-char)))))
                                   ;; no prefix options defined
                                   ))))
                     ;; no matching selection creates a new label
                     ((name . "Create new label")
                      (dummy)
                      ;; default action creates a new label, or replaces old one
                      (action . (lambda (label)
                                  (switch-to-buffer ,cb)
                                  (let* ((object (org-element-context))
                                         (last-char (save-excursion
                                                      (goto-char (org-element-property :end object))
                                                      (backward-char)
                                                      (if (looking-at " ")
                                                          " "
                                                        ""))))
                                    (if (-contains? '("label")
                                                    (org-element-property :type object))
                                        ;; we are on a link, so replace it.
                                        (setf
                                         (buffer-substring
                                          (org-element-property :begin object)
                                          (org-element-property :end object))
                                         (concat
                                          (replace-regexp-in-string
                                           (org-element-property :path object)
                                           helm-pattern
                                           (org-element-property :raw-link object))
                                          last-char))
                                      ;; new link
                                      (insert
                                       (concat
                                        "label:"
                                        (or label
                                            helm-pattern))))))))))))


(defun org-ref-complete-link (&optional arg)
  "Completion function for ref links.
Optional argument ARG Does nothing."
  (let ((label))
    (setq label (completing-read "label: " (org-ref-get-labels)))
    (format "ref:%s" label)))


(defun org-ref-insert-ref-link ()
  "Completion function for a ref link."
  (interactive)
  (insert (org-ref-complete-link)))


(defun org-ref-helm-insert-ref-link ()
  "Helm menu to insert ref links to labels in the document.
If you are on link, replace with newly selected label.  Use
\\[universal-argument] to insert a different kind of ref link.
Use a double \\[universal-argument] \\[universal-argument] to insert a
[[#custom-id]] link"
  (interactive)
  (let* ((labels (org-ref-get-labels))
         (bs (buffer-string))
         (contexts (with-temp-buffer
                     (insert bs)
                     (mapcar 'org-ref-get-label-context labels)))
         (cb (current-buffer)))

    (helm :input (thing-at-point 'word)
          :sources `(((name . "Available labels to ref")
                      (multiline)
                      (candidates . ,(cl-loop for label in labels
                                              for context in contexts
                                              ;; we do some kludgy adding spaces
                                              ;; and bars to make it "easier" to
                                              ;; see in helm.
                                              collect (cons (concat
                                                             label "\n"
                                                             (mapconcat
                                                              (lambda (x)
                                                                (concat "   |" x))
                                                              (split-string context "\n")
                                                              "\n"
                                                              ) "\n\n") label)))
                      ;; default action to replace or insert ref link.
                      (action . (lambda (label)
                                  (switch-to-buffer ,cb)

                                  (cond
                                   ;;  no prefix or on a link
                                   ((equal helm-current-prefix-arg nil)
                                    (let* ((object (org-element-context))
                                           (last-char (save-excursion
                                                        (goto-char (org-element-property :end object))
                                                        (backward-char)
                                                        (if (looking-at " ")
                                                            " "
                                                          ""))))
                                      (if (-contains? '("ref" "eqref" "pageref" "nameref")
                                                      (org-element-property :type object))
                                          ;; we are on a link, so replace it.
                                          (setf
                                           (buffer-substring
                                            (org-element-property :begin object)
                                            (org-element-property :end object))
                                           (concat
                                            (replace-regexp-in-string
                                             (org-element-property :path object)
                                             label
                                             (org-element-property :raw-link object))
                                            last-char))
                                        ;; insert a new link
                                        (insert
                                         (concat
                                          "ref:" label))
                                        )))
                                   ;; one prefix, alternate ref link
                                   ((equal helm-current-prefix-arg '(4))
                                    (insert
                                     (concat
                                      (helm :sources '((name . "Ref link types")
                                                       (candidates . ("ref" "eqref" "pageref" "nameref"))
                                                       (action . (lambda (x) x))))
                                      ":" label)))
                                   ;; two prefixes, insert section custom-id link
                                   ((equal helm-current-prefix-arg '(16))
                                    (insert
                                     (format "[[#%s]]" label)))))))))))

;; *** pageref link
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


(defun org-pageref-insert-ref-link ()
  "Insert a pageref link with completion."
  (interactive)
  (insert (org-pageref-complete-link)))


;; *** nameref link
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

;; *** eqref link

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

;; ** cite link

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
\bibliography{f1,f2} internal bibliographies

falling back to what the user has set in `org-ref-default-bibliography'"
  (catch 'result
    (save-excursion
      (goto-char (point-min))
      ;;  look for a bibliography link
      (when (re-search-forward "\\<bibliography:\\([^\]\|\n]+\\)" nil t)
        (setq org-ref-bibliography-files
              (mapcar 'org-ref-strip-string (split-string (match-string 1) ",")))
        (throw 'result org-ref-bibliography-files))


      ;; we did not find a bibliography link. now look for \bibliography
      (goto-char (point-min))
      (when (re-search-forward "\\\\bibliography{\\([^}]+\\)}" nil t)
        ;; split, and add .bib to each file
        (setq org-ref-bibliography-files
              (mapcar (lambda (x) (concat x ".bib"))
                      (mapcar 'org-ref-strip-string
                              (split-string (match-string 1) ","))))
        (throw 'result org-ref-bibliography-files))

      ;; no bibliography found. maybe we need a biblatex addbibresource
      (goto-char (point-min))
      ;;  look for a bibliography link
      (when (re-search-forward "addbibresource:\\([^\]\|\n]+\\)" nil t)
        (setq org-ref-bibliography-files
              (mapcar 'org-ref-strip-string (split-string (match-string 1) ",")))
        (throw 'result org-ref-bibliography-files))

      ;; we did not find anything. use defaults
      (setq org-ref-bibliography-files org-ref-default-bibliography)))

  ;; set reftex-default-bibliography so we can search
  (set (make-local-variable 'reftex-default-bibliography) org-ref-bibliography-files)
  org-ref-bibliography-files)


(defun org-ref-key-in-file-p (key filename)
  "Determine if the KEY is in the FILENAME."
  (save-current-buffer
    (let ((bibtex-files (list filename)))
      ;; This is something I am trying because when the bibtex file is open, and
      ;; you have added to it, the only way I find to get the update to update
      ;; is to close it and reopen it. or to save it and revert it.
      (when (get-file-buffer filename)
        (set-buffer (get-file-buffer filename))
        (when (buffer-modified-p (current-buffer))
          (save-buffer)
          (revert-buffer t t)))
      (bibtex-search-entry key t))))


(defun org-ref-get-bibtex-key-and-file (&optional key)
  "Return the bibtex KEY and file that it is in.  If no key is provided, get one under point."
  (let ((org-ref-bibliography-files (org-ref-find-bibliography))
        (file))
    (unless key
      (setq key (org-ref-get-bibtex-key-under-cursor)))
    (setq file     (catch 'result
                     (cl-loop for file in org-ref-bibliography-files do
                              (if (org-ref-key-in-file-p key (file-truename file))
                                  (throw 'result file)))))
    (cons key file)))

;; *** key at point functions
(defun org-ref-get-pdf-filename (key)
  "Return the pdf filename associated with a bibtex KEY."
  (format (concat (file-name-as-directory org-ref-pdf-directory) "%s.pdf") key))


(defun org-ref-get-mendeley-filename (key)
  "Return the pdf filename indicated by mendeley file field.
Falls back to ‘org-ref-get-pdf-filename’ if file filed does not exist.
Contributed by https://github.com/autosquid.
Argument KEY is the bibtex key."
  (let* ((results (org-ref-get-bibtex-key-and-file key))
         (bibfile (cdr results))
         entry)
    (with-temp-buffer
      (insert-file-contents bibfile)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (bibtex-search-entry key nil 0)
      (setq entry (bibtex-parse-entry))
      (let ((e (org-ref-reftex-get-bib-field "file" entry)))
        (if (> (length e) 4)
            (let ((clean-field (-remove (lambda (char) (-contains? "{}\\" char)) e)))
              (let ((first-file (car (split-string clean-field ";" t))))
                (format "/%s" (-slice first-file 1 (- (length first-file) 4)))))
          (format (concat
                   (file-name-as-directory org-ref-pdf-directory)
                   "%s.pdf")
                  key))))))


(defun org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (pdf-file (funcall org-ref-get-pdf-filename-function key)))
    (if (file-exists-p pdf-file)
        (org-open-file pdf-file)
      (message "no pdf found for %s" key))))


(defun org-ref-open-url-at-point ()
  "Open the url for bibtex key under point."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (bibfile (cdr results)))
    (save-excursion
      (with-temp-buffer
        (insert-file-contents bibfile)
        (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
        (bibtex-search-entry key)
        ;; I like this better than bibtex-url which does not always find
        ;; the urls
        (catch 'done
          (let ((url (bibtex-autokey-get-field "url")))
            (when  url
              (browse-url (s-trim url))
              (throw 'done nil)))

          (let ((doi (bibtex-autokey-get-field "doi")))
            (when doi
              (if (string-match "^http" doi)
                  (browse-url doi)
                (browse-url (format "http://dx.doi.org/%s" (s-trim doi))))
              (throw 'done nil))))))))


(defun org-ref-open-notes-at-point (&optional thekey)
  "Open the notes for bibtex key under point in a cite link in a buffer.
Can also be called with THEKEY in a program."
  (interactive)
  (funcall org-ref-notes-function))


(defun org-ref-citation-at-point ()
  "Give message of current citation at point."
  (interactive)
  (let* ((cb (current-buffer))
         (results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (bibfile (cdr results)))
    (message "%s" (progn
                    (with-temp-buffer
                      (insert-file-contents bibfile)
                      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
                      (bibtex-search-entry key)
                      (org-ref-bib-citation))))))


(defun org-ref-open-citation-at-point ()
  "Open bibtex file to key at point."
  (interactive)
  (let* ((cb (current-buffer))
         (results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (bibfile (cdr results)))
    (find-file bibfile)
    (bibtex-search-entry key)))

;; *** cite menu
(defvar org-ref-cite-menu-funcs '()
  "Functions to run on cite click menu.
Each entry is a list of (key menu-name function).  The function
must take no arguments and work on the key at point.  Do not
modify this variable, it is set to empty in the menu click
function, and functions are conditionally added to it.")


(defvar org-ref-user-cite-menu-funcs
  '(("C" "rossref" org-ref-crossref-at-point)
    ("y" "Copy entry to file" org-ref-copy-entry-at-point-to-file)
    ("s" "Copy summary" org-ref-copy-entry-as-summary))
  "User-defined functions to run on bibtex key at point.")


(defun org-ref-copy-entry-as-summary ()
  "Copy the bibtex entry for the citation at point as a summary."
  (interactive)
  (save-window-excursion
    (org-ref-open-citation-at-point)
    (kill-new (org-ref-bib-citation))))


(defun org-ref-copy-entry-at-point-to-file ()
  "Copy the bibtex entry for the citation at point to NEW-FILE.
Prompt for NEW-FILE includes bib files in
`org-ref-default-bibliography', and bib files in current working
directory.  You can also specify a new file."
  (interactive)
  (let ((new-file (ido-completing-read
                   "Copy to bibfile: "
                   (append org-ref-default-bibliography
                           (f-entries "." (lambda (f) (f-ext? f "bib"))))))
        (key (org-ref-get-bibtex-key-under-cursor)))
    (save-window-excursion
      (org-ref-open-citation-at-point)
      (bibtex-copy-entry-as-kill))

    (let ((bibtex-files (list (file-truename new-file))))
      (if (assoc key (bibtex-global-key-alist))
          (message "That key already exists in %s" new-file)
        ;; add to file
        (save-window-excursion
          (find-file new-file)
          (goto-char (point-max))
          ;; make sure we are at the beginning of a line.
          (unless (looking-at "^") (insert "\n\n"))
          (bibtex-yank)
          (save-buffer))))))


(defun org-ref-get-doi-at-point ()
  "Get doi for key at point."
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (bibfile (cdr results))
         doi)
    (save-excursion
      (with-temp-buffer
        (insert-file-contents bibfile)
        (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
        (bibtex-search-entry key)
        (setq doi (bibtex-autokey-get-field "doi"))
        ;; in case doi is a url, remove the url part.
        (replace-regexp-in-string "^http://dx.doi.org/" "" doi)))))


;; **** functions that operate on key at point for click menu
(defun org-ref-wos-at-point ()
  "Open the doi in wos for bibtex key under point."
  (interactive)
  (doi-utils-wos (org-ref-get-doi-at-point)))


(defun org-ref-wos-citing-at-point ()
  "Open the doi in wos citing articles for bibtex key under point."
  (interactive)
  (doi-utils-wos-citing (org-ref-get-doi-at-point)))


(defun org-ref-wos-related-at-point ()
  "Open the doi in wos related articles for bibtex key under point."
  (interactive)
  (doi-utils-wos-related (org-ref-get-doi-at-point)))


(defun org-ref-google-scholar-at-point ()
  "Open the doi in google scholar for bibtex key under point."
  (interactive)
  (doi-utils-google-scholar (org-ref-get-doi-at-point)))


(defun org-ref-pubmed-at-point ()
  "Open the doi in pubmed for bibtex key under point."
  (interactive)
  (doi-utils-pubmed (org-ref-get-doi-at-point)))


(defun org-ref-crossref-at-point ()
  "Open the doi in crossref for bibtex key under point."
  (interactive)
  (doi-utils-crossref (org-ref-get-doi-at-point)))

;; *** Minibuffer menu

(defun org-ref-cite-onclick-minibuffer-menu (&optional link-string)
  "Action when a cite link is clicked on.
Provides a menu of context sensitive actions.  If the bibtex entry
has a pdf, you get an option to open it.  If there is a doi, you
get a lot of options.  LINK-STRING is used by the link function."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (pdf-file (funcall org-ref-get-pdf-filename-function key))
         (bibfile (cdr results))
         (url (save-excursion
                (with-temp-buffer
                  (insert-file-contents bibfile)
                  (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
                  (bibtex-search-entry key)
                  (bibtex-autokey-get-field "url"))))
         (doi (save-excursion
                (with-temp-buffer
                  (insert-file-contents bibfile)
                  (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
                  (bibtex-search-entry key)
                  ;; I like this better than bibtex-url which does not always find
                  ;; the urls
                  (bibtex-autokey-get-field "doi")))))

    (when (string= "" doi) (setq doi nil))
    (when (string= "" url) (setq url nil))
    (setq org-ref-cite-menu-funcs '())

    ;; open action
    (when
        bibfile
      (add-to-list
       'org-ref-cite-menu-funcs
       '("o" "pen" org-ref-open-citation-at-point)))

    ;; pdf
    (when (file-exists-p pdf-file)
      (add-to-list
       'org-ref-cite-menu-funcs
       `("p" "df" ,org-ref-open-pdf-function) t))

    ;; notes
    (add-to-list
     'org-ref-cite-menu-funcs
     '("n" "otes" org-ref-open-notes-at-point) t)

    ;; url
    (when (or url doi)
      (add-to-list
       'org-ref-cite-menu-funcs
       '("u" "rl" org-ref-open-url-at-point) t))

    ;; doi funcs
    (when doi
      (add-to-list
       'org-ref-cite-menu-funcs
       '("w" "os" org-ref-wos-at-point) t)

      (add-to-list
       'org-ref-cite-menu-funcs
       '("c" "iting" org-ref-wos-citing-at-point) t)

      (add-to-list
       'org-ref-cite-menu-funcs
       '("r" "elated" org-ref-wos-related-at-point) t)

      (add-to-list
       'org-ref-cite-menu-funcs
       '("g" "oogle scholar" org-ref-google-scholar-at-point) t)

      (add-to-list
       'org-ref-cite-menu-funcs
       '("P" "ubmed" org-ref-pubmed-at-point) t))

    ;; add user functions
    (dolist (tup org-ref-user-cite-menu-funcs)
      (add-to-list
       'org-ref-cite-menu-funcs
       tup t))

    ;; finally quit
    (add-to-list
     'org-ref-cite-menu-funcs
     '("q" "uit" (lambda ())) t)

    ;; now we make a menu
    ;; construct menu string as a message
    (message
     (concat
      (let* ((results (org-ref-get-bibtex-key-and-file))
             (key (car results))
             (bibfile (cdr results)))
        (save-excursion
          (with-temp-buffer
            (insert-file-contents bibfile)
            (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
            (bibtex-search-entry key)
            (org-ref-bib-citation))))
      "\n"
      (mapconcat
       (lambda (tup)
         (concat "[" (elt tup 0) "]"
                 (elt tup 1) " "))
       org-ref-cite-menu-funcs "")))
    ;; get the input
    (let* ((input (read-char-exclusive))
           (choice (assoc
                    (char-to-string input) org-ref-cite-menu-funcs)))
      ;; now run the function (2nd element in choice)
      (when choice
        (funcall
         (elt
          choice
          2))))))

;; *** Generation of the cite links
(defmacro org-ref-make-completion-function (type)
  "Macro to make a link completion function for a link of TYPE."
  `(defun ,(intern (format "org-%s-complete-link" type)) (&optional arg)
     (interactive)
     (format "%s:%s"
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


(defun org-ref-define-citation-link (type &optional key)
  "Add a citation link of TYPE for ‘org-ref’.
With optional KEY, set the reftex binding.  For example:
\(org-ref-define-citation-link \"citez\" ?z) will create a new
citez link, with reftex key of z, and the completion function."
  (interactive "sCitation Type: \ncKey: ")

  ;; create the formatting function
  (eval `(org-ref-make-format-function ,type))

  (eval
   `(org-add-link-type
     ,type
     org-ref-cite-onclick-function
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


(defun org-ref-insert-cite-link (alternative-cite)
  "Insert a default citation link using reftex.
If you are on a link, it appends to the end of the link,
otherwise, a new link is inserted.  Use a prefix
arg (ALTERNATIVE-CITE) to get a menu of citation types."
  (interactive "P")
  (org-ref-find-bibliography)
  (let* ((object (org-element-context))
         (link-string-beginning (org-element-property :begin object))
         (link-string-end (org-element-property :end object))
         (path (org-element-property :path object)))

    (if (not alternative-cite)

        (cond
         ;; case where we are in a link
         ((and (equal (org-element-type object) 'link)
               (-contains? org-ref-cite-types (org-element-property :type object)))
          (goto-char link-string-end)
          ;; sometimes there are spaces at the end of the link
          ;; this code moves point pack until no spaces are there
          (skip-chars-backward " ")
          (insert (concat "," (mapconcat 'identity (reftex-citation t ?a) ","))))

         ;; We are next to a link, and we want to append
         ((save-excursion
            (backward-char)
            (and (equal (org-element-type (org-element-context)) 'link)
                 (-contains? org-ref-cite-types
                             (org-element-property :type (org-element-context)))))
          (skip-chars-backward " ")
          (insert (concat "," (mapconcat 'identity (reftex-citation t ?a) ","))))

         ;; insert fresh link
         (t
          (insert
           (concat org-ref-default-citation-link
                   ":"
                   (mapconcat 'identity (reftex-citation t) ",")))))

      ;; you pressed a C-u so we run this code
      (reftex-citation))))


(defun org-ref-insert-cite-with-completion (type)
  "Insert a cite link of TYPE with completion."
  (interactive (list (ido-completing-read "Type: " org-ref-cite-types)))
  (insert (funcall (intern (format "org-%s-complete-link" type)))))


(defun org-ref-store-bibtex-entry-link ()
  "Save a citation link to the current bibtex entry.  Save in the default link type."
  (interactive)
  (let ((link (concat org-ref-default-citation-link
                      ":"
                      (save-excursion
                        (bibtex-beginning-of-entry)
                        (reftex-get-bib-field "=key=" (bibtex-parse-entry))))))
    (message "saved %s" link)
    (push (list link) org-stored-links)
    (car org-stored-links)))

;; ** Index link
(org-add-link-type
 "index"
 (lambda (path)
   (occur path))

 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (format "\\index{%s}" path)))))

;; this will generate a temporary index of entries in the file.
(org-add-link-type
 "printindex"
 (lambda (path)
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
                     "[[elisp:(progn (switch-to-buffer \"%s\") (goto-char %s))][%s]]"
                     (current-buffer)
                     (plist-get plist :begin) ;; position of link
                     ;; grab a description
                     (save-excursion
                       (goto-char (plist-get plist :begin))
                       (if (thing-at-point 'sentence)
                           ;; get a sentence
                           (replace-regexp-in-string
                            "\n" "" (thing-at-point 'sentence))
                         ;; or call it a link
                         "link")))))))))

     ;; sort the links
     (setq *index-links* (cl-sort *index-links* 'string-lessp :key 'car))

     ;; now first letters
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
 ;; formatting
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (format "\\printindex")))))

;; ** Glossary link
(org-add-link-type
 "newglossaryentry"
 nil ;; no follow action
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (format "\\newglossaryentry{%s}{%s}" path desc)))))


;; link to entry
(org-add-link-type
 "gls"
 nil ;; no follow action
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (format "\\gls{%s}" path)))))

;; plural
(org-add-link-type
 "glspl"
 nil ;; no follow action
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (format "\\glspl{%s}" path)))))

;; capitalized link
(org-add-link-type
 "Gls"
 nil ;; no follow action
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (format "\\Gls{%s}" path)))))

;; capitalized link
(org-add-link-type
 "Glspl"
 nil ;; no follow action
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (format "\\Glspl{%s}" path)))))

;; * Utilities
;; ** create text citations from a bibtex entry
(defun org-ref-bib-citation ()
  "From a bibtex entry, create and return a simple citation string.
This assumes you are in an article."
  (bibtex-set-dialect nil t)
  (bibtex-beginning-of-entry)
  (let* ((cb (current-buffer))
         (bibtex-expand-strings t)
         (entry (cl-loop for (key . value) in (bibtex-parse-entry t)
                         collect (cons (downcase key) value)))
         (title (replace-regexp-in-string "\n\\|\t\\|\s+" " " (reftex-get-bib-field "title" entry)))
         (year  (reftex-get-bib-field "year" entry))
         (author (replace-regexp-in-string "\n\\|\t\\|\s+" " " (reftex-get-bib-field "author" entry)))
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
         (title (replace-regexp-in-string "\n\\|\t\\|\s+" " " (reftex-get-bib-field "title" entry)))
         (year  (reftex-get-bib-field "year" entry))
         (author (replace-regexp-in-string "\n\\|\t\\|\s+" " " (reftex-get-bib-field "author" entry)))
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

;; ** Open pdf in bibtex entry
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
           (pdf (format (concat (file-name-as-directory org-ref-pdf-directory) "%s.pdf") key)))
      (message "%s" pdf)
      (if (file-exists-p pdf)
          (org-open-link-from-string (format "[[file:%s]]" pdf))
        (ding)))))

;; ** Open notes from bibtex entry
(defun org-ref-open-bibtex-notes ()
  "From a bibtex entry, open the notes if they exist.
If the notes do not exist, then create a heading.

I never did figure out how to use reftex to make this happen
non-interactively.  the ‘reftex-format-citation’ function did not
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
         (key (reftex-get-bib-field "=key=" entry)))

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
      ;; put new entry in notes if we don't find it.
      (if (re-search-forward (format ":Custom_ID: %s$" key) nil 'end)
          (funcall org-ref-open-notes-function)
        ;; no entry found, so add one
        (insert (org-ref-reftex-format-citation
                 entry (concat "\n" org-ref-note-title-format)))
        (insert (format
                 "[[cite:%s]] [[file:%s/%s.pdf][pdf]]\n\n"
                 key (file-name-as-directory org-ref-pdf-directory) key))
        (save-buffer)))))


(defun org-ref-open-notes-from-reftex ()
  "Call reftex, and open notes for selected entry."
  (interactive)
  (let ((bibtex-key)))

  ;; now look for entry in the notes file
  (if  org-ref-bibliography-notes
      (find-file-other-window org-ref-bibliography-notes)
    (error "Org-ref-bib-bibliography-notes is not set to anything"))

  (org-open-link-from-string
   (format "[[#%s]]" (first (reftex-citation t))))
  (funcall org-ref-open-notes-function))


;; ** Open bibtex entry in browser
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


;; ** Build a pdf of the bibtex file
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

;; ** Extract bibtex entries in org-file

(defun org-ref-extract-bibtex-entries ()
  "Extract the bibtex entries in the current buffer into a src block.

If no bibliography is in the buffer the variable
`reftex-default-bibliography' is used."
  (interactive)
  (let* ((temporary-file-directory (file-name-directory (buffer-file-name)))
         (tempname (make-temp-file "extract-bib"))
         (contents (buffer-string))
         (cb (current-buffer))
         basename texfile bibfile results)

    ;; open tempfile and insert org-buffer contents
    (find-file tempname)
    (insert contents)
    (setq basename (file-name-sans-extension
                    (file-name-nondirectory buffer-file-name))
          texfile (concat tempname ".tex")
          bibfile (concat tempname ".bib"))

    ;; see if we have a bibliography, and insert the default one if not.
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward "^bibliography:" (point-max) 'end)
        (insert (format "\nbibliography:%s"
                        (mapconcat 'identity reftex-default-bibliography ",")))))
    (save-buffer)

    ;; get a latex file and extract the references
    (org-latex-export-to-latex)
    (find-file texfile)
    (reftex-parse-all)
    (reftex-create-bibtex-file bibfile)
    (save-buffer)
    ;; save results of the references
    (setq results (buffer-string))

    ;; kill buffers. these are named by basename, not full path
    (kill-buffer (concat basename ".bib"))
    (kill-buffer (concat basename ".tex"))
    (kill-buffer basename)

    (delete-file bibfile)
    (delete-file texfile)
    (delete-file tempname)

    ;; Now back to the original org buffer and insert the results
    (switch-to-buffer cb)
    (when (not (string= "" results))
      (save-excursion
        (goto-char (point-max))
        (insert "\n\n")
        (org-insert-heading)
        (insert (format " Bibtex entries

#+BEGIN_SRC text :tangle %s
%s
#+END_SRC" (concat (file-name-sans-extension (file-name-nondirectory (buffer-file-name))) ".bib") results))))))

;; ** Find bad citations
(defun org-ref-index (substring list)
  "Return the index of SUBSTRING in a LIST of strings."
  (let ((i 0)
        (found nil))
    (dolist (arg list i)
      (if (string-match (concat "^" substring "$") arg)
          (progn
            (setq found t)
            (return i)))
      (setq i (+ i 1)))
    ;; return counter if found, otherwise return nil
    (if found i nil)))


(defun org-ref-find-bad-citations ()
  "Create a list of citation keys that do not have a matching bibtex entry.
List is displayed in an `org-mode' buffer using the known bibtex
file.  Makes a new buffer with clickable links."
  (interactive)
  ;; generate the list of bibtex-keys and cited keys
  (let* ((bibtex-files (org-ref-find-bibliography))
         (bibtex-file-path (mapconcat (lambda (x) (file-name-directory (file-truename x))) bibtex-files ":"))
         (bibtex-keys (mapcar (lambda (x) (car x)) (bibtex-global-key-alist)))
         (bad-citations '()))

    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (let ((plist (nth 1 link)))
          (when (-contains? org-ref-cite-types (plist-get plist :type))
            (dolist (key (org-ref-split-and-strip-string (plist-get plist :path)))
              (when (not (org-ref-index key bibtex-keys))
                (setq
                 bad-citations
                 (append
                  bad-citations
                  `(,(format "%s [[elisp:(progn (switch-to-buffer-other-frame \"%s\")(goto-char %s))][not found here]]\n"
                             key
                             (buffer-name)
                             (plist-get plist :begin)))))
                )))))
      ;; set with-affilates to t to get citations in a caption
      nil nil nil t)

    (if bad-citations
        (progn
          (switch-to-buffer-other-window "*Missing citations*")
          (org-mode)
          (erase-buffer)
          (insert "* List of bad cite links\n")
          (insert (mapconcat 'identity bad-citations ""))
                                        ;(setq buffer-read-only t)
          (use-local-map (copy-keymap org-mode-map))
          (local-set-key "q" #'(lambda () (interactive) (kill-buffer))))

      (when (get-buffer "*Missing citations*")
        (kill-buffer "*Missing citations*"))
      (message "No bad cite links found"))))

;; ** helm interface to bad citations, labels, refs and files in orgfile
(defun org-ref-bad-cite-candidates ()
  "Return a list of conses (key . marker) where key does not exist in the known bibliography files, and marker points to the key."
  (let* ((cp (point))			; save to return to later
         (bibtex-files (org-ref-find-bibliography))
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
            (dolist (key (org-ref-split-and-strip-string (plist-get plist :path)))
              (when (not (org-ref-index key bibtex-keys))
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
                         labels) 1)
          ;; this is a multiply defined label.
          (let ((cp (point)))
            (goto-char (point-min))
            (while (re-search-forward
                    (format  "[^#+]label:%s\\s-" label) nil t)
              (push (cons label (point-marker)) multiple-labels))
            (goto-char (point-min))
            (while (re-search-forward
                    (format  "\\label{%s}\\s-?" label) nil t)
              (push (cons label (point-marker)) multiple-labels))

            (goto-char (point-min))
            (while (re-search-forward
                    (format  "^#\\+label:\\s-*%s" label) nil t)
              (push (cons label (point-marker)) multiple-labels))

            (goto-char (point-min))
            (while (re-search-forward
                    (format   "^#\\+tblname:\\s-*%s" label) nil t)
              (push (cons label (point-marker)) multiple-labels))
            (goto-char cp)))))
    multiple-labels))


(defun org-ref-bad-file-link-candidates ()
  "Return list of conses (link . marker) wehre the file in the link does not exist."
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
      (while (re-search-forward "\\attachfile{\\(.*\\)}" nil t)
        (unless (file-exists-p (match-string 1))
          (add-to-list 'bad-files (cons (match-string 1) (point-marker))))))
    bad-files))

;;;###autoload
(defun org-ref ()
  "Opens a helm interface to actions for ‘org-ref’.
Shows bad citations, ref links and labels"
  (interactive)
  (let ((cb (current-buffer))
        (bad-citations (org-ref-bad-cite-candidates))
        (bad-refs (org-ref-bad-ref-candidates))
        (bad-labels (org-ref-bad-label-candidates))
        (bad-files (org-ref-bad-file-link-candidates))
        (bib-candidates '()))

    ;; setup bib-candidates. This checks a variety of things in the bibliography, bibtex files.
    ;; check for which bibliographies are used
    (add-to-list
     'bib-candidates
     (cons (format  "Using these bibtex files: %s"
                    (org-ref-find-bibliography))
           (lambda () nil)))

    ;; Check bibliography style exists
    (save-excursion
      (goto-char 0)
      (unless (re-search-forward "bibliographystyle:\\|\\biblographystyle{" nil t)
        (add-to-list 'bib-candidates
                     (cons "No bibliographystyle found."
                           (lambda ()
                             (switch-to-buffer "*org-ref*")
                             (erase-buffer)
                             (insert "No bibliography style found. This may be ok, if your latex class style sets that up, but if not this is an error. Try adding something like:
bibliographystyle:unsrt
at the end of you file.
")
                             (org-mode)))
                     t)))

    ;; Check if latex knows of the bibliographystyle. We only check links here.
    ;;  I also assume this style exists as a bst file that kpsewhich can find.
    (save-excursion
      (goto-char 0)
      (when (re-search-forward "bibliographystyle:" nil t)
        ;; on a link. get style
        (let ((path (org-element-property :path (org-element-context))))
          (unless (= 0 (shell-command (format "kpsewhich %s.bst" path)))
            (add-to-list 'bib-candidates
                         (cons (format "bibliographystyle \"%s\" may be unknown" path)
                               (lambda ()
                                 (goto-char 0)
                                 (re-search-forward "bibliographystyle:"))))))))

    ;; check for multiple bibliography links
    (let* ((bib-links (-filter
                       (lambda (el)
                         (string= (org-element-property :type el) "bibliography"))
                       (org-element-map (org-element-parse-buffer) 'link 'identity)))
           (n-bib-links (length bib-links)))

      (when (> n-bib-links 1)
        (mapc (lambda (link)
                (setq
                 bib-candidates
                 (append
                  bib-candidates
                  (list (cons (format  "Multiple bibliography link: %s" (org-element-property :raw-link link))
                              `(lambda ()
                                 (goto-char ,(org-element-property :begin link))))))))
              bib-links)))

    ;; Check for bibliography files existence.
    (mapc (lambda (bibfile)
            (unless (file-exists-p bibfile)
              (add-to-list 'bib-candidates
                           (cons
                            (format "%s does not exist." bibfile)
                            (lambda ()
                              (message "Non-existent bibfile.")))
                           t)))
          (org-ref-find-bibliography))

    ;; check for spaces in bibliography
    (let ((bibfiles (mapcar 'expand-file-name
                            (org-ref-find-bibliography))))
      (mapc (lambda (bibfile)
              (when (string-match " " bibfile)
                (add-to-list
                 'bib-candidates
                 (cons (format "One or more spaces found in path to %s" bibfile)
                       (lambda ()
                         (message "No spaces are allowed in bibtex file paths. We recommend replacing them with -. Underscores usually cause other problems if you don't know what you are doing.")))
                 t)))
            bibfiles))

    ;; validate bibtex files
    (let ((bibfiles (mapcar 'expand-file-name
                            (org-ref-find-bibliography))))
      (mapc
       (lambda (bibfile)
         (unless (with-current-buffer
                     (find-file-noselect bibfile)
                   (bibtex-validate))
           (add-to-list 'bib-candidates
                        (cons
                         (format  "Invalid bibtex file found. %S" bibfile)
                         `(lambda ()
                            (find-file ,bibfile)))
                        t)))
       bibfiles))


    (helm :sources `(((name . "Bad citations")
                      (candidates . ,bad-citations)
                      (action . (lambda (marker)
                                  (switch-to-buffer (marker-buffer marker))
                                  (goto-char marker))))
                     ;;
                     ((name . "Bad Labels")
                      (candidates . ,bad-labels)
                      (action . (lambda (marker)
                                  (switch-to-buffer (marker-buffer marker))
                                  (goto-char marker))))
                     ;;
                     ((name . "Bad ref links")
                      (candidates . ,bad-refs)
                      (action . (lambda (marker)
                                  (switch-to-buffer (marker-buffer marker))
                                  (goto-char marker))))
                     ;;
                     ((name . "Bad file links")
                      (candidates . ,bad-files)
                      (lambda (marker)
                        (switch-to-buffer (marker-buffer marker))
                        (goto-char marker)))

                     ((name . "Bibliography")
                      (candidates . ,bib-candidates)
                      (action . (lambda (x)
                                  (switch-to-buffer ,cb)
                                  (funcall x))))
                     ;;
                     ((name . "Utilities")
                      (candidates . (("Check buffer again" . org-ref)
                                     ("Insert citation" . helm-bibtex)
                                     ("Insert label link" . org-ref-helm-insert-label-link)
                                     ("Insert ref link" . org-ref-helm-insert-ref-link)
                                     ("List of figures" . org-ref-list-of-figures)
                                     ("List of tables" . org-ref-list-of-tables)
                                     ("Table of contents" . helm-org-in-buffer-headings)))
                      (action . (lambda (x)
                                  (switch-to-buffer ,cb)
                                  (funcall x))))
                     ;;
                     ((name . "Document utilities")
                      (candidates . (("Spell check document" . ispell)))
                      (action . (lambda (x)
                                  (switch-to-buffer ,cb)
                                  (funcall x))))
                     ;; Exports
                     ((name . "Export functions")
                      (candidates . (("Extract cited entries" . org-ref-extract-bibtex-entries)
                                     ("Export to html and open" . (lambda ()
                                                                    (org-open-file
                                                                     (org-html-export-to-html))))
                                     ("Export to pdf and open" . (lambda ()
                                                                   (org-open-file
                                                                    (org-latex-export-to-pdf))))
                                     ("Export to manuscript pdf and open" . ox-manuscript-export-and-build-and-open)
                                     ("Export submission manuscript pdf and open" . ox-manuscript-build-submission-manuscript-and-open)

                                     ))
                      (action . (lambda (x)
                                  (switch-to-buffer ,cb)
                                  (funcall x))))))))

;; ** Find non-ascii charaters
(defun org-ref-find-non-ascii-characters ()
  "Find non-ascii characters in the buffer.  Useful for cleaning up bibtex files."
  (interactive)
  (occur "[^[:ascii:]]"))


;; ** Sort fields in a bibtex entry
(defun org-ref-sort-bibtex-entry ()
  "Sort fields of entry in standard order and downcase them."
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((master '("author" "title" "journal" "volume" "number" "pages" "year" "doi" "url"))
         (entry (bibtex-parse-entry))
         (entry-fields)
         (other-fields)
         (type (cdr (assoc "=type=" entry)))
         (key (cdr (assoc "=key=" entry))))

    ;; these are the fields we want to order that are in this entry
    (setq entry-fields (mapcar (lambda (x) (car x)) entry))
    ;; we do not want to reenter these fields
    (setq entry-fields (remove "=key=" entry-fields))
    (setq entry-fields (remove "=type=" entry-fields))

    ;;these are the other fields in the entry
    (setq other-fields (-remove (lambda(x) (member x master)) entry-fields))

    (cond
     ;; right now we only resort articles
     ((string= (downcase type) "article")
      (bibtex-kill-entry)
      (insert
       (concat "@article{" key ",\n"
               (mapconcat
                (lambda (field)
                  (when (member field entry-fields)
                    (format "%s = %s," (downcase field) (cdr (assoc field entry))))) master "\n")
               (mapconcat
                (lambda (field)
                  (format "%s = %s," (downcase field) (cdr (assoc field entry)))) other-fields "\n")
               "\n}\n\n"))
      (bibtex-find-entry key)
      (bibtex-fill-entry)
      (bibtex-clean-entry)
      ))))


;; ** Clean a bibtex entry
(defun org-ref-clean-bibtex-entry(&optional keep-key)
  "Clean and replace the key in a bibtex function.
When keep-key is t, do not replace it. You can use a prefix to
specify the key should be kept"
  (interactive "P")
  (bibtex-beginning-of-entry)
  (end-of-line)
  ;; some entries do not have a key or comma in first line. We check and add it, if needed.
  (unless (string-match ",$" (thing-at-point 'line))
    (end-of-line)
    (insert ","))

  ;; check for empty pages, and put eid or article id in its place
  (let ((entry (bibtex-parse-entry))
        (pages (bibtex-autokey-get-field "pages"))
        (year (bibtex-autokey-get-field "year"))
        (doi  (bibtex-autokey-get-field "doi"))
        ;; The Journal of Chemical Physics uses eid
        (eid (bibtex-autokey-get-field "eid")))

    ;; replace http://dx.doi.org/ in doi. some journals put that in,
    ;; but we only want the doi.
    (when (string-match "^http://dx.doi.org/" doi)
      (bibtex-beginning-of-entry)
      (goto-char (car (cdr (bibtex-search-forward-field "doi" t))))
      (bibtex-kill-field)
      (bibtex-make-field "doi")
      (backward-char)
      (insert (replace-regexp-in-string "^http://dx.doi.org/" "" doi)))

    ;; asap articles often set year to 0, which messes up key
    ;; generation. fix that.
    (when (string= "0" year)
      (bibtex-beginning-of-entry)
      (goto-char (car (cdr (bibtex-search-forward-field "year" t))))
      (bibtex-kill-field)
      (bibtex-make-field "year")
      (backward-char)
      (insert (read-string "Enter year: ")))

    ;; fix pages if they are empty if there is an eid to put there.
    (when (string= "-" pages)
      (when eid
        (bibtex-beginning-of-entry)
        ;; this seems like a clunky way to set the pages field.But I
        ;; cannot find a better way.
        (goto-char (car (cdr (bibtex-search-forward-field "pages" t))))
        (bibtex-kill-field)
        (bibtex-make-field "pages")
        (backward-char)
        (insert eid)))

    ;; replace naked & with \&
    (save-restriction
      (bibtex-narrow-to-entry)
      (bibtex-beginning-of-entry)
      (message "checking &")
      (while (re-search-forward " & ")
        (replace-match " \\\\& "))
      (widen))

    ;; generate a key, and if it duplicates an existing key, edit it.
    (unless keep-key
      (let ((key (bibtex-generate-autokey)))
        ;; ;; sometimes there are latex sequences in the key. We remove them. A
        ;; ;; better approach might be to have some lookup table, but this would
        ;; ;; somewhat duplicate `jmax-nonascii-latex-replacements', in an ascii
        ;; ;; version.
        ;; (loop for c in '("{" "}" "'" "\\" "\"" "`" "~")
        ;;       do
        ;;       (setq key (replace-regexp-in-string
        ;;		 (regexp-quote c) key)))

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
        (kill-new key))) ;; save key for pasting

    ;; run hooks. each of these operates on the entry with no arguments.
    ;; this did not work like  i thought, it gives a symbolp error.
    ;; (run-hooks org-ref-clean-bibtex-entry-hook)
    (mapc (lambda (x)
            (save-restriction
              (save-excursion
                (funcall x))))
          org-ref-clean-bibtex-entry-hook)

    ;; sort fields within entry
    (org-ref-sort-bibtex-entry)
    ;; check for non-ascii characters
    (occur "[^[:ascii:]]")))


(defun org-ref-get-citation-year (key)
  "Get the year of an entry with KEY.  Return year as a string."
  (let* ((results (org-ref-get-bibtex-key-and-file key))
         (bibfile (cdr results)))
    (with-temp-buffer
      (insert-file-contents bibfile)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (bibtex-search-entry key nil 0)
      (prog1 (reftex-get-bib-field "year" (bibtex-parse-entry t))
        ))))

;; ** Sort cite in cite link
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

;; ** Shift-arrow sorting of keys in a cite link
(defun org-ref-swap-keys (i j keys)
  "Swap index I and J in the list KEYS."
  (let ((tempi (nth i keys)))
    (setf (nth i keys) (nth j keys))
    (setf (nth j keys) tempi))
  keys)


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
      (setq i (org-ref-index key keys))  ;; defined in org-ref
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

;; ** context around org-ref links
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
      (throw 'result "!!! NO CONTEXT FOUND !!!"))))


(defun org-ref-link-message ()
  "Print a minibuffer message about the link that point is on."
  (interactive)
  ;; the way links are recognized in org-element-context counts a blank space
  ;; after a link and the closing brackets in literal links. We don't try to get
  ;; a message if the cursor is on those.
  (when (not (or (looking-at " ")
		 (looking-at "]")))

    (save-restriction
      (widen)
      (when (eq major-mode 'org-mode)
        (let* ((object (org-element-context))
               (type (org-element-property :type object)))
          ;; (message-box "%s-%s %s" (org-element-property :begin object)
          ;;	     (org-element-property :end object)
          ;;	     (point))
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
                          " occurence"
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

;; ** aliases
(defalias 'oro 'org-ref-open-citation-at-point)
(defalias 'orc 'org-ref-citation-at-point)
(defalias 'orp 'org-ref-open-pdf-at-point)
(defalias 'oru 'org-ref-open-url-at-point)
(defalias 'orn 'org-ref-open-notes-at-point)
(defalias 'ornr 'org-ref-open-notes-from-reftex)

(defalias 'orib 'org-ref-insert-bibliography-link)
(defalias 'oric 'org-ref-insert-cite-link)
(defalias 'orir 'org-ref-insert-ref-link)
(defalias 'orsl 'org-ref-store-bibtex-entry-link)

(defalias 'orcb 'org-ref-clean-bibtex-entry)

;; * Helm bibtex setup
(setq helm-bibtex-additional-search-fields '(keywords))

(defun helm-bibtex-candidates-formatter (candidates source)
  "Formats BibTeX entries for display in results list.
Argument CANDIDATES helm candidates.
Argument SOURCE the helm source.

Adapted from the function in ‘helm-bibtex’ to include additional
fields, the keywords I think."
  (cl-loop
   with width = (with-helm-window (helm-bibtex-window-width))
   for entry in candidates
   for entry = (cdr entry)
   for entry-key = (helm-bibtex-get-value "=key=" entry)
   if (assoc-string "author" entry 'case-fold)
   for fields = '("author" "title"  "year" "=has-pdf=" "=has-note=" "=type=")
   else
   for fields = '("editor" "title" "year" "=has-pdf=" "=has-note=" "=type=")
   for fields = (--map (helm-bibtex-clean-string
                        (helm-bibtex-get-value it entry " "))
                       fields)
   for fields = (-update-at 0 'helm-bibtex-shorten-authors fields)
   for fields = (append fields
                        (list (or (helm-bibtex-get-value "keywords" entry)
                                  "")))
   collect
   (cons (s-format "$0 $1 $2 $3 $4$5 $6" 'elt
                   (-zip-with (lambda (f w) (truncate-string-to-width f w 0 ?\s))
                              fields (list 36 (- width 85) 4 1 1 7 7)))
         entry-key)))

;; * org-ref bibtex keywords
;; adapted from bibtex-utils.el
;; these are candidates for selecting keywords/tags
(defun org-ref-bibtex-keywords ()
  "Get keywords defined in current bibtex file.
These are in the keywords field, and are comma or semicolon separated."
  (save-excursion
    (goto-char (point-min))
    (let (keywords kstring)
      (while (re-search-forward "^\\s-*keywords.*{\\([^}]+\\)}" nil t)
        ;; TWS - remove newlines/multiple spaces:
        (setq kstring (replace-regexp-in-string "[ \t\n]+" " " (match-string 1)))
        (mapc
         (lambda (v)
           (add-to-list 'keywords v t))
         (split-string kstring "\\(,\\|;\\)[ \n]*\\|{\\|}" t)))
      keywords)))


(defun org-ref-set-bibtex-keywords (keywords &optional arg)
  "Add KEYWORDS to a bibtex entry.
If KEYWORDS is a list, it is converted to a comma-separated
string.  The KEYWORDS are added to the beginning of the
field.  Otherwise KEYWORDS should be a string of comma-separate
keywords.  Optional argument ARG prefix arg to replace keywords."
  (interactive "sKeywords: \nP")
  (bibtex-set-field
   "keywords"
   (if arg
       ;; replace with arg
       (if (listp keywords)
           (mapconcat 'identity keywords ", ")
         keywords)
     ;; else concatentate
     (concat
      (if (listp keywords)
          (mapconcat 'identity keywords ", ")
        keywords)
      (when (not (string= "" (bibtex-autokey-get-field "keywords")))
        (concat ", "  (bibtex-autokey-get-field "keywords"))))))
  (save-buffer))


(defun helm-tag-bibtex-entry ()
  "Helm interface to add keywords to a bibtex entry.
Run this with the point in a bibtex entry."
  (interactive)
  (let ((keyword-source `((name . "Existing keywords")
                          (candidates . ,(org-ref-bibtex-keywords))
                          (action . (lambda (candidate)
                                      (org-ref-set-bibtex-keywords
                                       (mapconcat
                                        'identity
                                        (helm-marked-candidates)
                                        ", "))))))
        (fallback-source `((name . "Add new keywords")
                           (dummy)
                           (action . (lambda (candidate)
                                       (org-ref-set-bibtex-keywords helm-pattern)
                                       )))))
    (helm :sources '(keyword-source fallback-source))))


(defun org-ref-helm-tag-entries (candidates)
  "Set tags on selected bibtex entries from `helm-bibtex'.
User is prompted for tags.  This function is called from `helm-bibtex'.
Argument CANDIDATES helm candidates."
  (message "")
  (let ((keywords (read-string "Keywords (comma separated): ")))
    (cl-loop for key in (helm-marked-candidates)
             do
             (save-window-excursion
               (helm-bibtex-show-entry key)
               (bibtex-set-field
                "keywords"
                (concat
                 keywords
                 ", " (bibtex-autokey-get-field "keywords")))
               (save-buffer)))))




;; Add a new action
(helm-add-action-to-source
 "Add keywords to entries"
 'org-ref-helm-tag-entries
 helm-source-bibtex)


(defun org-ref-helm-bibtex-format-org (keys)
  "Insert selected KEYS as cite link.
Append KEYS if you are on a link.

Technically, this function should return a string that is
inserted by helm.  This function does the insertion and gives helm
an empty string to insert.  This lets us handle appending to a
link properly.

In the helm-bibtex buffer, \\[universal-argument] will give you a helm menu to
select a new link type for the selected entries.

A double \\[universal-argument] \\[universal-argument] will
change the key at point to the selected keys."
  (let* ((object (org-element-context))
         (last-char (save-excursion
                      (when (org-element-property :end object)
                        (goto-char (org-element-property :end object))
                        (unless (bobp)
                          (backward-char))
                        (if (looking-at " ")
                            " "
                          "")))))
    (cond
     ;; case where we are in a link
     ((and (equal (org-element-type object) 'link)
           (-contains?
            org-ref-cite-types
            (org-element-property :type object)))
      (cond
       ;; no prefix. append keys
       ((equal helm-current-prefix-arg nil)
        (goto-char (org-element-property :end object))
        (skip-chars-backward " ")
        (insert (concat "," (mapconcat 'identity keys ","))))
       ;; double prefix, replace key at point
       ((equal helm-current-prefix-arg '(16))
        (setf (buffer-substring
               (org-element-property :begin object)
               (org-element-property :end object))
              (concat
               (replace-regexp-in-string
                (car (org-ref-get-bibtex-key-and-file)) ; key
                (mapconcat 'identity keys ",")		; new keys
                (org-element-property :raw-link object))
               ;; replace space at end to avoid collapsing into next word.
               last-char))
        ;; and we want to go to the end of the new link
        (goto-char
         (org-element-property :end (org-element-context))))
       (t
        (message "Not found"))))

     ;; We are next to a link, and we want to append
     ;; next to a link means one character back is on a link.
     ((save-excursion
        (unless (bobp) (backward-char))
        (and (equal (org-element-type (org-element-context)) 'link)
             (-contains?
              org-ref-cite-types
              (org-element-property :type (org-element-context)))))
      (skip-chars-backward " ")
      (insert (concat "," (mapconcat 'identity keys ","))))

     ;; insert fresh link
     (t
      ;;(message-box "fresh link")
      (insert
       (concat (if (equal helm-current-prefix-arg '(4))
                   (helm :sources `((name . "link types")
                                    (candidates . ,org-ref-cite-types)
                                    (action . (lambda (x) x))))
                 org-ref-default-citation-link)
               ":"
               (s-join "," keys))))))
  ;; return empty string for helm
  "")


;;;###autoload
(defun org-ref-helm-insert-cite-link (arg)
  "Insert a citation link with ‘helm-bibtex’.
With one prefix ARG, insert a ref link.
With two prefix ARGs, insert a label link."
  (interactive "P")
  ;; save all bibtex buffers so we get the most up-to-date selection. I find
  ;; that I often edit a bibliography and forget to save it, so the newest entry
  ;; does not show in helm-bibtex.
  (loop for buffer in (buffer-list)
        do
        (with-current-buffer buffer
          (when (and (buffer-file-name) (f-ext? (buffer-file-name) "bib"))
            (save-buffer))))
  (cond
   ((equal arg nil)
    (let ((helm-bibtex-bibliography (org-ref-find-bibliography)))
      (helm-bibtex)))
   ((equal arg '(4))
    (org-ref-helm-insert-ref-link))
   ((equal arg '(16))
    (org-ref-helm-insert-label-link))))


;; add our own fallback entries where we want them. These appear in reverse order of adding in the menu
(setq helm-bibtex-fallback-options
      (-insert-at 1 '("Crossref" . "http://search.crossref.org/?q=%s") helm-bibtex-fallback-options))


(setq helm-bibtex-fallback-options
      (-insert-at
       1
       '("Scopus" . "http://www.scopus.com/scopus/search/submit/xadvanced.url?searchfield=TITLE-ABS-KEY(%s)")
       helm-bibtex-fallback-options))


(setq helm-bibtex-fallback-options
      (-insert-at 1 '("WOS" . "http://gateway.webofknowledge.com/gateway/Gateway.cgi?topic=%s&GWVersion=2&SrcApp=WEB&SrcAuth=HSB&DestApp=UA&DestLinkType=GeneralSearchSummary") helm-bibtex-fallback-options))


(defun org-ref-get-citation-string-at-point ()
  "Get a string of a formatted citation."
  (let* ((results (org-ref-get-bibtex-key-and-file))
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


(defun org-ref-cite-candidates ()
  "Generate the list of possible candidates for click actions on a cite link.
Checks for pdf and doi, and add appropriate functions."
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (pdf-file (funcall org-ref-get-pdf-filename-function key))
         (bibfile (cdr results))
         (url (save-excursion
                (with-temp-buffer
                  (insert-file-contents bibfile)
                  (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
                  (bibtex-search-entry key)
                  (bibtex-autokey-get-field "url"))))
         (doi (save-excursion
                (with-temp-buffer
                  (insert-file-contents bibfile)
                  (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
                  (bibtex-search-entry key)
                  ;; I like this better than bibtex-url which does not always find
                  ;; the urls
                  (bibtex-autokey-get-field "doi"))))
         (candidates `(("Quit" . org-ref-citation-at-point)
                       ("Open bibtex entry" . org-ref-open-citation-at-point))))
    ;; for some reason, when there is no doi or url, they are returned as "". I
    ;; prefer nil so we correct this here.
    (when (string= doi "") (setq doi nil))
    (when (string= url "") (setq url nil))

    ;; Conditional pdf functions
    (if (file-exists-p pdf-file)
        (add-to-list
         'candidates
         '("Open pdf" . org-ref-open-pdf-at-point)
         t)
      (add-to-list
       'candidates
       '("Try to get pdf" . (lambda ()
                              (save-window-excursion
                                (org-ref-open-citation-at-point)
                                (bibtex-beginning-of-entry)
                                (doi-utils-get-bibtex-entry-pdf))))
       t))


    (add-to-list
     'candidates
     '("Open notes" . org-ref-open-notes-at-point)
     t)

    ;; conditional url and doi functions
    (when (or url doi)
      (add-to-list
       'candidates
       '("Open in browser" . org-ref-open-url-at-point)
       t))

    (when doi
      (mapc (lambda (x)
              (add-to-list 'candidates x t))
            `(("WOS" . org-ref-wos-at-point)
              ("Related articles in WOS" . org-ref-wos-related-at-point)
              ("Citing articles in WOS" . org-ref-wos-citing-at-point)
              ("Google Scholar" . org-ref-google-scholar-at-point)
              ("Pubmed" . org-ref-pubmed-at-point)
              ("Crossref" . org-ref-crossref-at-point))))

    (add-to-list
     'candidates
     '("Insert new citation" . (lambda ()
                                 (org-ref-helm-insert-cite-link nil)))
     t)

    (add-to-list
     'candidates
     '("Delete key at point" . (lambda ()
                                 (let ((key (org-ref-get-bibtex-key-under-cursor)))
                                   (re-search-backward ":")
                                   (re-search-forward (concat key ",?"))
                                   (setf (buffer-substring
                                          (match-beginning 0)
                                          (match-end 0))
                                         ""))))
     t)

    ;;  This is kind of clunky. We store the key at point. Add the new ref. Get
    ;;  it off the end, and put it in the original position.
    (add-to-list
     'candidates
     '("Replace key at point" . (lambda ()
                                  (let ((key (org-ref-get-bibtex-key-under-cursor)))
                                    ;; add new citation
                                    (save-excursion
                                      (org-ref-helm-insert-cite-link nil))
                                    (let*  ((object (org-element-context))
                                            (type (org-element-property :type object))
                                            (begin (org-element-property :begin object))
                                            (end (org-element-property :end object))
                                            (link-string (org-element-property :path object))
                                            key keys i last-key)

                                      (setq
                                       key (org-ref-get-bibtex-key-under-cursor)
                                       keys (org-ref-split-and-strip-string link-string)
                                       i (org-ref-index key keys)
                                       last-key (car (reverse keys)))

                                      (setq keys (-remove-at i keys))
                                      (setq keys (-insert-at i last-key (butlast keys)))

                                      (cl--set-buffer-substring
                                       begin end
                                       (concat
                                        type ":" (mapconcat 'identity keys ",")
                                        ;; It seems the space at the end can get consumed, so we see if there
                                        ;; is a space, and add it if so. Sometimes there is a comma or period,
                                        ;; then we do not want a space.
                                        (when
                                            (save-excursion
                                              (goto-char end)
                                              (looking-back " ")) " ")))))))
     t)

    (add-to-list
     'candidates
     '("Delete citation at point" . (lambda ()
                                      (let*  ((object (org-element-context))
                                              (type (org-element-property :type object))
                                              (begin (org-element-property :begin object))
                                              (end (org-element-property :end object)))
                                        (cl--set-buffer-substring
                                         begin end
                                         ""))))
     t)

    (add-to-list
     'candidates
     '("Sort keys by year" . org-ref-sort-citation-link)
     t)

    (add-to-list
     'candidates
     '("Copy formatted citation to clipboard" . org-ref-copy-entry-as-summary)
     t)

    (add-to-list
     'candidates
     '("Copy key to clipboard" . (lambda ()
                                   (kill-new
                                    (car (org-ref-get-bibtex-key-and-file)))))
     t)

    (add-to-list
     'candidates
     '("Copy bibtex entry to file" . org-ref-copy-entry-at-point-to-file)
     t)

    (add-to-list
     'candidates
     '("Email bibtex entry and pdf" . (lambda ()
                                        (save-excursion
                                          (org-ref-open-citation-at-point)
                                          (email-bibtex-entry))))
     t)

    ;; add Scopus functions. These work by looking up a DOI to get a Scopus
    ;; EID. This may only work for Scopus articles. Not all DOIs are recognized
    ;; in the Scopus API. We only load these if you have defined a
    ;; `*scopus-api-key*', which is required to do the API queries. See
    ;; `scopus'. These functions are appended to the candidate list.
    (when (and (boundp '*scopus-api-key*) *scopus-api-key*)
      (add-to-list
       'candidates
       '("Open in Scopus" . (lambda ()
                              (let ((eid (scopus-doi-to-eid (org-ref-get-doi-at-point))))
                                (if eid
                                    (scopus-open-eid eid)
                                  (message "No EID found.")))))
       t)

      (add-to-list
       'candidates
       '("Scopus citing articles" . (lambda ()
                                      (let ((url (scopus-citing-url
                                                  (org-ref-get-doi-at-point))))
                                        (if url
                                            (browse-url url)
                                          (message "No url found.")))))
       t)

      (add-to-list
       'candidates
       '("Scopus related by authors" . (lambda ()
                                         (let ((url (scopus-related-by-author-url
                                                     (org-ref-get-doi-at-point))))
                                           (if url
                                               (browse-url url)
                                             (message "No url found.")))))
       t)

      (add-to-list
       'candidates
       '("Scopus related by references" . (lambda ()
                                            (let ((url (scopus-related-by-references-url
                                                        (org-ref-get-doi-at-point))))
                                              (if url
                                                  (browse-url url)
                                                (message "No url found.")))))
       t)

      (add-to-list
       'candidates
       '("Scopus related by keywords" . (lambda ()
                                          (let ((url (scopus-related-by-keyword-url
                                                      (org-ref-get-doi-at-point))))
                                            (if url
                                                (browse-url url)
                                              (message "No url found.")))))
       t))

    ;; finally return a numbered list of the candidates
    (cl-loop for i from 0
             for cell in candidates
             collect (cons (format "%2s. %s" i (car cell))
                           (cdr cell)))))


(defvar org-ref-helm-user-candidates '()
  "List of user-defined candidates to act when clicking on a cite link.
This is a list of cons cells '((\"description\" . action)). The
action function should not take an argument, and should assume
point is on the cite key of interest.")

;; example of adding your own function
(add-to-list
 'org-ref-helm-user-candidates
 '("Open pdf in emacs" . (lambda ()
                           (find-file
                            (concat
                             (file-name-as-directory org-ref-pdf-directory)
                             (car (org-ref-get-bibtex-key-and-file))
                             ".pdf"))))
 t)

;;;###autoload
(defun org-ref-cite-click-helm (key)
  "Open helm for actions on a cite link.
subtle points.

1. get name and candidates before entering helm because we need
the org-buffer.

2. switch back to the org buffer before evaluating the
action.  most of them need the point and buffer.

KEY is returned for the selected item(s) in helm."
  (interactive)
  (let ((name (org-ref-get-citation-string-at-point))
        (candidates (org-ref-cite-candidates))
        (cb (current-buffer)))

    (helm :sources `(((name . ,name)
                      (candidates . ,candidates)
                      (action . (lambda (f)
                                  (switch-to-buffer cb)
                                  (funcall f))))
                     ((name . "User functions")
                      (candidates . ,org-ref-helm-user-candidates)
                      (action . (lambda (f)
                                  (switch-to-buffer cb)
                                  (funcall f))))))))

;; * Hydra menus in org-ref
(when (featurep 'hydra)
  (require 'hydra)
  (setq hydra-is-helpful t)

  (defhydra org-ref-cite-hydra (:color blue)
    "
_p_: Open pdf     _w_: WOS          _g_: Google Scholar _K_: Copy citation to clipboard
_u_: Open url     _r_: WOS related  _P_: Pubmed         _k_: Copy key to clipboard
_n_: Open notes   _c_: WOS citing   _C_: Crossref       _f_: Copy bibtex entry to file
_o_: Open entry   _e_: Email entry and pdf
"
    ("o" org-ref-open-citation-at-point nil)
    ("p" org-ref-open-pdf-at-point nil)
    ("n" org-ref-open-notes-at-point nil)
    ("u" org-ref-open-url-at-point nil)
    ("w" org-ref-wos-at-point nil)
    ("r" org-ref-wos-related-at-point nil)
    ("c" org-ref-wos-citing-at-point nil)
    ("g" org-ref-google-scholar-at-point nil)
    ("P" org-ref-pubmed-at-point nil)
    ("C" org-ref-crossref-at-point nil)
    ("K" org-ref-copy-entry-as-summary nil)
    ("k" (progn
           (kill-new
            (car (org-ref-get-bibtex-key-and-file))))
     nil)
    ("f" org-ref-copy-entry-at-point-to-file nil)

    ("e" (save-excursion
           (org-ref-open-citation-at-point)
           (email-bibtex-entry))
     nil)))

;; * org-ref-help
(defun org-ref-help ()
  "Open the ‘org-ref’ manual."
  (interactive)
  (find-file (expand-file-name
              "org-ref.org"
              (file-name-directory
               (find-library-name "org-ref")))))

;; * org-ref menu
(defun org-ref-org-menu ()
  "Add ‘org-ref’ menu to the Org menu."

  (easy-menu-change
   '("Org") "org-ref"
   '( ["Insert citation" org-ref-helm-insert-cite-link]
      ["Insert ref" org-ref-helm-insert-ref-link]
      ["Insert label" org-ref-helm-insert-label-link]
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

;; * The end
(provide 'org-ref)

;;; org-ref.el ends here
