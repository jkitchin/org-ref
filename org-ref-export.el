;;; org-ref-export.el --- org-ref-export library -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Keywords: convenience

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
;;
;;; Commentary:
;; This is an export library that uses CSL to process the citations and bibliography.
;;
;; It is intended for non-LaTeX exports, but you can also use it with LaTeX if
;; you want to avoid using bibtex/biblatex for some reason.
;;
;; The default style is set by a CSL-STYLE keyword or
;; `org-ref-csl-default-style'. If this is an absolute or relative path that
;; exists, it is used. Otherwise, it looks in `org-cite-csl-styles-dir' from
;; `org-cite' if it is defined, and in the citeproc/csl-styles directory in
;; `org-ref' otherwise.
;;
;; The default locale is set by a CSL-LOCALE keyword or
;; `org-ref-csl-default-locale'. This is looked for in
;; `org-cite-csl-locales-dir' if it is defined, and otherwise in the csl-locales
;; directory of `org-ref'.
;;
;; Note that citeproc does not do anything for cross-references, so if non-LaTeX
;; export is your goal, you may want to use org-ref-refproc.el to handle
;; cross-references.
;;
;;; Formatting Limitations:
;;
;; Different export backends have different formatting capabilities. HTML and
;; LaTeX support full CSL formatting including small caps, italics, bold, etc.
;;
;; The org-mode backend has limitations because org-mode syntax does not support
;; small caps. Many CSL citation styles use small caps for author names, so these
;; will not render correctly when exporting to formats that use the org backend
;; (like DOCX by default).
;;
;; For DOCX export with better formatting support, set
;; `org-ref-docx-use-html-backend' to t. This causes citations to be rendered
;; as HTML which pandoc can convert to DOCX while preserving formatting.
;;
;; See `org-ref-backend-csl-formats' to customize backend mappings for different
;; export formats.
;;

;;; Code:
(require 'transient)

(defvar hfy-user-sheet-assoc)  		; to quiet compiler

(require 'ox-org)
(when (executable-find "pandoc")
  (ignore-errors (require 'ox-pandoc)))

(ignore-errors (require 'citeproc))

(defvar org-cite-csl-styles-dir)

(defcustom org-ref-backend-csl-formats
  '((html . html)
    (latex . latex)
    (md . plain)
    (org . org)
    (ascii . plain)
    (odt . org-odt)
    (docx . org))
  "Mapping of export backend to csl-backends.

Each entry maps an org export backend symbol to a citeproc-el backend
format used for rendering citations and bibliographies.

Note: The 'org backend has formatting limitations because org-mode
syntax does not support small caps. CSL styles that use small caps
(common for author names) will not render correctly when using the
'org backend. For DOCX export, consider setting
`org-ref-docx-use-html-backend' to t to use HTML formatting which
pandoc can convert while preserving small caps and other formatting.

Available citeproc backends:
- html: Full HTML formatting with CSS styles
- latex: Full LaTeX formatting with commands like \\textsc{}
- org: Org-mode markup (limited, no small caps support)
- org-odt: ODT-specific XML formatting
- plain: Plain text with no formatting"
  :type '(alist :key-type (symbol) :value-type (symbol))
  :group 'org-ref)


(defcustom org-ref-cite-internal-links 'auto
  "Should be one of these symbols (quoted)
- bib-links :: link cites to bibliography entries
- no-links :: do not link cites to bibliography entries
- nil or auto :: add links based on the style."
  :type '(choice symbol (sexp :tag bib-links)
		 symbol (sexp :tag no-links)
		 symbol (sexp :tag auto-links)
		 symbol (sexp :tag nil))
  :group 'org-ref)


(defcustom org-ref-csl-default-style "chicago-author-date-16th-edition.csl"
  "Default csl style to use.
Should be a csl filename, or an absolute path to a csl filename."
  :type 'string
  :group 'org-ref)


(defcustom org-ref-csl-default-locale "en-US"
  "Default csl locale to use."
  :type 'string
  :group 'org-ref)


(defcustom org-ref-docx-use-html-backend nil
  "Use HTML backend for DOCX export to preserve CSL formatting.

When non-nil, citations exported to DOCX will be rendered using the
HTML backend, which preserves formatting like small caps and italics
that pandoc can convert to DOCX.

When nil (default), uses the org backend which has limited formatting
support due to org-mode syntax limitations. Specifically, org-mode has
no native syntax for small caps, so CSL styles that use small caps
(common for author names in many citation styles) will not render
correctly in DOCX output.

Note: This option only affects DOCX export via pandoc. Other export
formats (HTML, LaTeX, ODT) are not affected.

See also `org-ref-backend-csl-formats' to customize backend mappings
for other export formats.

Related to issue #981: https://github.com/jkitchin/org-ref/issues/981"
  :type 'boolean
  :group 'org-ref)


(defcustom org-ref-csl-label-aliases
  '((("app" "apps") . "appendix")
    (("art" "arts") . "article-locator")
    (("bk" "bks") . "book")
    (("can") . "canon")
    (("ch" "chap" "chaps" "chapt") . "chapter")
    (("col" "cols") . "column")
    (("el") . "elocation")
    (("eq" "eqs") . "equation")
    (("fig" "figs") . "figure")
    (("fol" "fols") . "folio")
    (("iss") . "issue")
    (("l" "ll") . "line")
    (("n" "nn") . "note")
    ;; number is not listed in the url in the docstring
    (("no" "nos" "#") . "number")
    (("op" "opp") . "opus")
    (("p" "pp" "pg" "pgs") . "page")
    (("para" "paras" "¶" "¶¶" "§" "§§") . "paragraph")
    (("pt" "pts") . "part")
    (("sec" "secs") . "section")
    (("s.v" "s.vv") . "sub verbo")
    (("sup" "supp") . "supplement")
    (("tab" "tabs") . "table")
    (("ts") . "timestamp")
    (("ti" "tit") . "title")
    (("v" "vv") . "verse")
    (("vol" "vols") . "volume"))
  "A-list of aliases for a csl label.
The car is a list of possible aliases (including if they end in a .
This list was adapted from `org-cite-csl--label-alist'.
See https://github.com/citation-style-language/documentation/blob/master/specification.rst#locators"
  :type '(alist :key-type (list (repeat string)) :value-type string)
  :group 'org-ref)


(defcustom org-ref-export-suppress-affix-types
  '("citet"
    "citet*"
    "citetitle"
    "citeyear"
    "citeauthor"
    "citenum"
    "textcite")
  "List of cite types to suppress affixes (usually parentheses) on."
  :type '(list (repeat string))
  :group 'org-ref)


;;; Footnote citation support (issue #993)

(defvar org-ref-footnote-counter 0
  "Counter for footnote citations during export.
Reset to 0 at the start of each export process.")

(defcustom org-ref-footnote-cite-types
  '("footcite"
    "footfullcite"
    "footcitetext"
    "footcites"
    "footcitetexts"
    "smartcite"
    "Smartcite")
  "List of citation types that should be rendered as footnotes.
These citation types will be given a :note-index parameter when
passed to citeproc-el, which causes them to be rendered as notes/footnotes
when using CSL styles that support note formatting."
  :type '(repeat string)
  :group 'org-ref)


(defun org-ref-footnote-cite-type-p (cite-type)
  "Return non-nil if CITE-TYPE should be rendered as a footnote.
CITE-TYPE is a string like \"footcite\" or \"cite\"."
  (member cite-type org-ref-footnote-cite-types))


(defun org-ref-get-next-footnote-number ()
  "Get the next footnote number and increment the counter.
Returns the next sequential footnote number (1, 2, 3, ...)."
  (setq org-ref-footnote-counter (1+ org-ref-footnote-counter)))


(defun org-ref-csl-style-supports-notes-p (csl-style-file)
  "Return non-nil if CSL-STYLE-FILE supports note/footnote formatting.
CSL-STYLE-FILE is a filename like \"chicago-fullnote-bibliography.csl\".

This function checks if the CSL style file contains note-related
class attributes, which indicate it supports footnote formatting."
  (or
   ;; Check filename for common note-style indicators
   ;; Match: -note-, -note.csl, fullnote, footnote
   (string-match-p "\\(\\bnote\\b\\|fullnote\\|footnote\\)" csl-style-file)
   ;; If we have the actual file, check its contents
   (when (file-exists-p csl-style-file)
     (with-temp-buffer
       (insert-file-contents csl-style-file)
       (goto-char (point-min))
       ;; Look for class="note" in the CSL XML
       (re-search-forward "class=\"note\"" nil t)))))


(defun org-ref-get-citation-note-index (citation-link)
  "Get the note-index for CITATION-LINK if it's a footnote citation type.
CITATION-LINK is an org-element link object.
Returns a footnote number if this is a footnote citation, nil otherwise."
  (when (org-ref-footnote-cite-type-p
         (org-element-property :type citation-link))
    (org-ref-get-next-footnote-number)))


(defun org-ref-dealias-label (alias)
  "Return the full, de-aliased label for ALIAS.
Looked up from `org-ref-csl-label-aliases'.

I added this because I think it is reasonable to expect if you
write pg. 2 that it will show that way when rendered. At the
moment that is not the case, and only page is accepted. This is
actually done in oc-csl too, although it uses a flat a-list."
  (or (cdr (assoc "page" org-ref-csl-label-aliases
		  (lambda (x1 _x2)
		    (or (member alias x1)
			(member (concat (downcase alias) ".") x1)))))
      alias))


(defun org-ref-get-cite-links ()
  "Return list of cite links in the order they appear in the buffer."
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (lnk)
      (when (assoc (org-element-property :type lnk) org-ref-cite-types)
	lnk))))


(defun org-ref-ref-csl-data (ref type)
  "Return the CSL alist for a REF of TYPE.
REF is a plist data structure returned from `org-ref-parse-cite-path'."
  ;; I believe the suffix contains "label locator suffix"
  ;; where locator is a number, or maybe a range of numbers like 5-6.
  ;; label is something like page or chapter
  ;; and the rest is the suffix text.
  ;; For example: ch. 5, for example
  ;; would be label = ch., locator=5, ",for example" as suffix.
  (let* ((full-suffix (string-trim (or (plist-get ref :suffix) ""))) 
	 locator
	 label locator suffix)

    ;; org-cite is more sophisticated than this and would allow things like 5, 6
    ;; and 12. I am not sure about what all should be supported yet. I guess the
    ;; idea there is you use everything from the first to last number as the
    ;; locator, but that seems tricky, what about something like: 5, 6 and 12,
    ;; because he had 3 books. One solution might be some kind of delimiter,
    ;; e.g. {5, 6 and 12}, because he had 3 books.

    (if (and (string-match
	      (rx-to-string
	       `(seq
		 ;; optional label
		 (group-n 1 (optional
			     (regexp ,(regexp-opt (cl-loop for (abbrvs . full)
							   in org-ref-csl-label-aliases
							   append (append abbrvs (list full)))))))
		 (optional (one-or-more space))
		 ;; number or numeric ranges
		 (group-n 2 (one-or-more digit) (optional "-" (one-or-more digit)))
		 ;; everything else
		 (group-n 3 (* "."))))	      
	      full-suffix)
	     (match-string 2 full-suffix)
	     (not (string= "" (match-string 2 full-suffix)))) 
	;; We found a locator
	(setq label (match-string 1 full-suffix)
	      locator (match-string 2 full-suffix)
	      suffix (match-string 3 full-suffix))
      (setq label nil
	    locator nil
	    suffix full-suffix))
    
    ;; Let's assume if you have a locator but not a label that you mean page.
    (when (and locator (string= "" (string-trim label)))
      (setq label "page"))
    
    `((id . ,(plist-get ref :key))
      (prefix . ,(plist-get ref :prefix))
      (suffix . ,suffix)
      (locator . ,locator)
      (label . ,(when label (org-ref-dealias-label (string-trim label))))
      ;; [2024-01-29 Mon] see
      ;; https://github.com/jkitchin/org-ref/issues/1103#issuecomment-1915028374
      ;; and related comments at
      ;; https://github.com/andras-simonyi/citeproc-el/issues/151. It seems I
      ;; should not be adding this. TODO: proof of concept and not complete. I
      ;; did not go through all the types to see what else should be in here.
      ;; (suppress-author . ,(not (null (member type '("citenum" "citeyear"
      ;; "citeyear*" "citedate" "citedate*" "citetitle" "citetitle*"
      ;; "citeurl")))))
      )))

(declare-function org-ref-find-bibliography "org-ref-core")

(defun org-ref-process-buffer (backend &optional subtreep)
  "Process the citations and bibliography in the org-buffer.
Usually run on a copy of the buffer during export.
BACKEND is the org export backend."
  (save-restriction
    (when subtreep
      (org-narrow-to-subtree))
    (let* ((csl-backend (or (cdr (assoc backend org-ref-backend-csl-formats)) 'plain))
	   ;; Use HTML backend for DOCX if requested for better formatting
	   (csl-backend (if (and (eq backend 'docx) org-ref-docx-use-html-backend)
			    'html
			  csl-backend))

	   (style (or (cadr (assoc "CSL-STYLE"
				   (org-collect-keywords
				    '("CSL-STYLE"))))
		      org-ref-csl-default-style))
	   (locale (or (cadr (assoc "CSL-LOCALE"
				    (org-collect-keywords
				     '("CSL-LOCALE"))))
		       org-ref-csl-default-locale))

	   (proc (citeproc-create
		  ;; The style
		  (cond
		   ((file-exists-p style)
		    style)
		   ;; In a user-dir
		   ((and (bound-and-true-p org-cite-csl-styles-dir)
			 (file-exists-p (org-ref--file-join org-cite-csl-styles-dir style)))
		    (org-ref--file-join org-cite-csl-styles-dir style))
		   ;; provided by org-ref
		   ((file-exists-p (expand-file-name style
						     (org-ref--file-join (file-name-directory
							      (locate-library "org-ref"))
							     "citeproc/csl-styles")))
		    (expand-file-name style (org-ref--file-join
					     (file-name-directory
					      (locate-library "org-ref"))
					     "citeproc/csl-styles")))
		   (t
		    (error "%s not found" style)))
		  ;; item-getter
		  ;; (citeproc-itemgetter-from-bibtex (org-ref-find-bibliography))
		  (citeproc-hash-itemgetter-from-any (org-ref-find-bibliography) t)
		  ;; locale getter
		  (citeproc-locale-getter-from-dir (if (bound-and-true-p org-cite-csl-locales-dir)
						       org-cite-csl-locales-dir
						     (org-ref--file-join (file-name-directory
							      (locate-library "org-ref"))
							     "citeproc/csl-locales")))
		  ;; the actual locale
		  locale))

	   ;; list of links in the buffer
	   (cite-links (org-element-map (org-element-parse-buffer) 'link
			 (lambda (lnk)
			   (when (assoc (org-element-property :type lnk) org-ref-cite-types)
			     lnk))))

	   (cites (cl-loop for cl in cite-links collect
			   (let* ((cite-data (org-ref-parse-cite-path (org-element-property :path cl)))
				  (common-prefix (or (plist-get cite-data :prefix) ""))
				  (common-suffix (or (plist-get cite-data :suffix) ""))
				  (refs (plist-get cite-data :references))
				  (type (org-element-property :type cl))

				  (cites (cl-loop for ref in refs collect
						  (org-ref-ref-csl-data ref type))))
			     ;; TODO: update eventually
			     ;; https://github.com/andras-simonyi/citeproc-el/issues/46
			     ;; To handle common prefixes, suffixes, I just concat
			     ;; them with the first/last entries. That is all that
			     ;; is supported for now. Combine common/local prefix
			     (setf (cdr (assoc 'prefix (cl-first cites)))
				   (concat common-prefix
					   (cdr (assoc 'prefix (cl-first cites)))))
			     ;; Combine local/common suffix
			     (setf (cdr (assoc 'suffix (car (last cites))))
				   (concat (cdr (assoc 'suffix (car (last cites))))
					   common-suffix))

			     ;; https://github.com/andras-simonyi/citeproc-el#creating-citation-structures
			     (citeproc-citation-create
			      :cites cites
			      :suppress-affixes (let ((type (org-element-property :type cl)))
						  (when (member type
								org-ref-export-suppress-affix-types)
						    t))

			      ;; TODO: this is proof of concept, and not complete.
			      ;; mode is one of suppress-author, textual,
			      ;; author-only, year-only, or nil for default. These
			      ;; are not all clear to me.
			      :mode (let ((type (org-element-property :type cl)))
				      (cond
				       ((member type '("citet" "citet*"))
					'textual)
				       ((member type '("citeauthor" "citeauthor*"))
					'author-only)
				       ((member type '("citeyear" "citeyear*"))
					'year-only)
				       ((member type '("citedate" "citedate*"))
					'suppress-author)
				       (t
					nil)))

			      ;; I think the capitalized styles are what this is for
			      :capitalize-first (string-match
						 "[A-Z]"
						 (substring
						  (org-element-property :type cl) 0 1))
			      ;; I don't know where this information would come from.
			      :note-index nil
			      :ignore-et-al nil
			      :grouped nil))))

	   (rendered-citations (progn (citeproc-append-citations cites proc)
				      (citeproc-render-citations proc csl-backend org-ref-cite-internal-links)))
	   ;; I only use the returned bibliography string. citeproc returns a
	   ;; bunch of other things related to offsets and linespacing, but I
	   ;; don't know what you do with these, so just ignore them here.
	   (bibdata (citeproc-render-bib proc csl-backend))
	   (rendered-bib (car bibdata))
	   (bib-parameters (cdr bibdata))
	   ;; The idea is we will wrap each citation and the bibliography in
	   ;; org-code so it exports appropriately.
	   (cite-formatters '((html . "@@html:%s@@")
			      (latex . "@@latex:%s@@")
			      (odt . "@@odt:%s@@")))
	   (bib-formatters '((html .  "\n#+BEGIN_EXPORT html\n%s\n#+END_EXPORT\n")
			     (latex .  "\n#+BEGIN_EXPORT latex\n%s\n#+END_EXPORT\n")
			     (odt . "\n#+BEGIN_EXPORT ODT\n%s\n#+END_EXPORT\n"))))

      ;; replace the cite links
      (cl-loop for cl in (reverse cite-links) for rc in (reverse rendered-citations) do
	       (cl--set-buffer-substring (org-element-property :begin cl)
					 (org-element-property :end cl)
					 (format (or
						  (cdr (assoc backend cite-formatters))
						  "%s")
						 (concat
						  rc
						  ;; Add on extra spaces that were
						  ;; following it.
						  (make-string
						   (or
						    (org-element-property :post-blank cl) 0)
						   ? )))))

      ;; Decorate the bibliography for different outputs adapted from
      ;;  `org-cite-csl-render-bibliography' in oc-csl I don't know what all
      ;;  these do or how to use them in the CSS for html, or LaTeX. This is to
      ;;  fix an annoying HTML feature that has an extra line in the numbering.
      ;;  This seems to do the right thing. I don't support hanging-indent like
      ;;  oc-csl does, and I don't currently support LaTeX here. It already does
      ;;  a good job there.
      ;;
      ;; (find-library "oc-csl")
      ;;
      ;; Here are some examples of what is in the bib-parameters.
      ;;  (max-offset . 2) (hanging-indent) (second-field-align . flush)
      ;; (entry-spacing . 0)
      ;; (line-spacing . 2)
      ;;
      ;; Here we add style css for html output.
      (cond
       ((or (eq 'html backend) (eq 'html csl-backend))
	(let ((s1 "")
	      (s2 ""))
	  (when (cdr (assq 'second-field-align bib-parameters))
	    (setq s1 (format
		      "<style>.csl-left-margin{float: left; padding-right: 0em;}
 .csl-right-inline{margin: 0 0 0 %dem;}</style>"
		      ;; I hard coded this factor of 0.6 from the oc-csl code.  
		      (* 0.6  (or (cdr (assq 'max-offset bib-parameters)) 0)))))

	  ;; hard-coded the hanging indent. oc-csl uses a variable for this. I
	  ;; guess we could too, but this seems simpler.
	  (when (cdr (assq 'hanging-indent bib-parameters))
	    (setq s2 "<style>.csl-entry{text-indent: -1.5em; margin-left: 1.5em;}</style>"))
	  
	  (setq rendered-bib (concat
			      s1 s2
			      rendered-bib)))))

      ;; replace the bibliography
      (org-element-map (org-element-parse-buffer) 'link
	(lambda (lnk)
	  (cond
	   ((string= (org-element-property :type lnk) "bibliography")
	    (cl--set-buffer-substring (org-element-property :begin lnk)
				      (org-element-property :end lnk)
				      (format (or
					       (cdr (assoc backend bib-formatters))
					       "%s")
					      rendered-bib)))
	   ;; We just get rid of nobibliography links.
	   ((string= (org-element-property :type lnk) "nobibliography")
	    (cl--set-buffer-substring (org-element-property :begin lnk)
				      (org-element-property :end lnk)
				      "")))))
      ;; For LaTeX we need to define the citeprocitem commands
      ;; see https://www.mail-archive.com/emacs-orgmode@gnu.org/msg138546.html
      (when (eq backend 'latex)
	(goto-char (point-min))
	(insert "#+latex_header: \\makeatletter
#+latex_header: \\newcommand{\\citeprocitem}[2]{\\hyper@linkstart{cite}{citeproc_bib_item_#1}#2\\hyper@linkend}
#+latex_header: \\makeatother\n")))))


(defun org-ref-export-to (backend &optional async subtreep visible-only
				  body-only info)
  "Export buffer to BACKEND.
See `org-export-as' for the meaning of ASYNC SUBTREEP
VISIBLE-ONLY BODY-ONLY and INFO."
  (let* ((fname (buffer-file-name))
	 (extensions '((html . ".html")
		       (latex . ".tex")
		       (ascii . ".txt")
		       (md . ".md")
		       (odt . ".odf")))
	 (cp (point))
	 (mm) 				;marker to save place
	 (export-name (concat (file-name-sans-extension fname)
			      (or (cdr (assoc backend extensions)) ""))))

    (org-export-with-buffer-copy
     ;; Note I use a marker here to make sure we stay in the same place we were.
     ;; This is more robust than save-excursion I think, since processing moves
     ;; points around. In theory the marker should move too.
     (setq mm (make-marker))
     (move-marker mm cp)
     ;; make sure we expand includes
     (org-export-expand-include-keyword)
     (goto-char (marker-position mm))
     (org-ref-process-buffer backend subtreep)
     (set-marker mm nil)
     
     (pcase backend
       ;; odt is a little bit special, and is missing one argument
       ('odt (org-open-file (org-odt-export-to-odt async subtreep visible-only
						   info)
			    'system))
       ;; for pandoc, we mean make a docx via pandoc
       ('docx (org-open-file (plist-get (org-pandoc-export-to-docx async subtreep visible-only
								   body-only info)
					'output-file)
			     'system))
       (_
	(org-open-file (org-export-to-file backend export-name
			 async subtreep visible-only
			 body-only info)
		       'system))))))


;; I guess I tried to use apply-partially here, and it did not work, so these
;; are each defined manually

(defun org-ref-export-to-html (&optional async subtreep visible-only
					 body-only info)
  "Export the buffer to HTML and open.
See `org-export-as' for the meaning of ASYNC SUBTREEP
VISIBLE-ONLY BODY-ONLY and INFO."
  (org-ref-export-to 'html async subtreep visible-only
		     body-only info))


(defun org-ref-export-to-ascii (&optional async subtreep visible-only
					  body-only info)
  "Export the buffer to ascii and open.
See `org-export-as' for the meaning of ASYNC SUBTREEP
VISIBLE-ONLY BODY-ONLY and INFO."
  (org-ref-export-to 'ascii async subtreep visible-only
		     body-only info))


(defun org-ref-export-to-md (&optional async subtreep visible-only
				       body-only info)
  "Export the buffer to md and open.
See `org-export-as' for the meaning of ASYNC SUBTREEP
VISIBLE-ONLY BODY-ONLY and INFO."
  (org-ref-export-to 'md async subtreep visible-only
		     body-only info))


(defun org-ref-export-to-pdf (&optional async subtreep visible-only
					body-only info)
  "Export the buffer to PDF via LaTeX and open.
See `org-export-as' for the meaning of ASYNC SUBTREEP
VISIBLE-ONLY BODY-ONLY and INFO."
  (let ((org-export-before-parsing-functions (append
					      org-export-before-parsing-functions
					      '(org-ref-csl-preprocess-buffer))))
    (org-open-file (org-latex-export-to-pdf async subtreep visible-only
					    body-only info))))


(defun org-ref-export-to-latex (&optional async subtreep visible-only
					  body-only info)
  "Export the buffer to LaTeX and open.
See `org-export-as' for the meaning of ASYNC SUBTREEP
VISIBLE-ONLY BODY-ONLY and INFO."
  (org-ref-export-to 'latex async subtreep visible-only
		     body-only info))


(defun org-ref-export-to-odt (&optional async subtreep visible-only
					body-only info)
  "Export the buffer to ODT and open.
See `org-export-as' for the meaning of ASYNC SUBTREEP
VISIBLE-ONLY BODY-ONLY and INFO."
  (require 'htmlfontify)
  (unless (boundp 'hfy-user-sheet-assoc) (setq  hfy-user-sheet-assoc nil))
  (org-ref-export-to 'odt async subtreep visible-only
		     body-only info))


(defun org-ref-export-to-docx (&optional async subtreep visible-only
					 body-only info)
  "Export the buffer to docx via pandoc and open.

Note: By default, DOCX export uses the org backend for citations which
has formatting limitations (no small caps support). Set
`org-ref-docx-use-html-backend' to t for better formatting preservation.

See `org-export-as' for the meaning of ASYNC SUBTREEP
VISIBLE-ONLY BODY-ONLY and INFO."
  (org-ref-export-to 'docx async subtreep visible-only
		     body-only info))


(defun org-ref-export-as-org (&optional _async subtreep visible-only
					body-only info)
  "Export the buffer to an ORG buffer and open.
We only make a buffer here to avoid overwriting the original file.
See `org-export-as' for the meaning of ASYNC SUBTREEP
VISIBLE-ONLY BODY-ONLY and INFO."
  (let* ((export-buf "*org-ref ORG Export*")
	 export)

    (org-export-with-buffer-copy
     (org-ref-process-buffer 'org subtreep)
     (setq export (org-export-as 'org subtreep visible-only body-only info))
     (with-current-buffer (get-buffer-create export-buf)
       (erase-buffer)
       (org-mode)
       (insert export)))
    (pop-to-buffer export-buf)))


(defun org-ref-export-to-message (&optional _async subtreep visible-only
					    body-only info)
  "Export to ascii and insert in an email message."
  (let* ((backend 'ascii)
	 (content (org-export-with-buffer-copy
		   (org-ref-process-buffer backend subtreep)
		   (org-export-as backend subtreep visible-only
				  body-only info))))
    (compose-mail)
    (message-goto-body)
    (insert content)
    (message-goto-to)))


(org-export-define-derived-backend 'org-ref 'org
  :menu-entry
  '(?r "Org-ref export"
       ((?a "to Ascii" org-ref-export-to-ascii)
	(?m "to markdown" org-ref-export-to-md)
	(?h "to html" org-ref-export-to-html)
	(?l "to LaTeX" org-ref-export-to-latex)
	(?p "to PDF" org-ref-export-to-pdf)
	(?o "to ODT" org-ref-export-to-odt)
	(?O "to Org buffer" org-ref-export-as-org)
	(?e "to email" org-ref-export-to-message)
	(?w "to docx" org-ref-export-to-docx))))

;; An alternative to this exporter is to use an  `org-export-before-parsing-functions'
;; (add-hook 'org-export-before-parsing-functions 'org-ref-csl-preprocess-buffer)

(defun org-ref-csl-preprocess-buffer (backend)
  "Preprocess the buffer in BACKEND export.
Note this may not work as expected, what about subtreep? The hook
function just takes one argument. For now we rely on
`buffer-narrowed-p' and an org-heading at the beginning.
I am not positive on this though."
  (org-ref-process-buffer backend (and (buffer-narrowed-p)
				       (save-excursion
					 (goto-char (point-min))
					 (org-at-heading-p)))))


;; A transient exporter with preprocessors
(defvar org-ref/natmove nil
  "Toggle for moving natbib citations before export.")

(defvar org-ref/citeproc nil
  "Toggle for running citeproc before export.")

(defvar org-ref/refproc nil
  "Toggle for running refproc before export.")

(defvar org-ref/acrossproc nil
  "Toggle for running acrossproc before export.")

(defvar org-ref/idxproc nil
  "Toggle for running idxproc before export.")

(defvar org-ref/bblproc nil
  "Toggle for running bblproc before export.")

(defun org-ref-export--toggle (var)
  (set var (not (symbol-value var)))
  (message "%s %s"
           (capitalize (replace-regexp-in-string "/" " " (symbol-name var)))
           (if (symbol-value var) "enabled" "disabled"))
  (symbol-value var))

(defun org-ref-export-toggle-natmove ()
  (interactive)
  (org-ref-export--toggle 'org-ref/natmove))

(defun org-ref-export-toggle-citeproc ()
  (interactive)
  (org-ref-export--toggle 'org-ref/citeproc))

(defun org-ref-export-toggle-refproc ()
  (interactive)
  (org-ref-export--toggle 'org-ref/refproc))

(defun org-ref-export-toggle-acrossproc ()
  (interactive)
  (org-ref-export--toggle 'org-ref/acrossproc))

(defun org-ref-export-toggle-idxproc ()
  (interactive)
  (org-ref-export--toggle 'org-ref/idxproc))

(defun org-ref-export-toggle-bblproc ()
  (interactive)
  (org-ref-export--toggle 'org-ref/bblproc))

(defun org-ref-export--toggle-description (label var)
  (format "%s %s" label (if (symbol-value var) "[on]" "[off]")))


(defun org-ref-export-dispatch (&optional arg)
  "Run the export dispatcher with the desired hooks selected in the export menu."
  (interactive "P")

  (when (and org-ref/citeproc org-ref/bblproc)
    (error "You cannot use CSL and BBL at the same time."))

  (let ((org-export-before-parsing-functions org-export-before-parsing-functions))
    (when org-ref/citeproc
      (cl-pushnew 'org-ref-csl-preprocess-buffer org-export-before-parsing-functions))

    (when org-ref/refproc
      (cl-pushnew 'org-ref-refproc org-export-before-parsing-functions))

    (when org-ref/acrossproc
      (cl-pushnew 'org-ref-acrossproc org-export-before-parsing-functions))

    (when org-ref/idxproc
      (cl-pushnew 'org-ref-idxproc org-export-before-parsing-functions))

    (when org-ref/bblproc
      (unless (featurep 'org-ref-natbib-bbl-citeproc)
	(require 'org-ref-natbib-bbl-citeproc))
      (cl-pushnew 'org-ref-bbl-preprocess org-export-before-parsing-functions))

    ;; this goes last since it moves cites before they might get replaced.
    (when org-ref/natmove
      (cl-pushnew 'org-ref-cite-natmove org-export-before-parsing-functions))

    (org-export-dispatch arg)))

(transient-define-prefix org-ref-export-menu ()
  "Select export preprocessors and dispatch export."
  [["Preprocessors"
    ("n" org-ref-export-toggle-natmove :transient t
     :description (lambda () (org-ref-export--toggle-description "natmove" 'org-ref/natmove)))
    ("c" org-ref-export-toggle-citeproc :transient t
     :description (lambda () (org-ref-export--toggle-description "citeproc" 'org-ref/citeproc)))
    ("r" org-ref-export-toggle-refproc :transient t
     :description (lambda () (org-ref-export--toggle-description "refproc" 'org-ref/refproc)))
    ("a" org-ref-export-toggle-acrossproc :transient t
     :description (lambda () (org-ref-export--toggle-description "acrossproc" 'org-ref/acrossproc)))
    ("i" org-ref-export-toggle-idxproc :transient t
     :description (lambda () (org-ref-export--toggle-description "idxproc" 'org-ref/idxproc)))
    ("b" org-ref-export-toggle-bblproc :transient t
     :description (lambda () (org-ref-export--toggle-description "bblproc" 'org-ref/bblproc)))]
   ["Actions"
    ("e" "Export" org-ref-export-dispatch)
    ("q" "Quit" transient-quit-one)]])

(define-obsolete-function-alias 'org-ref-export/body
  #'org-ref-export-menu "3.1")

(define-obsolete-function-alias 'org-ref-export-from-hydra
  #'org-ref-export-dispatch "3.1")

(provide 'org-ref-export)

;;; org-ref-export.el ends here
