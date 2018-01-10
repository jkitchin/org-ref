;;; readme.org --- org-ref-citeproc - Citation processor for org-mode
;;; Commentary: This code is style agnostic. It will get information from the
;;; information in `citation-style' and `bibliography-style'. These are defined
;;; in a csl.el file.
;;
;; Conventions:
;; an "entry" is the result of `bibtex-parse-entry'.
;;

;;; Commentary:
;;

(declare-function org-ref-get-bibtex-key-and-file "org-ref-core")
(declare-function org-ref-get-bibtex-keys "org-ref-core")
(declare-function parsebib-find-bibtex-dialect "parsebib")
(defvar org-export-current-backend)
(defvar org-ref-cite-types)

(require 'org-element)



;;; Code:
(defvar *orcp-citation-links* '()
  "List of citation links in the text.
A link may have more than one citation in it.  These links get
replaced by the new citation text.")


(defvar *orcp-unique-entries* '()
  "List of unique (key . entry) parsed bibtex entries in the document.
The list is sorted according to the style.  This list eventually
makes up the bibliography.")


(defvar citation-style '()
  "Style data for an in-text citation.
For unsrt, a regular cite is superscripted, sorted,
range-collapsed numbers.

LABEL is a function that is run to get the label.
PREFIX goes before a citation.
SUFFIX goes after a citation.
citations are separated by DELIMITER.
SORT specifies that 3, 1, 2 be converted to 1,2,3
COLLAPSE is a function to collapse multiple citations, e.g. 1,2,3 becomes 1-3.
VERTICAL-ALIGN is a function that places the citation, e.g.
superscript, nil for baseline, etc...

Additional entries provide overrides for special citation types.")


(defvar bibliography-style '()
  "Bibliography style data.
SORT is a function that sorts the entries, e.g. by author, or
year, or nil.  It should take one argument, the list of unique
entries (key . entry).

LABEL is a function that returns how the entry is numbered, or
referenced in the text.

HANGING-INDENT is for the indentation of the entry on the left.

JUSTIFICATION is the overall justification on the right.

SPACING is the number of lines between entries.

HEADER is a string that is inserted above the bibliography.

ENTRIES is a alist of entry type and fields to make the entry from.")


;;* Collect citations

(defun orcp-collect-citations ()
  "Return a list of citation links in the document."
  (setq *orcp-citation-links*
	(cl-loop for link in (org-element-map
			      (org-element-parse-buffer) 'link 'identity)
	      if (-contains?
		  org-ref-cite-types
		  (org-element-property :type link))
	      collect link)))


(defun orcp-key-to-entry (key)
  "Return a parsed bibtex entry for KEY.
The KEY is found for the bibliography in the file."
  (let* ((results (org-ref-get-bibtex-key-and-file key))
	 (bibfile (cdr results)))

    (save-excursion
      (with-temp-buffer
	(insert-file-contents bibfile)
	(bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
	(bibtex-search-entry key)
	(let ((entry (bibtex-parse-entry t)))
	  (dolist (cons-cell entry)
	    (setf (car cons-cell) (downcase (car cons-cell))))
	  (setf (cdr (assoc "=type=" entry))
		(downcase (cdr (assoc "=type=" entry))))
	  entry)))))


(defun orcp-collect-unique-entries ()
  "Return a list of unique entries, sorted as required by the style.
Each entry is (key . entry)."
  (let ((keys (org-ref-get-bibtex-keys))
	sort-func
	entries)
    (setq entries
	  (cl-loop for key in keys
		collect (cons key (orcp-key-to-entry key))))
    ;; Now we should sort them if the style requires it
    (setq sort-func (cdr (assoc 'sort bibliography-style)))

    (when sort-func
      (setq entries (funcall sort-func entries)))
    (setq *orcp-unique-entries* entries)))


;;** Unique entry sorting functions

(defun orcp-sort-entries-increasing-year (unique-entries)
  "Sort UNIQUE-ENTRIES in increasing year of publication.
i.e. oldest stuff first."
  (sort unique-entries
	(lambda (a b)
	  (let* ((e1 (cdr a))
		 (e2 (cdr b))
		 (year1 (string-to-number (cdr (assoc "year" e1))))
		 (year2 (string-to-number (cdr (assoc "year" e2)))))
	    (> year2 year1)))))

(defun orcp-sort-entries-decreasing-year (unique-entries)
  "Sort UNIQUE-ENTRIES in decreasing year.
i.e. most current first."
  (reverse (orcp-sort-entries-increasing-year unique-entries)))

(defun orcp-get-entry-field (field entry)
  "RETURN FIELD from ENTRY.
Strip extra spaces and carriage returns."
  (let ((result (cdr (assoc field entry))))
    (when result
      (while (string-match "[\n\t\r]\\|[ \t][ \t]+" result)
	(setq result (replace-match " " nil t result))))
    result))


(defun orcp-sort-entries-alphabetical (unique-entries)
  "Sort UNIQUE-ENTRIES alphabetically by first author last name."
  (sort unique-entries
	(lambda (a b)
	  (let* ((e1 (cdr a))
		 (e2 (cdr b))
		 (authors1 (s-split
			    " and "
			    (orcp-get-entry-field "author" e1)))
		 (author1 (orcp-parse-authorname (car authors1)))
		 ;; lastname is "von last"
		 (last1 (concat (nth 1 author1) " " (nth 2 author1)))
		 (authors2 (s-split
			    " and "
			    (orcp-get-entry-field "author" e2)))
		 (author2 (orcp-parse-authorname (car authors2)))
		 (last2 (concat (nth 1 author2) " " (nth 2 author2))))
	    (string-lessp last1 last2)))))


;;* Citation labels for one citation key
;; No styling is done here.

(defun orcp-citation-number-label (key unique-entries)
  "Find the numeric index of KEY in UNIQUE-ENTRIES and return as a string.
Indexing starts at 0 so we add one."
  (number-to-string
   (+ 1
      (-find-index
       (lambda (entry)
	 (string= key (car entry)))
       unique-entries))))


(defun orcp-footnote-label (key unique-entries)
  "Return an org footnote label for KEY in UNIQUE-ENTRIES."
  (format "[fn:%s]" (orcp-citation-number-label key unique-entries)))


(defun orcp-citation-author-label (key unique-entries)
  "Return an author last name label for KEY.
KEY is found in UNIQUE-ENTRIES."
  (let* ((i (-find-index
	     (lambda (entry)
	       (string= key (car entry)))
	     unique-entries))
	 (entry (cdr (nth i unique-entries)))
	 (authors (s-split
		   " and "
		   (orcp-get-entry-field "author" entry)))
	 (first-author (orcp-parse-authorname (car authors))))
    (format "%s" (concat (nth 1 first-author)
			 (nth 2 first-author)))))

(defun orcp-citation-year-label (key unique-entries)
  "Return a year label for KEY.
KEY is found in UNIQUE-ENTRIES."
  (let* ((i (-find-index
	     (lambda (entry)
	       (string= key (car entry)))
	     unique-entries))
	 (entry (cdr (nth i unique-entries)))
	 (year (orcp-get-entry-field "year" entry)))
    (format "%s"  year)))


(defun orcp-citation-author-year-label (key unique-entries)
  "Return an author last name year label for KEY.
KEY is found in UNIQUE-ENTRIES.
We do not have a disambiguation strategy yet."
  (let* ((i (-find-index
	     (lambda (entry)
	       (string= key (car entry)))
	     unique-entries))
	 (entry (cdr (nth i unique-entries)))
	 (authors (s-split
		   " and "
		   (orcp-get-entry-field "author" entry)))
	 (first-author (orcp-parse-authorname (car authors)))
	 (year (orcp-get-entry-field "year" entry)))
    (format "%s %s" (concat (nth 1 first-author)
			    (nth 2 first-author))
	    year)))

;;* Replacements for citation links

;; Here we have to map over the keys in a citation, sort them according to the
;; style, get replacement labels, concat them together with the style delimiter,
;; add the prefix and suffix, and finally format for the type and output
;; backend.

(defun orcp-get-citation-style (symbol type)
  "Get the style info for SYMBOL for a citation TYPE from `citation-style'.
Styles have a default, but allow TYPE overrides.  This function
returns the style with the override."
  (let (style)
    ;; first get default style
    (setq style (cdr (assoc symbol citation-style)))

    ;; now check for an override
    ;; we need to find the type, and the symbol in the type
    (when (and (assoc type citation-style)
	       (assoc symbol (assoc type citation-style)))
      (setq style (cdr (assoc symbol (assoc type citation-style)))))
    style))


(defun orcp-get-text-replacement (citation-link)
  "Return replacement string for CITATION-LINK."
  (let* ((type (intern (org-element-property :type citation-link)))
	 (path (org-element-property :path citation-link))
	 (keys (s-split "," path))
	 (entries (mapcar 'orcp-key-to-entry keys))
	 (label-func (orcp-get-citation-style 'label type))
	 (delimiter (orcp-get-citation-style 'delimiter type))
	 (sort-func (orcp-get-citation-style 'sort type))
	 labels
	 replacement-text)

    ;; sort is not coded yet. I am not sure the best data to sort here. the keys?
    (when sort-func
      (setq keys (sort keys sort-func)))

    ;; get labels. This function is where you would, for example, create
    ;; hyperlinks to the bibliography. This function should return a list of
    ;; strings
    (setq labels
	  (mapcar
	   (lambda (key)
	     (funcall label-func key *orcp-unique-entries*))
	   keys))

    ;; collapse range - not used yet.

    ;; now get a string collecting everything
    (setq labels (mapconcat 'identity labels delimiter))

    (setq replacement-text (concat
			    (orcp-get-citation-style 'prefix type)
			    labels
			    (orcp-get-citation-style 'suffix type)))

    ;; finally, call formatter
    (funcall (or  (orcp-get-citation-style 'vertical-align type)
		  'baseline)
	     replacement-text)))


(defun orcp-get-citation-replacements ()
  "Get a list of replacements for all links in `*orcp-citation-links*'."
  (mapcar 'orcp-get-text-replacement *orcp-citation-links*))

;;* Formatted bibliography

(defun orcp-formatted-bibliography ()
  "Return the formatted bibliography."
  (let* ((spacing (or (cdr (assoc 'spacing bibliography-style)) 1))
	 (label-func (cdr (assoc 'label bibliography-style)))
	 (label-prefix (cdr (assoc 'label-prefix bibliography-style)))
	 (label-suffix (cdr (assoc 'label-suffix bibliography-style)))
	 (justification (cdr (assoc 'justification bibliography-style)))
	 (hanging-indent (cdr (assoc 'hanging-indent bibliography-style)))
	 (header (cdr (assoc 'header bibliography-style)))
	 (unique-entries (orcp-collect-unique-entries))
	 (adaptive-fill-function '(lambda () "    "))
	 (indent-tabs-mode nil)
	 bibliography-string)

    (setq bibliography-string
	  (mapconcat
	   'identity
	   ;; loop over the entries in the bibliography
	   (cl-loop for entry in unique-entries
		 collect
		 (progn
		   (let* ((entry-type (downcase
				       (cdr (assoc "=type=" (cdr entry)))))
			  (key (cdr (assoc "=key=" (cdr entry))))
			  (entry-styles (cdr (assoc 'entries bibliography-style)))
			  (entry-fields
			   (progn
			     (if (cdr (assoc (intern entry-type) entry-styles))
				 (cdr (assoc (intern entry-type) entry-styles))
			       (warn "%s not found. Using default." entry-type)
			       (cdr (assoc 't entry-styles))
			       )))
			  (funcs (mapcar
				  (lambda (field)
				    (if (fboundp (intern
						  (format "orcp-%s" field)))
					(intern
					 (format "orcp-%s" field))
				      ;; No formatter found. just get the data
				      `(lambda (entry)
					 (orcp-get-entry-field
					  ,(symbol-name field) entry))))
				  entry-fields))
			  (label (concat label-prefix
					 (funcall label-func key unique-entries)
					 label-suffix)))

		     ;; this is the entry. We do this in a buffer to make it
		     ;; easy to indent, fill, etc...
		     (with-temp-buffer
		       (insert label)
		       (insert (mapconcat (lambda (field-func)
					    (funcall field-func entry))
					  funcs
					  ""))
		       (goto-char (point-min))
		       (forward-word)
		       ;; It doesn't make sense to do this for all formats, e.g.HTML.
		       ;; commenting out for now.
		       ;; (increase-left-margin
		       ;;	(point-min) (point-max) hanging-indent)
		       ;; (fill-region (point-min) (point-max) justification)
		       (buffer-string)))))
	   ;; Here we put in the separator between entries
	   (cond
	    ;; placeholder for other formats
	    ((eq org-export-current-backend 'html)
	     " @@html:<br>@@\n")
	    (t
	     ;; put in a \n for each spacing
	     (mapconcat 'identity
			(cl-loop for i to spacing
			      collect "\n")
			"")))))
    ;; TODO: figure out header. how do we insert it properly formatted?
    bibliography-string))

;;* Text formatting functions.

;; These take text, and format them according to a backend. We derive the
;; backend from `org-export-current-backend' because I anticipate using this
;; during export.

(defun baseline (text)
  "Return TEXT."
  text)


(defun superscript (text)
  "Format TEXT as superscripted."
  (cond
   ((eq org-export-current-backend 'html)
    (format "@@html:<sup>%s</sup>@@" text))
   ;; the catch-all case is org-syntax
   (t
    (format "^{%s}" text))))


(defun italics (text)
  "Format TEXT as italics."
  (cond
   ((eq org-export-current-backend 'html)
    (format "@@html:<i>%s</i>@@" text))
   ;; the catch-all case is org-syntax
   (t
    (format "/%s/" text))))


(defun bold (text)
  "Format TEXT in bold."
  (cond
   ((eq org-export-current-backend 'html)
    (format "@@html:<b>%s</b>@@" text))
   ;; the catch-all case is org-syntax
   (t
    (format "*%s*" text))))

;;* Field formatting functions

;;These should be style agnostic functions. They take an entry and return a
;; formatted field for the entry, using information from the csl file.

(defun firstname (author-cell)
  "Return firstname from AUTHOR-CELL."
  (car author-cell))


(defun lastname (author-cell)
    "Return lastname from AUTHOR-CELL."
  (cdr author-cell))


(defun orcp-author (entry)
  "Return formatted author string from the ENTRY.
ENTRY is from `bibtex-parse-entry'.
Style information comes from `bibliography'"
  (let* ((style (cdr (assoc 'author bibliography-style)))
	 (delimiter (cdr (assoc 'delimiter style)))
	 (name1 (nth 0 (cdr (assoc 'name-order style))))
	 (name2 (nth 1 (cdr (assoc 'name-order style))))
	 (name-separator (cdr (assoc 'name-separator style)))
	 (suffix (cdr (assoc 'suffix style)))
	 (field-separator (cdr (assoc 'field-separator style)))
	 (et-al (cdr (assoc 'et-al style)))
	 (authors (s-split
		   " and "
		   (or
		    (orcp-get-entry-field "author" entry)
		    "")))
	 ;; parse to list of (first von last jr)
	 (author-data (mapcar
		       (lambda (x)
			 (let ((aud (orcp-parse-authorname x)))
			   (cons (nth 0 aud)
				 (concat
				  (or (nth 1 aud) "")
				  (or (nth 2 aud) "")
				  (or (nth 3 aud) "")))))
		       authors))
	 ;; map first and last names, in order specified in style with separator
	 (author-names
	  (mapcar
	   (lambda (x)
	     (concat
	      (funcall name1 x)
	      name-separator
	      (funcall name2 x)))
	   author-data)))
    ;; check on et-al - not implemented yet

    ;; work on initialize - not implemented yet

    ;; mapconcat on delimiter then last author.
    (if (= 1 (length author-names))
	(concat (car author-names) suffix field-separator)
	(concat
	 (mapconcat
	  'identity
	  (butlast author-names)
	  delimiter)
	 (cdr (assoc 'last-author-delimiter style))
	 (car (last author-names))
	 suffix
	 field-separator))))


(defun orcp-title (entry)
  "Return formatted title for the bibtex ENTRY."
  (let* ((style (cdr (assoc 'title bibliography-style)))
	 (font-style (cdr (assoc 'font-style style)))
	 (suffix (cdr (assoc 'suffix style)))
	 (field-separator (cdr (assoc 'field-separator style)))
	 (title (orcp-get-entry-field "title" entry)))

    (concat
     (if font-style
	 (funcall font-style title)
       title)
     suffix
     field-separator)))


(defun orcp-journal (entry)
    "Return formatted journal for the bibtex ENTRY."
  (let* ((style (cdr (assoc 'journal bibliography-style)))
	 (font-style (cdr (assoc 'font-style style)))
	 (suffix (cdr (assoc 'suffix style)))
	 (field-separator (cdr (assoc 'field-separator style)))
	 (journal (orcp-get-entry-field "journal" entry)))

    (concat
     (if font-style
	 (funcall font-style journal)
       journal)
     suffix
     field-separator)))


(defun orcp-volume (entry)
  "Return formatted volume for the bibtex ENTRY."
  (let* ((style (cdr (assoc 'volume bibliography-style)))
	 (font-style (cdr (assoc 'font-style style)))
	 (prefix (cdr (assoc 'prefix style)))
	 (suffix (eval (cdr (assoc 'suffix style))))
	 (field-separator (cdr (assoc 'field-separator style)))
	 (volume (orcp-get-entry-field "volume" entry)))

    (setq volume (concat prefix volume suffix))
    (concat
     (if font-style
	 (funcall font-style volume)
       volume)
     field-separator)))


(defun orcp-issue (entry)
  "Return formatted issue for the bibtex ENTRY."
  (let* ((style (cdr (assoc 'issue bibliography-style)))
	 (font-style (cdr (assoc 'font-style style)))
	 (prefix (cdr (assoc 'prefix style)))
	 (suffix (cdr (assoc 'suffix style)))
	 (field-separator (cdr (assoc 'field-separator style)))
	 (issue (orcp-get-entry-field "number" entry)))

    ;; issue is optional and isn't always present.
    (if (not issue)
	field-separator
      (setq issue (concat prefix issue suffix))
      (if font-style
	  (funcall font-style
		   issue)
	issue))))


(defun orcp-pages (entry)
  "Return formatted pages for the bibtex ENTRY."
  (let* ((style (cdr (assoc 'pages bibliography-style)))
	 (font-style (cdr (assoc 'font-style style)))
	 (prefix (cdr (assoc 'prefix style)))
	 (suffix (cdr (assoc 'suffix style)))
	 (field-separator (cdr (assoc 'field-separator style)))
	 (pages (orcp-get-entry-field "pages" entry)))

    (setq pages (concat prefix pages suffix))

    ;; collapse-range not supported yet
    (concat (if font-style
		(funcall font-style
			 pages)
	      pages)
	    field-separator)))


(defun orcp-year (entry)
  "Return formatted year for the bibtex ENTRY."
  (let* ((style (cdr (assoc 'year bibliography-style)))
	 (font-style (cdr (assoc 'font-style style)))
	 (prefix (cdr (assoc 'prefix style)))
	 (suffix (cdr (assoc 'suffix style)))
	 (field-separator (cdr (assoc 'field-separator style)))
	 (year (orcp-get-entry-field "year" entry)))

    (setq year (concat prefix year suffix))

    ;; collapse-range not supported yet
    (concat
     (if font-style
	 (funcall font-style
		  year)
       year)
     field-separator)))


(defun orcp-doi-formatter (doi)
  "Return formatted DOI for different backends."
  (cond
   ((eq org-export-current-backend 'html)
    (format "http://dx.doi.org/%s" doi))
   (t
    (format "doi:%s" doi))))


(defun orcp-doi (entry)
  "Return formatted doi for the bibtex ENTRY."
  (let* ((style (cdr (assoc 'doi bibliography-style)))
	 (font-style (cdr (assoc 'font-style style)))
	 (prefix (cdr (assoc 'prefix style)))
	 (suffix (cdr (assoc 'suffix style)))
	 (formatter (cdr (assoc 'formatter style)))
	 (doi (orcp-get-entry-field "doi" entry)))

    (when formatter
      (setq doi (funcall formatter doi)))
    (setq doi (concat prefix doi suffix))

    (if font-style
	(funcall font-style
		 (concat prefix doi suffix))
      doi)))


(defun orcp-url (entry)
  "Return formatted url for the bibtex ENTRY."
  (let* ((style (cdr (assoc 'doi bibliography-style)))
	 (font-style (cdr (assoc 'font-style style)))
	 (prefix (cdr (assoc 'prefix style)))
	 (suffix (cdr (assoc 'suffix style)))
	 (formatter (cdr (assoc 'formatter style)))
	 (url (orcp-get-entry-field "url" entry)))

    (when formatter
      (setq url (funcall formatter url)))
    (setq url (concat prefix url suffix))

    (if font-style
	(funcall font-style
		 (concat prefix url suffix))
      url)))

;;* Data structures for Author names

(defun orcp-unprotect-brackets (piece protected-strings)
  "Unprotect PIECE with the information in PROTECTED-STRINGS.
PROTECTED-STRINGS is a list of cons-cells (\"protection\" .
original text)."
  (when piece
    (mapc
     (lambda (cons-cell)
       (when (string-match (car cons-cell) piece)
	 (setq piece (replace-match (cdr cons-cell) t t piece))))
     protected-strings))
  piece)


;; See http://maverick.inria.fr/~Xavier.Decoret/resources/xdkbibtex/bibtex_summary.html#names for the parsing rules.
(defun orcp-parse-authorname (name)
  "Convert an author NAME to (first von last jr) data structure.
Valid name forms are:
First1 First2 Last
First1 First 2 {Last1 Last2}

First1 First2 von1 von2 Last1 Last2
von1 von2 Last1 Last2, Jr., First1 First2

Last1, First1 First2
{Von Last1}, First1 First2

We try to protect strings in curly brackets."
  (let* (protected-strings
	 uuid
	 ncommas
	 fields
	 first von last jr)

    ;; protect bracketed strings
    (while (string-match "{\\(.*\\)}" name)
      ;; We want our substitute to look like a name, not a von part so we add a
      ;; capital letter to the front.
      (setq uuid (concat "A" (md5 (format "%s%s%s%s%s%s%s"
					  (random)
					  (current-time)
					  (user-uid)
					  (emacs-pid)
					  (user-full-name)
					  user-mail-address
					  (recent-keys)))))
      (add-to-list 'protected-strings (cons uuid (match-string 0 name)))
      (setq name (replace-match uuid nil nil name)))

    (setq ncommas (s-count-matches "," name))

    (cond
     ;; "First von Last"
     ((= 0 ncommas)
      (setq fields (s-split " " name))
      (while (and (s-capitalized? (car fields)) (> (length fields) 1))
	(setq first (append first (list (pop fields)))))
      (when first
	(setq first (mapconcat 'identity first " ")))

      ;; Next, we get the von part. this is the longest white space delimited
      ;; string that ends with a lowercase word, and is not the rest of the
      ;; string.
      (let ((last-lower-index nil))
	(cl-loop for i to (length fields)
	      for word in (butlast fields)
	      if (s-lowercase? word)
	      do (setq last-lower-index i))
	(when last-lower-index
	  (setq von (mapconcat
		     'identity
		     (-slice fields 0 (+ 1 last-lower-index)) " "))
	  (setq fields (-slice fields (+ 1 last-lower-index)))))

      ;; all that should be left is the last name but it might be more than one
      ;; word, e.g. with a Jr. or a two work last name.
      (setq last (mapconcat 'identity fields " "))
      (mapcar
       (lambda (x)
	 (orcp-unprotect-brackets x protected-strings))
       (list first von last jr)))

     ;; "von Last, First"
     ((= 1 ncommas)
      (setq fields (s-split "," name))
      (setq first  (nth 1 fields))
      ;; split first field which could be von Lastname.
      (setq fields (s-split " " (car fields)))
      (let ((last-lower-index nil))
	(cl-loop for i to (length fields)
	      for word in fields
	      if (s-lowercase? word)
	      do (setq last-lower-index i))
	(when last-lower-index
	  (setq von (mapconcat
		     'identity
		     (-slice fields 0 (+ 1 last-lower-index)) " "))
	  (setq fields (-slice fields (+ 1 last-lower-index)))))
      ;; all that should be left is the last name
      (setq last (mapconcat 'identity fields " "))
      (mapcar
       (lambda (x)
	 (orcp-unprotect-brackets x protected-strings))
       (list first von last jr)))

     ;; "von Last, Jr, First"
     ((= 2 ncommas)
      (setq fields (s-split "," name))
      (setq first  (nth 2 fields))
      (setq jr (nth 1 fields))
      ;; split first field which could be von Lastname.
      (setq fields (s-split " " (car fields)))
      (let ((last-lower-index nil))
	(cl-loop for i to (length fields)
	      for word in fields
	      if (s-lowercase? word)
	      do (setq last-lower-index i))
	(when last-lower-index
	  (setq von (mapconcat 'identity (-slice fields 0 (+ 1 last-lower-index)) " "))
	  (setq fields (-slice fields (+ 1 last-lower-index)))))
      ;; all that should be left is the last name
      (setq last (mapconcat 'identity fields " "))
      (mapcar
       (lambda (x)
	 (orcp-unprotect-brackets x protected-strings))
       (list first von last jr))))))


;;* Collapse numeric range

(defun orcp-collapse-numeric-range (cites delimiter)
  "TODO use style info.
Collapse a numeric list of CITES into a range.
Collapsed ranges are separated by DELIMITER."
  (let (n
	(groups '()))
    (while cites
      (setq n (pop cites))
      (if (and (caar groups) (= (- n 1) (elt (car groups) 0)))
	  (setf (car groups) (append `(,n) (car groups)))
	(setf groups (append `((,n)) groups))))
    ;; Now for each group
    (mapconcat 'identity
	       (mapcar
		(lambda (lst)
		  (cond
		   ((>= (length lst) 3)
		    (format "%s-%s" (car lst) (car (last lst))))
		   ((= (length lst) 2)
		    (format "%s,%s" (nth 0 lst) (nth 1 lst)))
		   (t
		    (number-to-string (car lst)))))
		(mapcar 'reverse (reverse groups)))
	       delimiter)))


;;* Putting it all together

(defun sentence-beginning-p ()
  "Determine if point is at the beginning of a sentence.
The idea is to move forward a sentence, then back.  If the point
doesn't move, it means you were at the beginning of a sentence."
  (let ((cp (point)))
    (save-excursion
      (forward-sentence)
      (backward-sentence)
      (= cp (point)))))


(defun orcp-citeproc (&optional backend)
  "Format citations and bibliography for BACKEND.
Warning.  Destructive to your document! Will replace links.
Meant to be used in export on a temporary version of the
documents."

  ;; Get the style from bibliographystyle link
  ;; and eliminate bibliography style links
  ;; This will load all style modules
  (cl-loop for link in (org-element-map
			   (org-element-parse-buffer) 'link 'identity)
	   if (string= "bibliographystyle"
		       (org-element-property :type link))
	   do
	   ;; get path for style and load it
	   (load-library (org-element-property :path link))
	   ;; get rid of the link in the buffer
	   (setf (buffer-substring (org-element-property :begin link)
				   (org-element-property :end link))
		 ""))

  (orcp-collect-citations)
  (orcp-collect-unique-entries)

  (let ((link-replacements (cl-loop for link in *orcp-citation-links*
				    for repl in (orcp-get-citation-replacements)
				    collect
				    (list repl
					  (org-element-property :begin link)
					  (org-element-property :end link))))
	(bibliography-string (orcp-formatted-bibliography))
	punctuation
	trailing-space
	bibliography-link)

    ;; replace citation links
    (cl-loop for (repl start end) in (reverse link-replacements)
	     for link in (reverse *orcp-citation-links*)
	     do
	     ;; chomp leading spaces if needed
	     (when (orcp-get-citation-style
		    'chomp-leading-space
		    (intern (org-element-property :type link)))
	       (goto-char start)
	       (while (and (not (sentence-beginning-p))
			   (looking-back " " (- (point) 2)))
		 (delete-char -1)
		 (setq start (- start 1))
		 (setq end (- end 1))))

	     ;; chomp trailing spaces if needed
	     (when (orcp-get-citation-style
		    'chomp-trailing-space
		    (intern (org-element-property :type link)))
	       (goto-char end)
	       (while (looking-back " " (- (point) 2))
		 (delete-char 1)))

	     ;; Check for transposing punctuation
	     (setq punctuation nil)
	     (when (orcp-get-citation-style
		    'transpose-punctuation
		    (intern (org-element-property :type link)))
	       ;; goto end of link
	       (goto-char end)
	       (when (looking-at "\\.\\|,\\|;")
		 (setq punctuation (buffer-substring end (+ 1 end)))
		 (delete-char 1)))

	     ;; preserve trailing space
	     (goto-char end)
	     (setq trailing-space (if (looking-back " " (line-beginning-position)) " " ""))

	     (setf (buffer-substring start end) (concat repl trailing-space))

	     (when punctuation
	       (goto-char start)
	       ;; I can't figure out why this is necessary. I would have thought
	       ;; the chomp leading spaces would get it.
	       (when (thing-at-point 'whitespace)
		 (delete-char -1))
	       (insert punctuation)))

    ;; Insert bibliography section at the bibliography link
    (setq bibliography-link (cl-loop for link
				     in (org-element-map
					    (org-element-parse-buffer)
					    'link 'identity)
				     if (string= "bibliography"
						 (org-element-property :type link))
				     collect link))
    (pcase (length bibliography-link)
      ((pred (< 1)) (error "Only one bibliography link allowed"))
      ((pred (= 1))
       ;; replace bibliography link
       (setq bibliography-link (car bibliography-link))
       (setf (buffer-substring (org-element-property :begin bibliography-link)
                               (org-element-property :end bibliography-link))
             bibliography-string))
      ((pred (= 0))
       ;; no bibliography link in document
       (when link-replacements
         (message "Warning: No bibliography link found although there are citations to process"))))))

;; * the end
(provide 'org-ref-citeproc)
;;; org-ref-citeproc.el ends here
