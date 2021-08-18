(defcustom org-ref-clean-bibtex-key-function
  (lambda (key)
    (replace-regexp-in-string ":" "" key))
  "Function to modify a bibtex key.
The default behavior is to remove : from the key."
  :type 'function
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

;;** create text citations from a bibtex entry

(defun org-ref-bib-citation ()
  "From a bibtex entry, create and return a citation string.
If `bibtex-completion' library is loaded, return reference in APA
format. Otherwise return a  citation string from `org-ref-get-bibtex-entry-citation'."
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((bibtex-expand-strings t)
           (entry (bibtex-parse-entry t))
           (key (reftex-get-bib-field "=key=" entry)))
      (org-ref-format-entry key))))

;;** Open pdf in bibtex entry
;;;###autoload
(defun org-ref-open-bibtex-pdf ()
  "Open pdf for a bibtex entry, if it exists."
  (interactive)
  (bibtex-completion-open-pdf (list (bibtex-completion-get-key-bibtex))))


;;** Open notes from bibtex entry
;;;###autoload
(defun org-ref-open-bibtex-notes ()
  "From a bibtex entry, open the notes if they exist."
  (interactive)
  (bibtex-completion-edit-notes (list (bibtex-completion-get-key-bibtex))))


;;** Open bibtex entry in browser
;;;###autoload
(defun org-ref-open-in-browser ()
  "Open the bibtex entry at point in a browser using the url field or doi field."
  (interactive)
  (bibtex-completion-open-url-or-doi (list (bibtex-completion-get-key-bibtex))))


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
    (org-open-file pdffile)))


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
	 (field-order (cdr (assoc (if type (downcase type))
				  org-ref-bibtex-sort-order))))

    ;; these are the fields we want to order that are in this entry
    (setq entry-fields (mapcar (lambda (x) (car x)) entry))
    ;; we do not want to reenter these fields
    (setq entry-fields (remove "=key=" entry-fields))
    (setq entry-fields (remove "=type=" entry-fields))

    ;;these are the other fields in the entry, and we sort them alphabetically.
    (setq other-fields
	  (sort (-remove (lambda(x) (member x field-order)) entry-fields)
		'string<))

    (save-restriction
      (bibtex-kill-entry)
      (insert
       (concat "@" type "{" key ",\n"
	       (mapconcat
	        (lambda (field)
		  (when (member field entry-fields)
		    (format "%s = %s,"
			    field
			    (cdr (assoc field entry)))))
	        field-order "\n")
	       ;; now add the other fields
	       (mapconcat
	        (lambda (field)
		  (cl-loop for (f . v) in entry concat
			   (when (string= f field)
			     (format "%s = %s,\n" f v))))
	        (-uniq other-fields) "\n")
	       "\n}"))
      (bibtex-find-entry key)
      (bibtex-fill-entry)
      (bibtex-clean-entry))))

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

(defun orcb-clean-nil (arg)
  "Remove nil from some article fields.
The removal is conditional. Sometimes it is useful to have nil
around, e.g. for ASAP articles where the fields are not defined
yet but will be in the future.

With \\[univeral-argument], run `bibtex-clean-entry' after.
"
  (interactive "P")
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
         (type (downcase (cdr (assoc "=type=" entry)))))
    (when (string= type "article")
      (cond
       ;; we have volume and pages but number is nil.
       ;; remove the number field.
       ((and (string= type "article")
	     (not (string= (cdr (assoc "volume" entry)) "{nil}"))
	     (not (string= (cdr (assoc "pages" entry)) "{nil}"))
	     (string= (cdr (assoc "number" entry)) "{nil}"))
	(bibtex-set-field "number" "")
	(if arg
            (bibtex-clean-entry)))))))


(defun orcb-clean-nil-opinionated ()
  "Remove nil from all article fields.

Note that by default, this will leave the entry empty, which may
then get deleted by `bibtex-clean-entry.' To disable this
behavior, remove opts-or-alts from `bibtex-entry-format'. This
will leave the empty entries so that you may fill them in later."
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
         (type (downcase (cdr (assoc "=type=" entry)))))
    (when (string= type "article")
      (cl-loop for (field . text) in entry do
               (if (string= text "{nil}")
                   (bibtex-set-field field ""))))))


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


(defvar orcb-%-replacement-string " \\\\%"
  "Replacement for a naked % sign in cleaning a BibTeX entry.
The replacement string should be escaped for use with
`replace-match'. Compare to the default value. Common choices
would be to omit the space or to replace the space with a ~ for a
non-breaking space.")

(defun orcb-% ()
  "Replace naked % with % in a bibtex entry.
Except when it is already escaped or in a URL. The replacement
for the % is defined by `orcb-%-replacement-string'."
  (save-restriction
    (bibtex-narrow-to-entry)
    (bibtex-beginning-of-entry)
    (while (re-search-forward "\\([^\\]\\)%\\([^[:xdigit:]]\\)" nil t)
      (replace-match (concat "\\1"
                             orcb-%-replacement-string
                             "\\2")))))


(defun orcb-key-comma ()
  "Make sure there is a comma at the end of the first line."
  (bibtex-beginning-of-entry)
  (end-of-line)
  ;; some entries do not have a key or comma in first line. We check and add it,
  ;; if needed.
  (unless (string-match ", *$" (thing-at-point 'line))
    (end-of-line)
    (insert ",")))


(defun orcb-key (&optional allow-duplicate-keys)
  "Replace the key in the entry.
Prompts for replacement if the new key duplicates one already in
the file, unless ALLOW-DUPLICATE-KEYS is non-nil."
  (let ((key (funcall org-ref-clean-bibtex-key-function
		      (bibtex-generate-autokey))))
    ;; remove any \\ in the key
    (setq key (replace-regexp-in-string "\\\\" "" key))
    ;; first we delete the existing key
    (bibtex-beginning-of-entry)
    (re-search-forward bibtex-entry-maybe-empty-head)
    (if (match-beginning bibtex-key-in-head)
	(delete-region (match-beginning bibtex-key-in-head)
		       (match-end bibtex-key-in-head)))
    ;; check if the key is in the buffer
    (when (and (not allow-duplicate-keys)
               (save-excursion
                 (bibtex-search-entry key)))
      (save-excursion
	(bibtex-search-entry key)
	(bibtex-copy-entry-as-kill)
	(switch-to-buffer-other-window "*duplicate entry*")
	(bibtex-yank))
      (setq key (bibtex-read-key "Duplicate Key found, edit: " key)))

    (insert key)
    (kill-new key)))


(defun orcb-check-journal ()
  "Check entry at point to see if journal exists in `org-ref-bibtex-journal-abbreviations'.
If not, issue a warning."
  (interactive)
  (when
      (string= "article"
               (downcase
                (cdr (assoc "=type=" (bibtex-parse-entry)))))
    (save-excursion
      (bibtex-beginning-of-entry)
      (let* ((entry (bibtex-parse-entry t))
             (journal (reftex-get-bib-field "journal" entry)))
        (when (null journal)
          (error "Unable to get journal for this entry."))
        (unless (member journal (-flatten org-ref-bibtex-journal-abbreviations))
          (message "Journal \"%s\" not found in org-ref-bibtex-journal-abbreviations." journal))))))


(defun orcb-fix-spacing ()
  "Delete whitespace and fix spacing between entries."
  (let (beg end)
    (save-excursion
      (save-restriction
    	(widen)
	(bibtex-beginning-of-entry)
	(setq beg (point))
	(bibtex-end-of-entry)
	(setq end (if (re-search-forward bibtex-any-entry-maybe-empty-head nil t)
		      (progn (beginning-of-line)
			     (point))
		    (point-max)))
	;; 1. delete whitespace
	(narrow-to-region beg end)
	(delete-trailing-whitespace)
	;; 2. delete consecutive empty lines
	(goto-char end)
	(while (re-search-backward "\n\n\n+" nil 'move)
	  (replace-match "\n\n"))
	;; 3. add one line between entries
	(goto-char end)
	(forward-line -1)
	(when (looking-at "[}][ \t]*\\|@Comment.+\\|%.+")
	  (end-of-line)
	  (newline))))))


;;;###autoload
(defun org-ref-clean-bibtex-entry ()
  "Clean and replace the key in a bibtex entry.
See functions in `org-ref-clean-bibtex-entry-hook'."
  (interactive)
  (save-excursion
    (save-restriction
      (bibtex-narrow-to-entry)
      (bibtex-beginning-of-entry)
      ;; run hooks. each of these operates on the entry with no arguments.
      ;; this did not work like  i thought, it gives a symbolp error.
      ;; (run-hooks org-ref-clean-bibtex-entry-hook)
      (mapc (lambda (x)
	      (save-restriction
		(save-excursion
		  (funcall x))))
	    org-ref-clean-bibtex-entry-hook))))

(defun org-ref-get-citation-year (key)
  "Get the year of an entry with KEY.  Return year as a string."
  (let* ((results (org-ref-get-bibtex-key-and-file key))
         (bibfile (cdr results)))
    (with-temp-buffer
      (insert-file-contents bibfile)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (bibtex-search-entry key nil 0)
      (prog1 (reftex-get-bib-field "year" (bibtex-parse-entry t))))))
