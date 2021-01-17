;;; org-ref-glossary.el --- glossary support in org-ref  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  John Kitchin

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

;; Provides Some glossary support for org-mode. Only export to LaTeX is
;; supported. The functionality is based on the LaTeX glossaries package. See
;; https://en.wikibooks.org/wiki/LaTeX/Glossary and
;; http://ctan.math.washington.edu/tex-archive/macros/latex/contrib/glossaries/glossaries-user.pdf

;; Put something like this in your org-file.
;; #+latex_header: \usepackage{glossaries}
;; #+latex_header: \makeglossaries

;; Put this where you want the glossaries to appear in your org-file.
;; \printglossaries

;; Add new glossary entries to your org-file like this. Enclose strings
;; containing a comma in {}. Multiline entries are supported.

;; #+latex_header_extra: \newglossaryentry{computer}{name=computer,description={A machine, that computes}}
;; #+latex_header_extra: \newglossaryentry{tree}{name=tree,description=a big plant}

;; #+latex_header_extra: \newglossaryentry{naiive}
;; #+latex_header_extra: {
;; #+latex_header_extra:   name=na\"{\i}ve,
;; #+latex_header_extra:   description={is a French loanword (adjective, form of naïf)
;; #+latex_header_extra:                indicating having or showing a lack of experience,
;; #+latex_header_extra:                understanding or sophistication}
;; #+latex_header_extra: }

;; Here is an example acronym definition
;; #+latex_header_extra: \newacronym{lvm}{LVM}{Logical Volume Manager}

;; New links defined:
;; gls:name A reference to the glossary entry NAME.
;; glspl:name The plural version of the entry
;; Gls:name Capitalized glossary entry
;; Glspl: Capitalized, plural glossary entry
;; [[gslink:name][alternate text]]
;; glssymbol:name Outputs the symbol value of the glossary entry settings.
;; glsdesc:name  The description of name

;; The links export to LaTeX. You can click on the link and jump to the
;; definition. The links have tooltips for the definitions.

;; Acronym links
;; acrshort:label
;; acrfull:label
;; acrlong:label
;; ac:label  (exports to \gls{label})
;; Ac:label  (exports to \Gls{label})
;; acp:label (exports to \glspl{label})
;; Acp:label (exports to \Glspl{label})

(require 'org-element)
(require 'org-ref-utils)
(require 'ox)

(declare-function helm "helm")
(declare-function helm-build-sync-source "helm-source")

;;; Code:
(defgroup org-ref-glossary nil
  "Customization group for org-ref-glossary."
  :tag "Org Ref glossary"
  :group 'org)


(defcustom org-ref-glossary-color "Mediumpurple3"
  "Color for glossary links."
  :type 'string
  :group 'org-ref)


(defcustom org-ref-acronym-color "Darkorange2"
  "Color for acronym links."
  :type 'string
  :group 'org-ref)


(defvar org-ref-glsentries '()
  "Variable to hold locations of glsentries load files.")


(defun or-find-closing-curly-bracket (&optional limit)
  "Find closing bracket for the bracket at point and move point to it.
Go up to LIMIT or `point-max'. This is a parsing function. I
wrote this because using `forward-list' does not always work if
there is an escaped \" for example. This seems pretty robust."
  (unless (looking-at "{") (error "Not at a curley bracket"))

  (let ((level 1))
    (while (and (not (= 0 level))
		(not (eobp))
		(< (point) (or limit (point-max))))
      (forward-char)
      (when (and (looking-at "{")
		 (not (looking-back "\\\\" (- (point) 2))))
	(cl-incf level))
      (when (and (looking-at "}")
		 (not (looking-back "\\\\" (- (point) 2))))
	(cl-decf level)))
    (point)))


;;* Glossary
(defun or-parse-glossary-entry (entry)
  "Parse glossary ENTRY definition to a p-list of key=value.
Typically:
  (:name name :description description)
but there could be other :key value pairs."
  (save-excursion
    (goto-char (point-min))
    (let* (end-of-entry
	   data
	   (external (when (re-search-forward "\\loadglsentries\\(\\[.*\\]\\){\\(?1:.*\\)}" nil t)
		       (match-string 1)))
	   (glsentries (and external
			    (or (cdr (assoc external org-ref-glsentries))
				(progn
				  (pushnew (cons external (s-trim (shell-command-to-string
								   (format "kpsewhich tex %s" external))))
					   org-ref-glsentries)
				  (cdr (assoc external org-ref-glsentries))))))
	   key value p1 p2)
      (catch 'data
	;; look inside first for latex-headers
	(goto-char (point-min))
	(when (re-search-forward
	       (format "\\newglossaryentry{%s}" entry) nil t)
	  (re-search-forward "{")
	  (save-excursion
	    (backward-char)
	    (or-find-closing-curly-bracket)
	    (setq end-of-entry (point)))

	  (while (re-search-forward "\\(\\w+?\\)=" end-of-entry t)
	    (setq key (match-string 1))
	    ;; get value
	    (goto-char (+ 1 (match-end 1)))
	    (setq p1 (point))
	    (if (looking-at "{")
		;; value is wrapped in {}
		(progn
		  (or-find-closing-curly-bracket)
		  (setq p2 (point)
			value (buffer-substring (+ 1 p1) p2)))
	      ;; value is up to the next comma
	      (re-search-forward "," end-of-entry 'mv)
	      (setq value (buffer-substring p1 (- (point) 1))))
	    ;; remove #+latex_header_extra:
	    (setq value (replace-regexp-in-string
			 "#\\+latex_header_extra: " "" value))
	    (setq value (replace-regexp-in-string
			 "\n +" " " value))
	    (setq data (append data
			       (list (intern (format ":%s" key)))
			       (list value))))
	  (throw 'data data))

	;; check for a glossary table
	(let* ((entries (save-excursion
			  (catch 'found
			    (org-element-map
				(org-element-parse-buffer)
				'table
			      (lambda (el)
				(when (string= "glossary" (org-element-property :name el))
				  (goto-char (org-element-property :contents-begin el))
				  (throw 'found
					 (nthcdr 2 (org-babel-read-table)))))))))
	       (result (assoc entry entries)))
	  (when result
	    (throw 'data (list :name (cl-second result) :description (cl-third result)))))

	;; then external
	(when (and glsentries
		   (file-exists-p glsentries))

	  (with-current-buffer (find-file-noselect glsentries)
	    (goto-char (point-min))
	    (when (re-search-forward
		   (format "\\newglossaryentry{%s}" entry) nil t)
	      (re-search-forward "{")
	      (save-excursion
		(backward-char)
		(or-find-closing-curly-bracket)
		(setq end-of-entry (point)))

	      (while (re-search-forward "\\(\\w+?\\)=" end-of-entry t)
		(setq key (match-string 1))
		;; get value
		(goto-char (+ 1 (match-end 1)))
		(setq p1 (point))
		(if (looking-at "{")
		    ;; value is wrapped in {}
		    (progn
		      (or-find-closing-curly-bracket)
		      (setq p2 (point)
			    value (buffer-substring (+ 1 p1) p2)))
		  ;; value is up to the next comma
		  (re-search-forward "," end-of-entry 'mv)
		  (setq value (buffer-substring p1 (- (point) 1))))
		(setq data (append data
				   (list (intern (format ":%s" key)))
				   (list value))))
	      (throw 'data data))))

	;; and finally with an export in case there are includes
	;; (let ((org-export-show-temporary-export-buffer nil)
	;;       ;; we do not need to run babel cells for this.
	;;       (org-export-use-babel nil))
	;;   (with-current-buffer (org-org-export-as-org)
	;;     (when (re-search-forward
	;; 	   (format "\\newglossaryentry{%s}" entry) nil t)
	;;       (re-search-forward "{")
	;;       (save-excursion
	;; 	(backward-char)
	;; 	(or-find-closing-curly-bracket)
	;; 	(setq end-of-entry (point)))

	;;       (while (re-search-forward "\\(\\w+?\\)=" end-of-entry t)
	;; 	(setq key (match-string 1))
	;; 	;; get value
	;; 	(goto-char (+ 1 (match-end 1)))
	;; 	(setq p1 (point))
	;; 	(if (looking-at "{")
	;; 	    ;; value is wrapped in {}
	;; 	    (progn
	;; 	      (or-find-closing-curly-bracket)
	;; 	      (setq p2 (point)
	;; 		    value (buffer-substring (+ 1 p1) p2)))
	;; 	  ;; value is up to the next comma
	;; 	  (re-search-forward "," end-of-entry 'mv)
	;; 	  (setq value (buffer-substring p1 (- (point) 1))))
	;; 	;; remove #+latex_header_extra:
	;; 	(setq value (replace-regexp-in-string
	;; 		     "#\\+latex_header_extra: " "" value))
	;; 	(setq value (replace-regexp-in-string
	;; 		     "\n +" " " value))
	;; 	(setq data (append data
	;; 			   (list (intern (format ":%s" key)))
	;; 			   (list value))))
	;;       (throw 'data data))))
	))))


;;;###autoload
(defun org-ref-add-glossary-entry (label name description)
  "Insert a new glossary entry.
LABEL is how you refer to it with links.
NAME is the name of the entry to be defined.
DESCRIPTION is the definition of the entry.
Entry gets added after the last #+latex_header line."
  (interactive "sLabel: \nsName: \nsDescription: ")
  (save-excursion
    (re-search-backward "#\\+latex_header" nil t)
    (forward-line)
    (when (not (looking-at "^$"))
      (beginning-of-line)
      (insert "\n")
      (forward-line -1))
    (insert (format "#+latex_header_extra: \\newglossaryentry{%s}{name={%s},description={%s}}\n"
		    label name description))))


(defun org-ref-glossary-face-fn (label)
  "Return a face for a glossary link."
  (save-match-data
    (cond
     ((or (not org-ref-show-broken-links)
	  (or-parse-glossary-entry label))
      'org-ref-glossary-face)
     (t
      'font-lock-warning-face))))


;;** Glossary links
(defun or-follow-glossary (entry)
  "Goto beginning of the glossary ENTRY."
  (org-mark-ring-push)
  (goto-char (point-min))
  (re-search-forward (format "\\newglossaryentry{%s}" entry))
  (goto-char (match-beginning 0)))


(defvar org-ref-glossary-gls-commands
  '("gls" "glspl" "Gls" "Glspl" "glssymbol" "glsdesc"))


(dolist (command org-ref-glossary-gls-commands)
  (org-ref-link-set-parameters command
    :follow #'or-follow-glossary
    :face 'org-ref-glossary-face-fn
    :help-echo 'or-glossary-tooltip
    :export (lambda (path _ format)
	      (cond
	       ((memq format '(latex beamer))
		(format "\\%s{%s}" command path))
	       (t
		(format "%s" path))))))


(org-ref-link-set-parameters "glslink"
  :follow #'or-follow-glossary
  :face 'org-ref-glossary-face-fn
  :help-echo 'or-glossary-tooltip
  :export (lambda (path desc format)
            (cond
             ((memq format '(latex beamer))
              (format "\\glslink{%s}{%s}" path desc))
	     (t
	      (format "%s" path)))))


;;** Tooltips on glossary entries
(defface org-ref-glossary-face
  `((t (:inherit org-link :foreground ,org-ref-glossary-color)))
  "Face for glossary links.")


(defun or-glossary-tooltip (_window _object position)
  "Return tooltip for the glossary entry.
The entry is in WINDOW and OBJECT at POSITION.
Used in fontification."
  (save-excursion
    (goto-char position)
    (let* ((label (org-element-property :path (org-element-context)))
	   (data (or (or-parse-glossary-entry label)
		     (or-parse-acronym-entry label)))
	   (name (or (plist-get data :name)
		     (plist-get data :abbrv)))
	   (description (or (plist-get data :description)
			    (plist-get data :full))))
      (format
       "%s: %s"
       name
       (with-temp-buffer
	 (insert (concat  description "."))
	 (fill-paragraph)
	 (buffer-string))))))


(unless (fboundp 'org-link-set-parameters)
  (defun or-next-glossary-link (limit)
    "Search to next glossary link up to LIMIT.
Adds a tooltip to the link that is found."
    (when (and (re-search-forward
		(concat
		 (regexp-opt '("gls" "glspl"
			       "Gls" "Glspl"
			       "glslink"
			       "glssymbol"
			       "glsdesc"))
		 ":[a-zA-Z]\\{2,\\}")
		limit t)
	       (not (org-in-src-block-p))
	       (not (org-at-comment-p)))
      (forward-char -2)
      (let ((next-link (org-element-context)))
	(if next-link
	    (progn
	      (set-match-data (list (org-element-property :begin next-link)
				    (- (org-element-property :end next-link)
				       (org-element-property :post-blank next-link))))
	      (add-text-properties
	       (org-element-property :begin next-link)
	       (- (org-element-property :end next-link)
		  (org-element-property :post-blank next-link))
	       (list
		'help-echo 'or-glossary-tooltip))
	      (goto-char (org-element-property :end next-link)))
	  (goto-char limit)
	  nil)))))


;; ** exporting with a glossary table
(defun org-ref-glossary-before-parsing (backend)
  "Function to preprocess a glossary table on export.
This assumes a table like

#+name: glossary
| label | name  | description   |
|-------+-------+---------------|
| tree  | Tree  | A woody plant |
| shrub | Shrub | A woody bush  |

is in the org-buffer, and will add the relevant latex_header items if there is. The table is deleted in a copy of the buffer before the export.

This will run in `org-export-before-parsing-hook'."
  (let* (begin
	 end
	 (entries (save-excursion
		    (catch 'found
		      (org-element-map
			  (org-element-parse-buffer)
			  'table
			(lambda (el)
			  (when (and (org-element-property :name el)
				     (stringp (org-element-property :name el))
				     (string= "glossary" (org-element-property :name el)))
			    (setq begin (org-element-property :begin el)
				  end (org-element-property :end el))
			    (goto-char (org-element-property :contents-begin el))
			    (throw 'found
				   (nthcdr 2 (org-babel-read-table))))))))))
    ;; Delete the table
    (when entries
      (setf (buffer-substring begin end) "")

      (goto-char (point-min))
      (cl-loop for (label name description) in entries
	       do
	       (insert (format "#+latex_header_extra: \\newglossaryentry{%s}{name=%s,description={{%s}}}\n"
			       label name description))))))

(add-to-list 'org-export-before-parsing-hook 'org-ref-glossary-before-parsing)

;;* Acronyms
;;;###autoload
(defun org-ref-add-acronym-entry (label abbrv full)
  "Add an acronym entry with LABEL.
  ABBRV is the abbreviated form.
  FULL is the expanded acronym."
  (interactive "sLabel: \nsAcronym: \nsFull name: ")
  (save-excursion
    (re-search-backward "#\\+latex_header" nil t)
    (forward-line)
    (when (not (looking-at "^$"))
      (beginning-of-line)
      (insert "\n")
      (forward-line -1))

    (insert (format "#+latex_header_extra: \\newacronym{%s}{%s}{%s}\n"
		    label abbrv full))))


(defun or-parse-acronym-entry (label)
  "Parse an acronym entry LABEL to a plist.
  \(:abbrv abbrv :full full)
\newacronym{<label>}{<abbrv>}{<full>}"
  (save-excursion
    (goto-char (point-min))
    (let* (abbrv
	   full p1
	   (external (when (re-search-forward "\\loadglsentries\\(\\[.*\\]\\){\\(?1:.*\\)}" nil t)
		       (match-string 1)))
	   (glsentries (and external
			    (or (cdr (assoc external org-ref-glsentries))
				(progn
				  (pushnew (cons external (s-trim (shell-command-to-string
								   (format "kpsewhich tex %s" external))))
					   org-ref-glsentries)
				  (cdr (assoc external org-ref-glsentries)))))))
      (catch 'data
	(goto-char (point-min))
	;; check in the definitions of newacronym
	(when (re-search-forward (format "\\newacronym{%s}" label) nil t)
	  (setq p1 (+ 1 (point)))
	  (forward-list)
	  (setq abbrv (buffer-substring p1 (- (point) 1)))
	  (setq p1 (+ 1 (point)))
	  (forward-list)
	  (setq full (buffer-substring p1 (- (point) 1)))
	  (throw 'data
		 (list :abbrv abbrv :full full)))

	;; look for acronyms table
	(let* ((entries (save-excursion
			  (catch 'found
			    (org-element-map
				(org-element-parse-buffer)
				'table
			      (lambda (el)
				(when (string= "acronyms" (org-element-property :name el))
				  (goto-char (org-element-property :contents-begin el))
				  (throw 'found
					 (nthcdr 2 (org-babel-read-table)))))))))
	       (result (assoc label entries)))
	  (when result
	    (throw 'data (list :abbrv (cl-second result) :full (cl-third result)))))

	;; look external
	(when (and glsentries
		   (file-exists-p glsentries))
	  (with-current-buffer (find-file-noselect glsentries)
	    (goto-char (point-min))
	    (when (re-search-forward (format "\\newacronym{%s}" label) nil t)
	      (setq p1 (+ 1 (point)))
	      (forward-list)
	      (setq abbrv (buffer-substring p1 (- (point) 1)))
	      (setq p1 (+ 1 (point)))
	      (forward-list)
	      (setq full (buffer-substring p1 (- (point) 1)))
	      (throw 'data
		     (list :abbrv abbrv :full full)))))

	;; This should be a last resort, as it involves an export
	;; but, it supports #+include
	;; I think these will only show as the latex header because of the export
	;; (let ((org-export-show-temporary-export-buffer nil)
	;;       (org-export-use-babel nil))
	;;   (with-current-buffer (org-org-export-as-org)
	;;     (when (re-search-forward (format "\\newacronym{%s}" label) nil t)
	;;       (setq p1 (+ 1 (point)))
	;;       (forward-list)
	;;       (setq abbrv (buffer-substring p1 (- (point) 1)))
	;;       (setq p1 (+ 1 (point)))
	;;       (forward-list)
	;;       (setq full (buffer-substring p1 (- (point) 1)))
	;;       (throw 'data
	;; 	     (list :abbrv abbrv :full full)))))
	))))

;;** Acronym links
(defun or-follow-acronym (label)
  "Go to the definition of the acronym LABEL."
  (org-mark-ring-push)
  (goto-char (point-min))
  (re-search-forward (format "\\\\newacronym{%s}" label))
  (goto-char (match-beginning 0)))


(defvar org-ref-glossary-acr-commands-mapping
  '(("acrshort" . "acrshort")
    ("acrlong"  . "acrlong")
    ("acrfull"  . "acrfull")
    ("ac"       . "gls")
    ("Ac"       . "Gls")
    ("acp"      . "glspl")
    ("Acp"      . "Glspl")))


(dolist (mapping org-ref-glossary-acr-commands-mapping)
  (org-ref-link-set-parameters (car mapping)
    :follow #'or-follow-acronym
    :face 'org-ref-acronym-face-fn
    :help-echo 'or-acronym-tooltip
    :export (lambda (path _ format)
	      (cond
	       ((memq format '(latex beamer))
		(format "\\%s{%s}" (cdr mapping) path))
	       (t
		(format "%s" (upcase path)))))))


;;** Tooltips on acronyms
(defface org-ref-acronym-face
  `((t (:inherit org-link :foreground ,org-ref-acronym-color)))
  "Face for acronym links.")


(defun org-ref-acronym-face-fn (label)
  "Return a face for an acronym link."
  (save-match-data
    (cond
     ((or (not org-ref-show-broken-links)
	  (or-parse-acronym-entry label))
      'org-ref-acronym-face)
     (t
      'font-lock-warning-face))))


(defun or-acronym-tooltip (_window _object position)
  "Return tooltip for the acronym entry.
The entry is in WINDOW and OBJECT at POSITION.
Used in fontification.
WINDOW and OBJECT are ignored."
  (save-excursion
    (goto-char position)
    (let* ((label (org-element-property :path (org-element-context)))
	   (acronym-data (or-parse-acronym-entry label))
	   (abbrv (plist-get acronym-data :abbrv))
	   (full (plist-get acronym-data :full)))
      (if acronym-data
	  (format
	   "%s: %s"
	   abbrv full)
	(format "%s is not defined in this file." label)))))


;; We use search instead of a regexp to match links with descriptions. These are
;; hard to do with regexps.
(unless (fboundp 'org-link-set-parameters)
  (defun or-next-acronym-link (limit)
    "Search to next acronym link up to LIMIT and add a tooltip."
    (when (and (re-search-forward
		(concat
		 (regexp-opt '("acrshort" "acrfull" "acrlong" "ac" "Ac" "acp" "Acp"))
		 ":[a-zA-Z]\\{2,\\}")
		limit t)
	       (not (org-in-src-block-p))
	       (not (org-at-comment-p)))
      (save-excursion
	(forward-char -2)
	(let ((next-link (org-element-context)))
	  (if next-link
	      (progn
		(set-match-data
		 (list (org-element-property :begin next-link)
		       (- (org-element-property :end next-link)
			  (org-element-property :post-blank next-link))))
		(add-text-properties
		 (org-element-property :begin next-link)
		 (- (org-element-property :end next-link)
		    (org-element-property :post-blank next-link))
		 (list
		  'help-echo 'or-acronym-tooltip))
		(goto-char (org-element-property :end next-link)))
	    (goto-char limit)
	    nil))))))

;; ** Exporting with an acronym table
(defun org-ref-acronyms-before-parsing (backend)
  "Function to preprocess a glossary table on export.
This assumes a table like

#+name: acronyms
| Key  | Short | Long                           |
|------+-------+--------------------------------|
| mimo |       | multiple-input multiple output |
| qos  | QoS   | quality-of-service             |
| bb   | BB    | branch and bound               |

is in the org-buffer, and will add the relevant latex_header items if there is. The table is deleted in a copy of the buffer before the export.

This will run in `org-export-before-parsing-hook'."
  (let* (begin
	 end
	 (entries (save-excursion
		    (catch 'found
		      (org-element-map
			  (org-element-parse-buffer)
			  'table
			(lambda (el)
			  (when (and (org-element-property :name el)
				     (stringp (org-element-property :name el))
				     (string= "acronyms" (org-element-property :name el)))
			    (setq begin (org-element-property :begin el)
				  end (org-element-property :end el))
			    (goto-char (org-element-property :contents-begin el))
			    (throw 'found
				   (nthcdr 2 (org-babel-read-table))))))))))
    (when entries
      ;; Delete the table
      (setf (buffer-substring begin end) "")

      (goto-char (point-min))
      (cl-loop for (label name description) in entries
	       do
	       (insert (format "#+latex_header_extra: \\newacronym{%s}{%s}{%s}\n"
			       label name description))))))

(add-to-list 'org-export-before-parsing-hook 'org-ref-acronyms-before-parsing)

;; * Helm command to insert entries
;;;###autoload
(defun org-ref-insert-glossary-link ()
  "Helm command to insert glossary and acronym entries as links."
  (interactive)
  ;; gather entries
  (let ((glossary-candidates '())
	(acronym-candidates '())
	key
	entry)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
	      "\\\\newglossaryentry{\\([[:ascii:]]+?\\)}" nil t)
	(setq key (match-string 1)
	      entry (or-parse-glossary-entry key))
	(setq glossary-candidates
	      (append
	       glossary-candidates
	       (list
		(cons
		 ;; for helm
		 (format "%s: %s."
			 (plist-get entry :name)
			 (plist-get entry :description))
		 ;; the returned candidate
		 (list key
		       (plist-get entry :name))))))))

    ;; acronym candidates
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
	      "\\\\newacronym{\\([[:ascii:]]+?\\)}" nil t)
	(setq key (match-string 1)
	      entry (or-parse-acronym-entry key))
	(setq acronym-candidates
	      (append
	       acronym-candidates
	       (list
		(cons
		 ;; for helm
		 (format "%s (%s)."
			 (plist-get entry :full)
			 (plist-get entry :abbrv))
		 ;; the returned candidate
		 (list key
		       (plist-get entry :abbrv))))))))

    (helm :sources
	  `(,(helm-build-sync-source "Insert glossary term"
	       :candidates glossary-candidates
	       :action (lambda (candidate)
			 (insert (format
				  "[[%s:%s][%s]]"
				  (completing-read "Type: "
						   '("gls"
						     "glspl"
						     "Gls"
						     "Glspl"
						     "glssymbol"
						     "glsdesc")
						   nil t
						   "gls")
				  (nth 0 candidate)
				  (nth 1 candidate)))))
	    ,(helm-build-sync-source "Insert acronym term"
	       :candidates acronym-candidates
	       :action (lambda (candidate)
			 (insert (format
				  "[[%s:%s][%s]]"
				  (completing-read "Type: "
						   '("acrshort"
						     "acrlong"
						     "acrfull"
						     "ac"
						     "Ac"
						     "acp"
						     "Acp")
						   nil t
						   "ac")
				  (nth 0 candidate)
				  (nth 1 candidate)))))
	    ,(helm-build-sync-source "Add new term"
	       :candidates '(("Add glossary term" . org-ref-add-glossary-entry)
			     ("Add acronym term" . org-ref-add-acronym-entry))
	       :action (lambda (x)
			 (call-interactively x)))))))


(provide 'org-ref-glossary)
;;; org-ref-glossary.el ends here
