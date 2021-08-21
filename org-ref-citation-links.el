;;; org-ref-citation-links.el --- citation links for org-ref
;;
;; Copyright (C) 2021  John Kitchin

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
;;

;;; Code:
(defface org-ref-cite-face
  `((t (:inherit org-link
                 :foreground "forest green")))
  "Color for cite-like links in org-ref.")


(defface org-ref-bad-cite-key-face
  `((t (:inherit org-ref-cite-face
		 :foreground "red")))
  "Color for bad cite keys in org-ref.")


(defface org-ref-cite-global-prefix/suffix-face
  `((t (:inherit org-ref-cite-face :weight bold)))
  "Face for global prefix/suffix in a cite link.")


(defface org-ref-cite-local-prefix/suffix-face
  `((t (:inherit org-ref-cite-face :slant italic)))
  "Face for local prefix/suffix in a cite link.")

(defface org-ref-cite-invalid-local-prefix/suffix-face
  `((t (:inherit org-ref-cite-face :foreground "red")))
  "Face for invalid local prefix/suffix in a cite link.
This is mostly for multicites and natbib.")


(defcustom org-ref-default-citation-link
  "cite"
  "The default type of citation link to use."
  :type 'string
  :group 'org-ref)


(defcustom org-ref-natbib-types
  '("cite" "nocite"
    "citet" "citet*" "citep" "citep*"
    "citealt" "citealt*" "citealp" "citealp*"
    "citenum" "citetext"
    "citeauthor" "citeauthor*"
    "citeyear" "citeyear*" "citeyearpar"
    "Citet" "Citep" "Citealt" "Citealp" "Citeauthor")
  "Natbib commands can have many references, and global prefix/suffix text.
For natbib cite commands see
http://tug.ctan.org/macros/latex/contrib/natbib/natnotes.pdf"
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
    "fnotecite")
  "biblatex commands.
Biblatex commands
https://mirrors.ibiblio.org/CTAN/macros/latex/contrib/biblatex/doc/biblatex.pdf"
  :type '(repeat :tag "List of citation types" string)
  :group 'org-ref)


(defcustom org-ref-biblatex-multitypes
  '("cites" "Cites" "parencites" "Parencites"
    "footcites" "footcitetexts"
    "smartcites" "Smartcites" "textcites" "Textcites"
    "supercites" "autocites" "Autocites")
  "Multicite link types"
  :type '(repeat :tag "List of citation types" string)
  :group 'org-ref)


(defcustom org-ref-cite-types
  (append
   org-ref-natbib-types
   org-ref-biblatex-types
   ;; for the bibentry package
   '("bibentry"))
  "List of citation types known in `org-ref'."
  :type '(repeat :tag "List of citation types" string)
  :group 'org-ref)


(defcustom org-ref-cite-keymap
  (let ((map (copy-keymap org-mouse-map)))
    ;; Navigation keys
    (define-key map (kbd "C-<left>") 'org-ref-previous-key)
    (define-key map (kbd "C-<right>") 'org-ref-next-key)

    ;; rearrangement keys
    (define-key map (kbd "S-<left>") (lambda () (interactive) (org-ref-swap-citation-link -1)))
    (define-key map (kbd "S-<right>") (lambda () (interactive) (org-ref-swap-citation-link 1)))
    (define-key map (kbd "S-<up>") 'org-ref-sort-citation-link)
    (define-key map (kbd "<tab>") (lambda ()
				    (interactive)
				    (funcall org-ref-insert-cite-function)))
    map)
  "Keymap for cite links."
  :type 'symbol
  :group 'org-ref)


(defvar org-ref-citation-key-re
  (rx "@" (group-n 1 (one-or-more (any word "-.:?!`'/*@+|(){}<>&_^$#%&~"))))
  "Numbered regular expression for a version 3 cite key.
Key is in group 1.
Adapted from the expression in org-cite.")


(defun org-ref-cite-version (path)
  "Return the version for PATH.
PATH is from a cite link.
Version 2 is separated by commas and uses plain keys.
Version 3 is separated by semicolons and uses @keys.
I think that if there is a @ in path, it must be version 3."
  (if (string-match "@" path)
      3
    2))


(defun org-ref-parse-cite-path (path)
  "Return a data structure representing the PATH."
  (pcase (org-ref-cite-version path)
    (2
     ;; This will not have any prefix or suffix, since that was previously done in the desc.
     (list :version 2 :references (cl-loop for key in (split-string path ",") collect
					   (list :key (string-trim key)))))
    (3 (let ((citation-references (split-string path ";"))
	     (these-results '(:version 3)))
	 ;; if the first ref doesn't match a key, it must be a global prefix
	 ;; this pops the referenc off.
	 (when (null (string-match org-ref-citation-key-re (cl-first citation-references)))
	   (setq these-results (append these-results (list :prefix (cl-first citation-references)))
		 citation-references (cdr citation-references)))

	 ;; if the last ref doesn't match a key, then it is a global suffix
	 ;; we remove the last one if this is true after getting the suffix.
	 (when (null (string-match org-ref-citation-key-re (car (last citation-references))))
	   (setq these-results (append these-results (list :suffix (car (last citation-references))))
		 citation-references (butlast citation-references)))

	 (setq these-results (append these-results
				     (list
				      :references
				      (cl-loop for s in citation-references collect
					       (if (null (string-match org-ref-citation-key-re s))
						   (error "No label found")
						 (let* ((key (match-string-no-properties 1 s))
							(key-start (match-beginning 0))
							(key-end (match-end 0))
							(prefix (substring s 0 key-start))
							(suffix (substring s key-end)))
						   (list :key key :prefix prefix :suffix suffix)))))))))))


(defun org-ref-interpret-cite-data (data)
  "Interpret the DATA structure from `org-ref-parse-cite-path' back
to a path string."
  (pcase (plist-get data :version)
    (2
     (string-join (cl-loop for ref in (plist-get data :references) collect (plist-get ref :key)) ","))
    (3
     (concat
      (when-let (prefix (plist-get data :prefix)) (concat prefix ";"))
      (string-join (cl-loop for ref in (plist-get data :references) collect
			    (concat
			     (plist-get ref :prefix)
			     "@" (plist-get ref :key)
			     (plist-get ref :suffix)))
		   ";")
      (when-let (suffix (plist-get data :suffix)) (concat ";" suffix))))))


(defvar-local org-ref-buffer-local-bib nil
  "Buffer-local variable for current bibliography files")


(defvar-local org-ref-buffer-local-valid-keys nil
  "Buffer-local variable for current valid keys")


(defun org-ref-valid-keys ()
  "Return a list of valid bibtex keys for this buffer.
This is used a lot in `org-ref-cite-activate' so we try to cache
it as a local variable, but also detect when the cache is
invalid, e.g. if you change the bibliographies."
  (let ((current-bib (org-ref-find-bibliography)))
    (if (equal current-bib org-ref-buffer-local-bib)
	org-ref-buffer-local-valid-keys
      (setq-local
       org-ref-buffer-local-bib current-bib
       org-ref-buffer-local-valid-keys
       (let ((bibtex-completion-bibliography current-bib))
	 (cl-loop for entry in (bibtex-completion-candidates)
		  collect (cdr (assoc "=key=" (cdr entry)))))))))


(defun org-ref-cite-activate (start end path _bracketp)
  "Activation function for a cite link.
START and END are the bounds of the link.
PATH has the citations in it."
  (let* ((valid-keys org-ref-valid-keys) ;; this is cached in a buffer local var.
	 substrings)
    (goto-char start)
    (pcase (org-ref-cite-version path)
      (2
       (setq substrings (split-string path ","))
       (cl-loop for key in substrings
		do
		;; get to the substring
		(search-forward key end)
		(put-text-property (match-beginning 0)
				   (match-end 0)
				   'keymap
				   org-ref-cite-keymap)
		(put-text-property (match-beginning 0)
				   (match-end 0)
				   'cite-key
				   key)
		(unless (member (string-trim key) valid-keys)
		  (put-text-property (match-beginning 0)
				     (match-end 0)
				     'face 'org-ref-bad-cite-key-face)
		  (put-text-property (match-beginning 0)
				     (match-end 0)
				     'help-echo "Key not found"))))
      (3
       (setq substrings (split-string path ";"))
       (cl-loop for i from 0 for s in substrings
		do
		;; get to the substring
		(search-forward s end)
		(put-text-property (match-beginning 0)
				   (match-end 0)
				   'keymap
				   org-ref-cite-keymap)
		(let* (key-begin
		       key-end
		       key)

		  (save-match-data
		    (when (string-match org-ref-citation-key-re s)
		      (setq key (match-string-no-properties 1 s)
			    valid-key (member key valid-keys))))

		  ;; these are global prefix/suffixes
		  (when (and (or (= i 0)
				 (= i (- (length substrings) 1)))
			     (null key))
		    (put-text-property (match-beginning 0) (match-end 0)
				       'face 'org-ref-cite-global-prefix/suffix-face)
		    (put-text-property (match-beginning 0) (match-end 0)
				       'help-echo "Global prefix/suffix"))

		  ;; we have a key. we have to re-search to get its position
		  (when key
		    (save-excursion
		      (save-match-data
			(search-backward (concat "@" key))
			(setq key-begin (match-beginning 0)
			      key-end (match-end 0))))
		    ;; store key on the whole thing
		    (put-text-property (match-beginning 0)
				       (match-end 0)
				       'cite-key
				       key)

		    ;; fontify any prefix /suffix text
		    (put-text-property (match-beginning 0) key-begin
				       'face 'org-ref-cite-local-prefix/suffix-face)

		    (put-text-property key-end (match-end 0)
				       'face 'org-ref-cite-local-prefix/suffix-face)

		    ;; bad key activation
		    (unless valid-key
		      (message "%s is not in %s" key valid-keys)
		      (put-text-property key-begin key-end
					 'face 'font-lock-warning-face)
		      (put-text-property key-begin key-end
					 'help-echo "Key not found")))))))))


(defun org-ref-cite-follow (_path)
  "Follow a cite link."
  (org-ref-citation-hydra/body))


(defun org-ref-cite-tooltip (_win _obj position)
  "Get a tooltip for the cite at POSITION."
  (let ((key (get-text-property position 'cite-key)))
    (when key
      (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
	(bibtex-completion-apa-format-reference key)))))


(defun org-ref-cite-export (cmd path desc backend)
  "Export a cite link.
This supports the syntax:  \\cmd[optional prefix][optional suffix]{keys}
The prefix and suffix must be the global version. Local prefix/suffixes are ignored.
PATH contains the link path.
BACKEND is the export backend.
Use with apply-partially."
  (pcase backend
    ('latex
     (let* ((cite (org-ref-parse-cite-path path))
	    (references (plist-get cite :references))
	    (keys (cl-loop for ref in references collect
			   (plist-get ref :key))))
       (pcase (org-ref-cite-version path)
	 (2
	  (let* ((prefix-suffix (split-string desc "::"))
		 (prefix (cond
			  ((cl-first prefix-suffix)
			   (format "[%s]" (cl-first prefix-suffix)))
			  ((cl-second prefix-suffix)
			   "[]")
			  (t
			   "")))
		 (suffix (cond
			  ((cl-second prefix-suffix)
			   (format "[%s]" (cl-second prefix-suffix)))
			  (t
			   ""))))
	    (s-format "\\${cmd}${prefix}${suffix}{${keys}}" 'aget
		      `(("cmd" . ,cmd)
			("prefix" . ,prefix)
			("suffix" . ,suffix)
			("keys" . ,(string-join keys ","))))))
	 (3
	  (s-format "\\${cmd}${prefix}${suffix}{${keys}}" 'aget
		    `(("cmd" . ,cmd)
		      ;; if there is more than one key, we only do global prefix/suffix
		      ;; But for one key, we should allow local prefix and suffix.
		      ("prefix" . ,(if (= 1 (length references))
				       (cond
					((plist-get (car references) :prefix)
					 (concat "[" (plist-get (car references) :prefix) "]"))
					;; if you have a suffix, you need an empty prefix
					((plist-get cite :suffix)
					 "[]")
					(t
					 ""))
				     (cond
				      ((plist-get cite :prefix)
				       (concat "[" (plist-get cite :prefix) "]"))
				      ;; if you have a suffix, you need an empty prefix
				      ((plist-get cite :suffix)
				       "[]")
				      (t
				       ""))))
		      ("suffix" . ,(if (= 1 (length references))
				       (if (plist-get (car references) :suffix)
					   (format "[%s]" (plist-get (car references) :suffix))
					 "")
				     (if (plist-get cite :suffix)
					 (format "[%s]" (plist-get cite :suffix))
				       "")))
		      ("keys" . ,(string-join keys ","))))))))))


(defun org-ref-multicite-export (cmd path _desc backend)
  "Export a multicite link.
This supports the syntax:  \\cmd(multiprenote)(multipostnote)[prenote][postnote]{key1}...[prenote][postnote]{key}
PATH contains the link path.
BACKEND is the export backend.
Use with apply-partially."
  (pcase backend
    ('latex
     (let ((cite (org-ref-parse-cite-path path)))
       (s-format "\\${cmd}${global-prefix}${global-suffix}${keys}" 'aget
		 `(("cmd" . ,cmd)
		   ("global-prefix" . ,(cond
					((plist-get cite :prefix)
					 (concat "(" (plist-get cite :prefix) ")"))
					;; if you have a suffix, you need an empty prefix
					((plist-get cite :suffix)
					 "()")
					(t
					 "")))
		   ("global-suffix" . ,(if (not (string= "" (or (plist-get cite :suffix) "")))
					   (format "(%s)" (plist-get cite :suffix))
					 ""))
		   ("keys" . ,(string-join (cl-loop for ref in (plist-get cite :references) collect
						    (format "%s%s{%s}"
							    (cond
							     ;; we have a prefix, stick it in
							     ((not (string= "" (or (plist-get ref :prefix) "")))
							      (concat "[" (plist-get ref :prefix) "]"))
							     ;; no prefix, but a suffix, so empty prefix for placeholder
							     ((not (string= "" (or (plist-get ref :suffix) "")))
							      "[]")
							     (t
							      ""))
							    (cond
							     ((not (string= "" (or (plist-get ref :suffix) "")))
							      (concat "[" (plist-get ref :suffix) "]"))
							     (t
							      ""))
							    (plist-get ref :key)))))))))))


(cl-loop for cmd in org-ref-natbib-types do
	 (org-link-set-parameters
	  cmd
	  :follow #'org-ref-cite-follow
	  :face 'org-ref-cite-face
	  :help-echo #'org-ref-cite-tooltip
	  :export (apply-partially 'org-ref-cite-export  cmd)
	  :activate-func #'org-ref-cite-activate))


(cl-loop for cmd in org-ref-biblatex-types do
	 (org-link-set-parameters
	  cmd
	  :follow #'org-ref-cite-follow
	  :face 'org-ref-cite-face
	  :help-echo #'org-ref-cite-tooltip
	  :export (apply-partially 'org-ref-cite-export  cmd)
	  :activate-func #'org-ref-cite-activate))


(cl-loop for cmd in org-ref-biblatex-multitypes do
	 (org-link-set-parameters
	  cmd
	  :follow #'org-ref-cite-follow
	  :face 'org-ref-cite-face
	  :help-echo #'org-ref-cite-tooltip
	  :export (apply-partially 'org-ref-multicite-export cmd)
	  :activate-func #'org-ref-cite-activate))


;; * Cite link utilities

;;;###autoload
(defun org-ref-change-cite-type ()
  "Change the cite type of citation link at point."
  (interactive)
  (let* ((new-type (completing-read "Type: " org-ref-cite-types))
	 (cite-link (org-element-context))
	 (cp (point)))
    (cl--set-buffer-substring
     (org-element-property :begin cite-link)
     (org-element-property :end cite-link)
     (org-element-interpret-data
      (org-element-create 'link
			  `(:type ,new-type
				  :path ,(org-element-property :path cite-link)
				  :contents-begin ,(org-element-property :contents-begin cite-link)
				  :contents-end ,(org-element-property :contents-end  cite-link)))))
    (goto-char cp)))


(defun org-ref-get-bibtex-key-under-cursor ()
  "Return key under the cursor in org-mode."
  (get-text-property (point) 'cite-key))


;;** Shift-arrow sorting of keys in a cite link
(defun org-ref-swap-list-elements (i j lst)
  "Swap index I and J in the list LST."
  (let ((tempi (nth i lst)))
    (setf (nth i lst) (nth j lst))
    (setf (nth j lst) tempi))
  lst)


;;;###autoload
(defun org-ref-swap-citation-link (direction)
  "Move citation at point in DIRECTION +1 is to the right, -1 to the left."
  (interactive)
  (let* ((object (org-element-context))
         (type (org-element-property :type object))
         (begin (org-element-property :begin object))
         (end (org-element-property :end object))
         (link-string (org-element-property :path object))
	 (data (org-ref-parse-cite-path link-string))
	 (references (plist-get data :references))
         key keys i)
    ;;   We only want this to work on citation links
    (when (-contains? org-ref-cite-types type)
      (setq key (org-ref-get-bibtex-key-under-cursor))
      (setq i (seq-position references key (lambda (el key) (string= key (plist-get el :key))))) ;; defined in org-ref
      (if (> direction 0) ;; shift right
          (org-ref-swap-keys i (min (+ i 1) (- (length references) 1)) references)
        (org-ref-swap-keys i (max (- i 1) 0) references))
      (setq data (plist-put data :references references))

      ;; and replace the link with the sorted keys
      (save-excursion
	(goto-char begin)
	(re-search-forward link-string)
	(replace-match (org-ref-interpret-cite-data data)))
      ;; now go forward to key so we can move with the key
      (re-search-forward key)
      (goto-char (match-beginning 0)))))


;;** Sort cite in cite link

(defun org-ref-sort-citation-link ()
  "Replace link at point with sorted link by year."
  (interactive)
  (let* ((object (org-element-context))
         (type (org-element-property :type object))
         (begin (org-element-property :begin object))
         (end (org-element-property :end object))
         (link-string (org-element-property :path object))
	 (data (org-ref-parse-cite-path link-string))
	 (references (plist-get data :references)))

    (setq references (cl-sort (cl-loop for ref in references collect
				       (append ref (list :year (bibtex-completion-get-value
								"year"
								(bibtex-completion-get-entry
								 (plist-get ref :key))))))
			      (lambda (x y)
				(< (string-to-number (plist-get x :year))
				   (string-to-number (plist-get y :year))))))
    (setq data (plist-put data :references references))
    (save-excursion
      (goto-char begin)
      (re-search-forward link-string)
      (replace-match (org-ref-interpret-cite-data data)))))


;;** C-arrow navigation of cite keys
;;
;; These are a little tricky to understand to me. There are two calls because
;; when you are on a cite, and move, the first change is the boundary of the
;; current cite, and the second is the boundary of next cite.

;;;###autoload
(defun org-ref-next-key ()
  "Move cursor to the next cite key when on a cite link.
Otherwise run `right-word'. If the cursor moves off the link,
move to the beginning of the next cite link after this one."
  (interactive)
  (when-let (next (next-single-property-change (point) 'cite-key))
    (goto-char next))
  (unless (get-text-property (point) 'cite-key)
    (when-let (next (next-single-property-change (point) 'cite-key))
      (goto-char next))))


;;;###autoload
(defun org-ref-previous-key ()
  "Move cursor to the previous cite key when on a cite link.
Otherwise run `left-word'. If the cursor moves off the link,
move to the beginning of the previous cite link after this one."
  (interactive)
  (when-let (prev (previous-single-property-change (point) 'cite-key))
    (goto-char prev))
  (unless (get-text-property (point) 'cite-key)
    (when-let (prev (previous-single-property-change (point) 'cite-key))
      (goto-char prev))))


;; * Insert links

;; all these functions are defined in ivy-bibtex
(defcustom org-ref-citation-alternate-insert-actions
  '(("p" ivy-bibtex-open-pdf "Open PDF file (if present)")
    ("u" ivy-bibtex-open-url-or-doi "Open URL or DOI in browser")
    ;; this insert-citation only inserts an org-ref cite.
    ;; ("c" ivy-bibtex-insert-citation "Insert citation")
    ("r" ivy-bibtex-insert-reference "Insert reference")
    ("k" ivy-bibtex-insert-key "Insert BibTeX key")
    ("b" ivy-bibtex-insert-bibtex "Insert BibTeX entry")
    ("a" ivy-bibtex-add-PDF-attachment "Attach PDF to email")
    ("e" ivy-bibtex-edit-notes "Edit notes")
    ("s" ivy-bibtex-show-entry "Show entry")
    ("l" ivy-bibtex-add-pdf-to-library "Add PDF to library")
    ("f" (lambda (_candidate) (ivy-bibtex-fallback ivy-text)) "Fallback options"))
  "Alternate actions to do instead of inserting.")


(defun org-ref-insert-cite-key (key)
  "Rules:
1. at beginning of key, insert before it.
2. at middle or end of key, insert after it."
  (let* ((object (org-element-context))
	 (type (org-element-property :type object))
	 (cp (point))
	 link-string version data references key-at-point index)

    (if (null (member type org-ref-cite-types))
	;; Not on a link, so we just insert a cite
	(insert (format "[[%s:@%s]]" org-ref-default-citation-link key))

      ;; On a link somewhere, and we need to figure out what to do.
      (setq link-string (org-element-property :path object)
	    version (org-ref-cite-version link-string)
	    data (org-ref-parse-cite-path link-string)
	    references (plist-get data :references)
	    key-at-point (org-ref-get-bibtex-key-under-cursor))

      ;; There are two scenarios where key-at-point is null
      ;; 1. on the link-type before the :
      ;; 2. at the end of the link
      (when (null key-at-point)
	;; see if we are before the colon, and move to the first ref if so.
	(if (search-forward ":" (org-element-property :end object) t)
	    (progn
	      (search-forward (plist-get (cl-first references) :key))
	      (goto-char (match-beginning 0))
	      (when (= 3 version)
		(backward-char 1))
	      (setq key-at-point (plist-get (cl-first references) :key)))

	  ;; that failed, so move to the last one
	  (search-backward (plist-get (car (last references)) :key))
	  (when (= 3 version)
	    (backward-char 1))
	  (setq key-at-point (plist-get (car (last references)) :key))))

      ;; this is index of selected key
      (setq index (seq-position references key-at-point
				(lambda (el1 key-at-point)
				  (string= key-at-point (plist-get el1 :key)))))


      (save-excursion
	(goto-char (org-element-property :begin object))
	(search-forward key-at-point)
	(setq data (plist-put data :references
			      (-insert-at
			       (+ index (if (= cp (- (match-beginning 0) (if (= 3 version) 1 0)))
					    -1
					  +1))
			       (list :key key) references)))
	(goto-char (org-element-property :begin object))
	(search-forward link-string)
	(replace-match (org-ref-interpret-cite-data data)))
      (goto-char (org-element-property :begin object))
      (search-forward key)
      (goto-char (match-end 0))
      ;; for version 3 go back one to get on @
      (when (= 3 version)
	(backward-char 1)))))


(defun org-ref-cite-insert ()
  "Function for inserting a citation."
  (interactive)

  ;; I do this here in case you change the actions after loading this, so that
  ;; it should be up to date.
  (ivy-set-actions
   'org-ref-cite-insert
   org-ref-citation-alternate-insert-actions)

  (ivy-set-display-transformer 'org-ref-cite-insert 'ivy-bibtex-display-transformer)

  (bibtex-completion-init)
  (let* ((bibtex-completion-bibliography (org-ref-find-bibliography))
	 (candidates (bibtex-completion-candidates)))
    (ivy-read "BibTeX entries: " candidates
	      :caller 'org-ref-cite-insert
	      :action (lambda (candidate)
			(org-ref-insert-cite-key (cdr (assoc "=key=" (cdr candidate))))))))



(defhydra org-ref-citation-hydra (:color blue :hint nil)
  "Citation actions
"
  ("o" org-ref-open-citation-at-point  "Bibtex" :column "Open")
  ("p" org-ref-open-pdf-at-point "PDF" :column "Open")
  ("n" org-ref-open-notes-at-point "Notes" :column "Open")
  ("u" org-ref-open-url-at-point "URL" :column "Open")

  ("w" org-ref-wos-at-point "WOS" :column "WWW")
  ("r" org-ref-wos-related-at-point "WOS related" :column "WWW")
  ("c" org-ref-wos-citing-at-point "WOS citing" :column "WWW")
  ("g" org-ref-google-scholar-at-point "Google Scholar" :column "WWW")
  ("P" org-ref-pubmed-at-point "Pubmed" :column "WWW")
  ("C" org-ref-crossref-at-point "Crossref" :column "WWW")
  ("e" org-ref-email-at-point "Email" :column "WWW")

  ("K" org-ref-copy-entry-as-summary "Copy bibtex" :column "Copy")
  ("k" (kill-new (car (org-ref-get-bibtex-key-and-file))) "Copy key" :column "Copy")
  ("f" (kill-new (bibtex-completion-apa-format-reference
		  (org-ref-get-bibtex-key-under-cursor)))
   "Copy formatted" :column "Copy")

  ("<left>" org-ref-cite-shift-left "Shift left" :color red :column "Edit")
  ("<right>" org-ref-cite-shift-right "Shift right" :color red :column "Edit")
  ("<up>" org-ref-sort-citation-link "Sort by year" :column "Edit")
  ("i" (funcall org-ref-insert-cite-function) "Insert cite" :column "Edit")
  ("t" org-ref-change-cite-type "Change cite type" :column "Edit")
  ("q" nil "Quit"))




(provide 'org-ref-citation-links)

;;; org-ref-citation-links.el ends here
