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
;; This library provides hyper-functional citation links. These links can
;; contain common pre/post notes, and multiple citation keys that each have
;; their own pre/postnotes.
;;
;; These links are fontified to indicate if the citation keys are valid, and to
;; indicate the pre/post-note structure.
;;
;; Each link is functional, and clicking on one will open a hydra menu of
;; actions that range from opening the bibtex entry, notes, pdf or associated
;; URL, to searching the internet for related articles.
;;
;; Each citation link also has a local keymap on it, which provides keyboard
;; shortcuts for some actions like sorting, rearranging and navigating citation
;; links.
;;
;; Each link exports to a corresponding LaTeX citation command, or can be
;; rendered with CSL for other kinds of exports like HTML, markdown, or ODT.
;;
;;
;;; Code:
(require 'hydra)

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
    "citeyear"  "citeyearpar"
    "Citet" "Citep" "Citealt" "Citealp" "Citeauthor"
    "Citet*" "Citep*" "Citealt*" "Citealp*" "Citeauthor*")
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
   org-ref-biblatex-multitypes
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


(defcustom org-ref-cite-insert-version 3
  "Default version to insert citations with.
The default is 3. In legacy documents you might prefer 2 though,
so this variable can be buffer- or directory local if you want.

version 2 means the links are not bracketed, and comma-separated keys.

version 3 means the links are bracketed, with semicolon-separated
@keys."
  :type 'number
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


;; * Parsing/interpreting a citation path

(defun org-ref-parse-cite-path (path)
  "Return a data structure representing the PATH.
the data structure is a plist with (:version :prefix :suffix :references).
Each reference is a plist with (:key :prefix :suffix)."
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

	 (setq these-results
	       (append these-results
		       (list
			:references
			(cl-loop for s in citation-references collect
				 (if (null (string-match org-ref-citation-key-re s))
				     (error "No label found")
				   (let* ((key (match-string-no-properties 1 s))
					  (key-start (match-beginning 0))
					  (key-end (match-end 0))
					  (prefix (let ((p (substring s 0 key-start)))
						    (if (string= "" (string-trim p))
							nil
						      p)))
					  (suffix (let ((s (substring s key-end)))
						    (if (string= "" (string-trim s))
							nil
						      s))))
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


;; * Activating citation links
;;
;; We use the activate-func for sophisticated fontification of pieces of each
;; link.

(defvar-local org-ref-buffer-local-bib nil
  "Buffer-local variable for current bibliography files.
This is used to tell when the local bibliography has changed,
which means we need to re-compute the valid-keys.")


(defvar-local org-ref-buffer-local-valid-keys nil
  "Buffer-local variable for current valid keys.
This is used to cache the valid keys.")

(defvar-local org-ref-buffer-local-candidates nil
  "Buffer-local variable to store completion candidates.")

(defun org-ref-clear-cache ()
  (interactive)
  (setq-local org-ref-buffer-local-bib nil
	      org-ref-buffer-local-valid-keys nil
	      org-ref-buffer-local-candidates nil))


(defun org-ref-cache-valid-p ()
  "Return non-nil if cache is valid."
  (let ((current-bib (cl-loop for bibfile in (org-ref-find-bibliography)
			      collect
			      (cons bibfile
				    (file-attribute-modification-time (file-attributes bibfile))))))
    (equal current-bib org-ref-buffer-local-bib)))


(defun org-ref-refresh-cache ()
  "Refresh the cache."
  (let (valid-keys local-candidates)
    (setq-local
     org-ref-buffer-local-bib (cl-loop for bibfile in (org-ref-find-bibliography)
				       collect
				       (cons bibfile
					     (file-attribute-modification-time (file-attributes bibfile))))
     org-ref-buffer-local-valid-keys nil
     org-ref-buffer-local-candidates nil)

    (let ((bibtex-completion-bibliography (mapcar 'car org-ref-buffer-local-bib)))
      (bibtex-completion-init)
      (cl-loop for entry in (bibtex-completion-candidates)
	       do
	       (push (cdr (assoc "=key=" (cdr entry))) valid-keys)
	       (push (cons (bibtex-completion-format-entry entry (1- (frame-width)))
			   (cdr entry))
		     local-candidates)))

    (setq-local org-ref-buffer-local-candidates (reverse local-candidates)
		org-ref-buffer-local-valid-keys valid-keys)))


(defun org-ref-valid-keys ()
  "Return a list of valid bibtex keys for this buffer.
This is used a lot in `org-ref-cite-activate' so we try to cache
it as a local variable, but also detect when the cache is
invalid, e.g. if you change the bibliographies."
  (unless (org-ref-cache-valid-p)
    (org-ref-refresh-cache))
  org-ref-buffer-local-valid-keys)


(defun org-ref-cite-activate (start end path _bracketp)
  "Activation function for a cite link.
START and END are the bounds of the link.
PATH has the citations in it."
  (let* ((valid-keys (org-ref-valid-keys)) ;; this is cached in a buffer local var.
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

		  ;; Look for a key. common pre/post notes do not have keys in them.
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
		      (put-text-property key-begin key-end
					 'face 'font-lock-warning-face)
		      (put-text-property key-begin key-end
					 'help-echo "Key not found")))))))))


;; * Following citation links

(defhydra org-ref-citation-hydra (:color blue :hint nil)
  "Citation actions
"
  ("o" org-ref-open-citation-at-point  "Bibtex" :column "Open")
  ("p" org-ref-open-pdf-at-point "PDF" :column "Open")
  ("n" org-ref-open-notes-at-point "Notes" :column "Open")
  ("u" org-ref-open-url-at-point "URL" :column "Open")

  ;; WWW actions
  ("ww" org-ref-wos-at-point "WOS" :column "WWW")
  ("wr" org-ref-wos-related-at-point "WOS related" :column "WWW")
  ("wc" org-ref-wos-citing-at-point "WOS citing" :column "WWW")
  ("wg" org-ref-google-scholar-at-point "Google Scholar" :column "WWW")
  ("wp" org-ref-pubmed-at-point "Pubmed" :column "WWW")
  ("wc" org-ref-crossref-at-point "Crossref" :column "WWW")
  ("wb" org-ref-biblio-at-point "Biblio" :column "WWW")
  ("e" org-ref-email-at-point "Email" :column "WWW")

  ;; Copyish actions
  ("K" org-ref-copy-entry-as-summary "Copy bibtex" :column "Copy")
  ("k" (kill-new (car (org-ref-get-bibtex-key-and-file))) "Copy key" :column "Copy")
  ("f" (kill-new (bibtex-completion-apa-format-reference
		  (org-ref-get-bibtex-key-under-cursor)))
   "Copy formatted" :column "Copy")

  ;; Editing actions
  ("<left>" org-ref-cite-shift-left "Shift left" :color red :column "Edit")
  ("<right>" org-ref-cite-shift-right "Shift right" :color red :column "Edit")
  ("<up>" org-ref-sort-citation-link "Sort by year" :column "Edit")
  ("i" (funcall org-ref-insert-cite-function) "Insert cite" :column "Edit")
  ("t" org-ref-change-cite-type "Change cite type" :column "Edit")
  ("d" org-ref-delete-citation-at-point "Delete at point" :column "Edit")
  ("r" org-ref-replace-citation-at-point "Replace cite" :column "Edit")

  ;; Navigation
  ("[" org-ref-previous-key "Previous key" :column "Navigation" :color red)
  ("]"  org-ref-next-key "Next key" :column "Navigation" :color red)
  ("v" org-ref-jump-to-visible-key "Visible key" :column "Navigation" :color red)
  ("q" nil "Quit"))


(defun org-ref-cite-follow (_path)
  "Follow a cite link."
  (org-ref-citation-hydra/body))

;; * Citation links tooltips

(defun org-ref-cite-tooltip (_win _obj position)
  "Get a tooltip for the cite at POSITION."
  (let ((key (get-text-property position 'cite-key)))
    (when key
      (let ((bibtex-completion-bibliography (org-ref-find-bibliography))
	    (has-pdf (when (bibtex-completion-find-pdf key) bibtex-completion-pdf-symbol))
	    (has-notes (when (cl-some #'identity
				      (mapcar (lambda (fn)
						(funcall fn key))
					      bibtex-completion-find-note-functions))
			 bibtex-completion-notes-symbol)))
	(format "%s%s %s" (or has-pdf "") (or has-notes "")
		(bibtex-completion-apa-format-reference key))))))


;; * Exporting citation links

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
	  (let* ((prefix-suffix (split-string (or desc "") "::"))
		 (prefix (cond
			  ((and (cl-first prefix-suffix) (not (string= "" (cl-first prefix-suffix))))
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
			("prefix" . ,(string-trim prefix))
			("suffix" . ,(string-trim suffix))
			("keys" . ,(string-join keys ","))))))
	 (3
	  (s-format "\\${cmd}${prefix}${suffix}{${keys}}" 'aget
		    `(("cmd" . ,cmd)
		      ;; if there is more than one key, we only do global
		      ;; prefix/suffix But for one key, we should allow local
		      ;; prefix and suffix or the global one.
		      ("prefix" . ,(if (= 1 (length references))
				       ;; single reference
				       (cond
					;; local prefix is not empty, we use it.
					((plist-get (car references) :prefix)
					 (concat "["
						 (string-trim (plist-get (car references) :prefix))
						 "]"))
					;; local prefix is empty, but global one
					;; is not, so we use it
					((plist-get cite :prefix)
					 (concat "["
						 (string-trim (plist-get cite :prefix))
						 "]"))
					;; if you have a suffix, you need an empty prefix
					((plist-get cite :suffix)
					 "[]")
					(t
					 ""))
				     ;; Multiple references
				     (cond
				      ;; Check the common prefix
				      ((plist-get cite :prefix)
				       (concat "["
					       (string-trim (plist-get cite :prefix))
					       "]"))
				      ;; Check the prefix in the first cite
				      ((plist-get (car references) :prefix)
				       (concat "["
					       (string-trim (plist-get (car references) :prefix))
					       "]"))
				      ;; if you have a suffix, you need an empty prefix
				      ((plist-get cite :suffix)
				       "[]")
				      (t
				       ""))))
		      ("suffix" . ,(if (= 1 (length references))
				       ;; Single reference
				       (cond
					;; local prefix is not empty, so use it
					((plist-get (car references) :suffix)
					 (format "[%s]"
						 (string-trim (plist-get (car references) :suffix))))
					;; global prefix is not empty
					((plist-get cite :suffix)
					 (format "[%s]" (string-trim (plist-get cite :suffix))))
					(t
					 ""))
				     ;; Multiple references
				     (cond
				      ((plist-get cite :suffix)
				       (format "[%s]" (string-trim (plist-get cite :suffix))))
				      ;; last reference has a suffix
				      ((plist-get (car (last references)) :suffix)
				       (format "[%s]" (string-trim (plist-get (car (last references)) :suffix))))
				      (t
				       ""))))
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
		   ("keys" . ,(string-join
			       (cl-loop for ref in (plist-get cite :references)
					collect
					(format "%s%s{%s}"
						(cond
						 ;; we have a prefix, stick it in
						 ((not (string= ""
								(or (plist-get ref :prefix) "")))
						  (concat "[" (plist-get ref :prefix) "]"))
						 ;; no prefix, but a suffix, so
						 ;; empty prefix for placeholder
						 ((not (string= ""
								(or (plist-get ref :suffix) "")))
						  "[]")
						 (t
						  ""))
						(cond
						 ((not (string= ""
								(or (plist-get ref :suffix) "")))
						  (concat "[" (plist-get ref :suffix) "]"))
						 (t
						  ""))
						(plist-get ref :key)))))))))))

;; * Completion for citation links
;;
;; This allows you to type C-c l, choose a cite link type, and then insert a key.

(defun org-ref-cite-link-complete (cmd &optional arg)
  "Cite link completion for CMD."
  (concat
   cmd ":"
   "@" (org-ref-read-key)))

;; * Generate all the links
;;
;; We loop on the three categories because there are some differences between
;; them, mostly in the multitypes.

(cl-loop for cmd in (append org-ref-natbib-types
			    org-ref-biblatex-types)
	 do
	 (org-link-set-parameters
	  cmd
	  :complete (apply-partially #'org-ref-cite-link-complete cmd)
	  :follow #'org-ref-cite-follow
	  :face 'org-ref-cite-face
	  :help-echo #'org-ref-cite-tooltip
	  :export (apply-partially 'org-ref-cite-export  cmd)
	  :activate-func #'org-ref-cite-activate))


(cl-loop for cmd in org-ref-biblatex-multitypes do
	 (org-link-set-parameters
	  cmd
	  :complete (apply-partially #'org-ref-cite-link-complete cmd)
	  :follow #'org-ref-cite-follow
	  :face 'org-ref-cite-face
	  :help-echo #'org-ref-cite-tooltip
	  :export (apply-partially 'org-ref-multicite-export cmd)
	  :activate-func #'org-ref-cite-activate))


;; * Cite link utilities

;;;###autoload
(defun org-ref-delete-citation-at-point ()
  "Delete the citation or reference at point."
  (interactive)
  (let* ((object (org-element-context))
         (type (org-element-property :type object))
         (begin (org-element-property :begin object))
         (end (org-element-property :end object))
         (link-string (org-element-property :path object))
	 (data (org-ref-parse-cite-path link-string))
	 (references (plist-get data :references))
	 (cp (point))
         key keys i)
    ;;   We only want this to work on citation links
    (when (-contains? org-ref-cite-types type)
      (setq key (org-ref-get-bibtex-key-under-cursor))
      (if (null key)
	  ;; delete the whole cite
	  (cl--set-buffer-substring begin end "")
	(setq i (seq-position references key (lambda (el key)
					       (string= key (plist-get el :key)))))
	;; delete i'th reference
	(setq references (-remove-at i references))
	(setq data (plist-put data :references references))
	(save-excursion
	  (goto-char begin)
	  (re-search-forward link-string)
	  (replace-match (org-ref-interpret-cite-data data)))
	(goto-char cp)))))


;;;###autoload
(defun org-ref-replace-citation-at-point ()
  "Replace the citation at point."
  (interactive)
  (let* ((object (org-element-context))
         (type (org-element-property :type object))
         (begin (org-element-property :begin object))
         (end (org-element-property :end object))
         (link-string (org-element-property :path object))
	 (data (org-ref-parse-cite-path link-string))
	 (references (plist-get data :references))
	 (cp (point))
         key keys i)
    ;;   We only want this to work on citation links
    (when (-contains? org-ref-cite-types type)
      (setq key (org-ref-get-bibtex-key-under-cursor))

      (if (null key)
	  ;; delete the whole cite
	  (cl--set-buffer-substring begin end "")
	(setq i (seq-position references key (lambda (el key) (string= key (plist-get el :key))))) ;; defined in org-ref
	(setf (plist-get (nth i references) :key)  (org-ref-read-key))

	(setq data (plist-put data :references references))
	(save-excursion
	  (goto-char begin)
	  (re-search-forward link-string)
	  (replace-match (org-ref-interpret-cite-data data)))
	(goto-char cp)))))


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
  "Return key under the cursor in org-mode.
If not on a key, but on a cite, prompt for key."
  (if-let ((key (get-text-property (point) 'cite-key)))
      key
    (let ((el (org-element-context))
	  data
	  keys)
      (when (and
	     (eq (org-element-type el) 'link)
	     (member (org-element-property :type el) org-ref-cite-types))
	(goto-char (org-element-property :begin el))
	(setq data (org-ref-parse-cite-path (org-element-property :path el))
	      keys (cl-loop for ref in (plist-get data :references) collect (plist-get ref :key)))
	(cond
	 ((= 1 (length keys))
	  (search-forward (car keys))
	  (goto-char (match-beginning 0)))
	 ;; multiple keys
	 (t
	  (setq key (completing-read "Key: " keys))
	  (search-forward key)
	  (goto-char (match-beginning 0))))
	(get-text-property (point) 'cite-key)))))


;; ** Shift-arrow sorting of keys in a cite link

(defun org-ref-swap-list-elements (i j lst)
  "Swap index I and J in the list LST."
  (let ((tempi (nth i lst)))
    (setf (nth i lst) (nth j lst))
    (setf (nth j lst) tempi))
  lst)


(defun org-ref-swap-citation-link (direction)
  "Move citation at point in DIRECTION +1 is to the right, -1 to the left."
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
          (org-ref-swap-list-elements i (min (+ i 1) (- (length references) 1)) references)
        (org-ref-swap-list-elements i (max (- i 1) 0) references))
      (setq data (plist-put data :references references))

      ;; and replace the link with the sorted keys
      (save-excursion
	(goto-char begin)
	(re-search-forward link-string)
	(replace-match (org-ref-interpret-cite-data data)))
      ;; now go forward to key so we can move with the key
      (goto-char begin)
      (re-search-forward key)
      (goto-char (match-beginning 0)))))


(defun org-ref-cite-shift-left ()
  "Shift reference at point to the left."
  (interactive)
  (org-ref-swap-citation-link -1))


(defun org-ref-cite-shift-right ()
  "Shift citation at point to the right."
  (interactive)
  (org-ref-swap-citation-link +1))


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


;;;###autoload
(defun org-ref-jump-to-visible-key ()
  "Jump to a visible key with avy."
  (interactive)
  (avy-with avy-goto-typo
    (avy-process
     (apply #'append
	    (save-excursion
	      (org-element-map (org-element-parse-buffer) 'link
		(lambda (c)
		  (when (member (org-element-property :type c) org-ref-cite-types)
		    (goto-char (org-element-property :begin c))
		    (let* ((path (org-element-property :path c))
			   (data (org-ref-parse-cite-path path))
			   (references (plist-get data :references)))
		      (append (list (org-element-property :begin c))
			      (cl-loop for ref in references collect
				       (progn
					 (search-forward (plist-get ref :key))
					 (match-beginning 0)))))))))))
    (avy--style-fn avy-style)))


;; * Insert links

;; The formatting is adapted from ivy-bibtex-transformer. I feel like it is
;; slower than ivy-bibtex though. It is completion agnostic though...
(defun org-ref-read-key ()
  "Read a key with completion."
  (unless bibtex-completion-display-formats-internal
    (bibtex-completion-init))
  (let* ((bibtex-completion-bibliography (org-ref-find-bibliography))
	 (candidates (mapcar (lambda (entry)
			       (cons (bibtex-completion-format-entry entry (1- (frame-width)))
				     (cdr entry)))
			     (bibtex-completion-candidates)))
	 (choice (completing-read "BibTeX entries: " candidates)))
    (cdr (assoc "=key=" (assoc choice candidates)))))


(defun org-ref-insert-cite-key (key)
  "Insert KEY at point as a cite link.
Rules:
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
	    key-at-point (get-text-property (point) 'cite-key))

      ;; There are two scenarios where key-at-point is null
      ;; 1. on the link-type before the :
      ;; 2. at the end of the link
      ;; Either way we just go to the end.
      (when (null key-at-point)
	;; that failed, so move to the last one
	;; This seems weird, but when you insert several marked candidates the point does weird things.
	(goto-char (org-element-property :end object))
	(skip-chars-backward " ")
	(setq key-at-point (plist-get (car (last references)) :key)))


      ;; this is index of selected key
      (setq index (seq-position references key-at-point
				(lambda (el1 key-at-point)
				  (string= key-at-point (plist-get el1 :key)))))

      (setq data (plist-put data :references
			    (-insert-at
			     (+ index (if (and (= 3 version) (looking-at "@"))
					  0
					+1))
			     (list :key key) references)))

      (pcase org-ref-cite-insert-version
	(2
	 (cl--set-buffer-substring
	  (org-element-property :begin object)
	  (org-element-property :end object)
	  (concat type ":" (string-join (cl-loop for ref in references
						 collect (plist-get ref :key))))))
	(3 (cl--set-buffer-substring
	    (org-element-property :begin object)
	    (org-element-property :end object)
	    (concat "[[" type ":" (org-ref-interpret-cite-data data) "]]"))))


      ;; Now get to the end of the key you just put in.
      (setq object (org-element-context))
      (goto-char (org-element-property :end object))
      (skip-chars-backward " "))))


(defun org-ref-insert-cite-keys (keys)
  "Insert KEYS as citation links."
  (cl-loop for key in keys
	   do
	   (org-ref-insert-cite-key key)))


;;;###autoload
(defun org-ref-insert-cite-link ()
  "Insert a cite link with completion."
  (interactive)
  (org-ref-insert-cite-key (org-ref-read-key)))


;; * natmove like pre-processing
;;
;; I think that citations belong in the sentence where they are used, which
;; means on the left side of punctuation. However, for some citation styles,
;; especially superscripts, it is nicer if they appear on the right hand side of
;; punctuation. achemso in LaTeX provides natmove
;; (https://ctan.org/pkg/natmove?lang=en) for this. It doesn't seem to work for
;; all LaTeX styles though, and in particular only works on the cite command
;; itself. So, Here is a preprocessor function you can use to move all the
;; cites.

(defun org-ref-cite-natmove (_backend)
  "Move citations to the right side of punctuation.
Intended for use in `org-export-before-parsing-hook'.

Here is an example use:

  (let ((org-export-before-parsing-hook '(org-ref-cite-natmove)))
    (org-open-file (org-latex-export-to-pdf)))"
  (let ((cites (org-ref-get-cite-links))
	;; temp point holders
	p1 p2
	punct)
    (cl-loop for cite in (reverse cites) do
	     (goto-char (org-element-property :end cite))
	     (skip-chars-backward " ")
	     (when (string-match-p "[[:punct:]]" (buffer-substring (point) (+ (point) 1)))
	       (setq punct (buffer-substring (point) (+ (point) 1)))
	       ;; delete the punctuation
	       (cl--set-buffer-substring (point) (+ (point) 1) "")
	       ;; and insert it at the beginning of the link.
	       (goto-char (org-element-property :begin cite))
	       ;; delete spaces backward
	       (skip-chars-backward " ")
	       (cl--set-buffer-substring (point) (org-element-property :begin cite) "")
	       (insert punct)))))


(provide 'org-ref-citation-links)

;;; org-ref-citation-links.el ends here
