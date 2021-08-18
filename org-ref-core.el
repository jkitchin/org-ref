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
  'org-ref-ivy-cite
  "Symbol for library to define completion functions.
The completion library should provide functions for
`org-ref-insert-link-function', `org-ref-insert-cite-function',
`org-ref-insert-label-function', `org-ref-insert-ref-function',
and `org-ref-cite-onclick-function', and set those variables to
the values of those functions."
  :type 'symbol
  :options '(org-ref-ivy-cite)
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


(defvar org-ref-label-re
  "label:\\([a-zA-Z0-9_:-]+,?\\)+"
  "Regexp for label links.")


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
        (setq org-ref-bibliography-files
              (reftex-locate-bibliography-files default-directory))
        (when org-ref-bibliography-files
          (throw 'result org-ref-bibliography-files)))


      ;; we did not find anything. use defaults
      (setq org-ref-bibliography-files org-ref-default-bibliography)))


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
