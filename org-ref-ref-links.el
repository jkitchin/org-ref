;;; org-ref-ref-links.el --- cross-reference links for org-ref
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

(defcustom org-ref-ref-types
  '("ref" "eqref" "pageref" "nameref" "autoref" "cref" "Cref")
  "List of ref link types."
  :type '(repeat :tag "List of ref types" string)
  :group 'org-ref)


(defcustom org-ref-default-ref-type "ref"
  "Default ref link type to use when inserting ref links"
  :type 'string
  :group 'org-ref)


(defcustom org-ref-ref-color
  "dark red"
  "Color of ref like links."
  :type 'string
  :group 'org-ref)


(defface org-ref-ref-face
  `((t (:inherit org-link :foreground ,org-ref-ref-color)))
  "Face for ref links in org-ref.")


(defvar org-ref-label-re
  (rx (group-n 1 (one-or-more (any word "-.:?!`'/*@+|(){}<>&_^$#%&~"))))
  "Regexp for labels.")

(defvar org-ref-label-link-re
  (rx "label:" (group-n 1 (one-or-more (any word "-.:?!`'/*@+|(){}<>&_^$#%&~"))))
  "Regexp for label links.")


(defvar org-ref-ref-label-regexps
  (list
   ;; CUSTOM_ID in a heading
   (concat ":CUSTOM_ID:\\s-+" org-ref-label-re "\\_>")
   ;; #+name
   (concat "^\\s-*#\\+name:\\s-+" org-ref-label-re "\\_>")
   ;; labels in latex
   (concat "\\\\label{" org-ref-label-re "}")
   ;; A target, code copied from org-target-regexp and group 1 numbered.
   (let ((border "[^<>\n\r \t]"))
     (format "<<\\(?1:%s\\|%s[^<>\n\r]*%s\\)>>"
	     border border border))
   ;; A label link
   (concat "label:" org-ref-label-re "\\_>"))
  "List of regular expressions to labels.
The label should always be in group 1.")


(defun org-ref-change-ref-type (new-type)
  "Change the ref type to NEW-TYPE."
  (interactive (list (completing-read "Type: " org-ref-ref-types)))
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


(defun org-ref-get-labels ()
  "Return a list of referenceable labels in the document.
You can reference:
A NAME keyword
A CUSTOM_ID property on a heading
A LaTeX label
A target.
A label link.

See `org-ref-ref-label-regexps' for the patterns that find these.

I am not putting label links here for now to keep it totally
separate from `org-ref'. I think that the NAME keyword is
adequate for figures and tables, and CUSTOM_ID is ok for
headings. You can always fall back on the \\label syntax if you
need to.

Returns a list of cons cells (label . context).

It is important for this function to be fast, since we use it in
font-lock."
  (let ((rx (string-join org-ref-ref-label-regexps "\\|"))
	(labels '())
	context)
    (save-excursion
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward rx nil t)
	 (setq context (buffer-substring
			(save-excursion (forward-line -1) (point))
			(save-excursion (forward-line +2) (point))))
	 (cl-pushnew (cons (match-string-no-properties 1)
			   ;; This attempts to pad the context.
			   (string-join
			    (mapcar (lambda (s)
				      (concat (make-string 20 ? ) s))
				    (split-string context "\n"))
			    "\n"))
		     labels))))
    ;; reverse so they are in the order we find them.
    (delete-dups (reverse labels))))


;; TODO: Maybe this needs a hydra, e.g. to follow, replace? replace in buffer?, not sure what else.
(defun org-ref-ref-follow (_path)
  "Follow the ref link.
_PATH is ignored. We get the label from a text property so we
don't have to figure out which label you clicked on."
  (let ((label (get-text-property (point) 'org-ref-ref-label))
	(rx (string-join org-ref-ref-label-regexps "\\|")))
    (when label
      (org-mark-ring-push)
      (widen)
      (goto-char (point-min))
      (catch 'found
	(while (re-search-forward rx)
	  (when (string= label (match-string-no-properties 1))
	    (save-match-data (org-mark-ring-push))
	    (goto-char (match-beginning 1))
	    (org-show-entry)
	    (substitute-command-keys
	     "Go back with (org-mark-ring-goto) \`\\[org-mark-ring-goto]'.")
	    (throw 'found t)))))))


(defun org-ref-ref-help-echo (_win _obj position)
  "Tooltip for context on a ref label.
POSITION is the point under the mouse I think."
  (cdr (assoc (get-text-property position 'org-ref-ref-label) (org-ref-get-labels))))


(defun org-ref-ref-activate (start end path bracketp)
  "Activate a ref link.
The PATH should be a comma-separated list of labels.
Argument START is the start of the link.
Argument END is the end of the link."
  (let ((labels (mapcar 'car (org-ref-get-labels))))
    (goto-char start)
    (cl-loop for label in (split-string path ",") do
	     (search-forward label)
	     ;; store property so we can follow it later.
	     (put-text-property (match-beginning 0)
				(match-end 0)
				'org-ref-ref-label
				label)

	     (unless (member label labels)


	       (put-text-property (match-beginning 0)
				  (match-end 0)
				  'face
				  'font-lock-warning-face)
	       (put-text-property (match-beginning 0)
				  (match-end 0)
				  'help-echo
				  "Label not found")))))


(defun org-ref-ref-export (cmd keyword _desc backend)
  "An export function for ref links.
Argument CMD is the LaTeX command to export to.
Argument KEYWORD is the path of the ref link.
Argument BACKEND is the export backend.
This is meant to be used with `apply-partially'."
  (cond
   ((eq backend 'latex)
    (format "%s{%s}" cmd keyword))))


(defun org-ref-complete-link (refstyle &optional arg)
  "Complete a ref link to an existing label."
  (concat refstyle ":" (completing-read "Label: " (org-ref-get-labels))))


(defun org-ref-label-store-link (reftype)
  "Store a link to a label.  The output will be a ref to that label.
This has several conditional ways to store a link to figures and
tables also. Note it does not currently work with latex labels,
only org labels and names."
  ;; First we have to make sure we are on a label link.
  (let* ((object (and (eq major-mode 'org-mode) (org-element-context)))
	 (stored nil)
	 label)
    (cond
     ;; here literally on a label link.
     ((and
       (equal (org-element-type object) 'link)
       (equal (org-element-property :type object) "label"))
      (setq label (org-element-property :path object))
      (org-store-link-props
       :type reftype
       :link (concat reftype ":" label)))

     ;; here on a file link that probably contains an image, although I don't check that
     ((and
       (equal (org-element-type object) 'link)
       (equal (org-element-property :type object) "file")
       (org-file-image-p (org-element-property :path object)))

      (if (org-element-property :name object)
	  (progn
	    (setq label (org-element-property :name object))
	    (org-store-link-props
	     :type reftype
	     :link (concat reftype ":"label)))
	;; maybe we have a caption to get it from.
	(let* ((parent (org-element-property :parent object))
	       (caption))
	  (when (and parent
		     (equal (org-element-type parent) 'paragraph))
	    (if (org-element-property :name parent)
		;; caption paragraph may have a name which we use if it is there
		(setq label (org-element-property :name parent))
	      ;; else search caption
	      (setq caption (s-join
			     ""
			     (mapcar 'org-no-properties
				     (org-export-get-caption parent))))
	      (when (string-match org-ref-label-re caption)
		(setq label (match-string 1 caption))))

	    (org-store-link-props
	     :type reftype
	     :link (concat reftype ":" label))))))

     ;; here on a paragraph (eg in a caption of an image). it is a paragraph with a caption
     ;; in a caption, with no name, but maybe a label
     ((equal (org-element-type object) 'paragraph)
      (if (org-element-property :name object)
	  (org-store-link-props
	   :type reftype
	   :link (concat reftype ":" (org-element-property :name object)))
	;; See if it is in the caption name
	(let ((caption (s-join "" (mapcar 'org-no-properties
					  (org-export-get-caption object)))))
	  (when (string-match org-ref-label-re caption)
	    (setq label (match-string 1 caption))
	    (org-store-link-props
	     :type reftype
	     :link (concat reftype ":" label))))))

     ;; If you are in a table, we need to be at the beginning to make sure we get the name.
     ;; Note when in a caption it appears you are in a table but org-at-table-p is nil there.
     ((or (equal (org-element-type object) 'table) (org-at-table-p))
      (save-excursion
	(goto-char (org-table-begin))
	(let* ((table (org-element-context))
	       (label (org-element-property :name table))
	       (caption (s-join "" (mapcar 'org-no-properties (org-export-get-caption table)))))
	  (when (null label)
	    ;; maybe there is a label in the caption?
	    (when (string-match org-ref-label-link-re caption)
	      (setq label (match-string 1 caption))))

	  (org-store-link-props
	   :type reftype
	   :link (concat reftype ":" label)))))

     ;; and to #+label: lines
     ((and (equal (org-element-type object) 'paragraph)
           (org-element-property :name object))
      (setq label (org-element-property :name object))
      (org-store-link-props
       :type reftype
       :link (concat reftype ":" label)))

     ;; in a latex environment
     ((equal (org-element-type object) 'latex-environment)
      (let ((value (org-element-property :value object))
	    label)
	(when (string-match "\\\\label{\\(?1:[+a-zA-Z0-9:\\._-]*\\)}" value)
	  (setq label (match-string-no-properties 1 value))
	  (org-store-link-props
	   :type reftype
	   :link (concat reftype ":" label)))))

     ((equal (org-element-type object) 'target)
      (org-store-link-props
       :type reftype
       :link (concat reftype ":" (org-element-property :value object))))

     (t
      nil))))


;; TODO Should there be just one? and make ref the default choice?
(defun org-ref-store-ref () (org-ref-label-store-link "ref"))

(defun org-ref-store-pageref () (org-ref-label-store-link "pageref"))

(defun org-ref-store-nameref () (org-ref-label-store-link "nameref"))

(defun org-ref-store-eqref () (org-ref-label-store-link "eqref"))

(defun org-ref-store-autoref () (org-ref-label-store-link "autoref"))

(defun org-ref-store-cref () (org-ref-label-store-link "cref"))

(defun org-ref-store-Cref () (org-ref-label-store-link "Cref"))

;; ** ref link

(org-link-set-parameters "ref"
			 :store #'org-ref-store-ref
			 :complete (apply-partially #'org-ref-complete-link "ref")
			 :activate-func #'org-ref-ref-activate
			 :follow #'org-ref-ref-follow
			 :export (apply-partially #'org-ref-ref-export "\\ref")
			 :face 'org-ref-ref-face
			 :help-echo #'org-ref-ref-help-echo)


;;** pageref link

(org-link-set-parameters "pageref"
			 :store #'org-ref-store-pageref
			 :complete (apply-partially #'org-ref-complete-link "pageref")
			 :activate-func #'org-ref-ref-activate
			 :follow #'org-ref-ref-follow
			 :export (apply-partially #'org-ref-ref-export "\\pageref")
			 :face 'org-ref-ref-face
			 :complete (lambda (&optional arg) (org-ref-complete-link arg "pageref"))
			 :help-echo #'org-ref-ref-help-echo)


;;** nameref link

(org-link-set-parameters "nameref"
			 :store #'org-ref-store-nameref
			 :complete (apply-partially #'org-ref-complete-link "nameref")
			 :activate-func #'org-ref-ref-activate
			 :follow #'org-ref-ref-follow
			 :export (apply-partially #'org-ref-ref-export "\\nameref")
			 :face 'org-ref-ref-face
			 :help-echo #'org-ref-ref-help-echo)

;;** eqref link

(org-link-set-parameters "eqref"
			 :store #'org-ref-store-eqref
			 :complete (apply-partially #'org-ref-complete-link "eqref")
			 :activate-func #'org-ref-ref-activate
			 :follow #'org-ref-ref-follow
			 :export (apply-partially #'org-ref-ref-export "\\eqref")
			 :face 'org-ref-ref-face
			 :help-echo #'org-ref-ref-help-echo)

;;** autoref link

(org-link-set-parameters "autoref"
			 :store #'org-ref-store-autoref
			 :complete (apply-partially #'org-ref-complete-link "autoref")
			 :activate-func #'org-ref-ref-activate
			 :follow #'org-ref-ref-follow
			 :export (apply-partially #'org-ref-ref-export "\\autoref")
			 :face 'org-ref-ref-face
			 :help-echo #'org-ref-ref-help-echo)

;;** cref link
;; for LaTeX cleveref package:
;; https://www.ctan.org/tex-archive/macros/latex/contrib/cleveref


(org-link-set-parameters "cref"
			 :store #'org-ref-store-cref
			 :complete (apply-partially #'org-ref-complete-link "cref")
			 :activate-func #'org-ref-ref-activate
			 :follow #'org-ref-ref-follow
			 :export (apply-partially #'org-ref-ref-export "\\cref")
			 :face 'org-ref-ref-face
			 :help-echo #'org-ref-ref-help-echo)


(org-link-set-parameters "Cref"
			 :store #'org-ref-store-Cref
			 :complete (apply-partially #'org-ref-complete-link "Cref")
			 :activate-func #'org-ref-ref-activate
			 :follow #'org-ref-ref-follow
			 :export (apply-partially #'org-ref-ref-export "\\Cref")
			 :face 'org-ref-ref-face
			 :help-echo #'org-ref-ref-help-echo)


(defun org-ref-crefrange-export (path _desc backend)
  (pcase backend
    ('latex
     (let ((labels (split-string path ",")))
       (format "\\crefrange{%s}{%s}" (cl-first labels) (cl-second labels))))))


(defun org-ref-Crefrange-export (path _desc backend)
  (pcase backend
    ('latex
     (let ((labels (split-string path ",")))
       (format "\\crefrange{%s}{%s}" (cl-first labels) (cl-second labels))))))


(defun org-ref-crefrange-complete (cmd &optional arg)
  "Completing function for the c/Crefrange links."
  (concat cmd ":"
	  (completing-read "Label 1: " (org-ref-get-labels))
	  ","
	  (completing-read "Label 2: " (org-ref-get-labels))))


(org-link-set-parameters "crefrange"
			 :complete (apply-partially #'org-ref-crefrange-complete "crefrange")
			 :activate-func #'org-ref-ref-activate
			 :follow #'org-ref-ref-follow
			 :export #'org-ref-crefrange-export
			 :face 'org-ref-ref-face
			 :help-echo #'org-ref-ref-help-echo)


(org-link-set-parameters "Crefrange"
			 :complete (apply-partially #'org-ref-crefrange-complete "Crefrange")
			 :activate-func #'org-ref-ref-activate
			 :follow #'org-ref-ref-follow
			 :export #'org-ref-Crefrange-export
			 :face 'org-ref-ref-face
			 :help-echo #'org-ref-ref-help-echo)

;; * Insert link
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


(defun org-ref-insert-ref-link (&optional set-type)
  "Insert a ref link."
  (interactive)
  (let* ((label (completing-read "Label: " (org-ref-get-labels)))
	 (type (if set-type (completing-read "Type: " org-ref-ref-types)
		 (org-ref-infer-ref-type label))))
    (insert (format  "%s:%s" type label))))

(provide 'org-ref-ref-links)

;;; org-ref-ref-links.el ends here
