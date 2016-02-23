;;; org-ref-helm-cite.el --- Helm interface to insert citations from bibtex files for org-ref  -*- lexical-binding: t; -*-

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

;;; Commentary: This is a competitor of `helm-bibtex'.
;; The main difference is the format of the candidates, which are full citations
;; in this package, and multiline. This package also makes the candidates
;; sortable in different ways, and provides different, context specific actions
;; depending on what buffer you call `org-ref-helm-cite' from, and depending
;; on the properties of the selected candidate.
;;
;; Another significant feature is persistent caching of bibtex files to make
;; startup fast.

;;

;;; Code:
(defvar-local bibliography-style nil)

(require 'org-ref-helm)
(require 'org-ref-bibtex)



(defun org-ref-helm-cite-completion ()
  "Use helm and org-ref for completion."
  (interactive)
  (setq org-ref-insert-link-function 'org-ref-insert-link
	org-ref-insert-cite-function 'org-ref-helm-cite
	org-ref-insert-label-function 'org-ref-helm-insert-label-link
	org-ref-insert-ref-function 'org-ref-helm-insert-ref-link
	org-ref-cite-onclick-function 'org-ref-cite-click-helm)

  ;; define key for inserting citations
  (define-key org-mode-map
    (kbd org-ref-insert-cite-key)
    org-ref-insert-link-function))

(org-ref-helm-cite-completion)

(add-to-list 'load-path
	     (expand-file-name
	      "citeproc"
	      (file-name-directory  (locate-library "org-ref"))))

(load-file (expand-file-name
	    "org-ref-citeproc.el"
	    (expand-file-name
	     "citeproc"
	     (file-name-directory  (locate-library "org-ref")))))

;;* Variables
(defvar org-ref-helm-cite-from nil
  "Variable to store the mode `org-ref-helm-cite' was called
  from. This is used to provide some context specific actions.")


(defvar org-ref-helm-cite-help-message
  "* Org-ref helm bibtex.
M-<down> allows you to sort the entries by year or first author
last name.")


(defvar org-ref-helm-cite-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-<down>") 'org-ref-helm-cite-sort)
    map))


(defvar orhc-sort-fn nil
  "Function for sorting the helm entries.")

;;* Helm functions

(defun orhc-helm-cite-sort-alphabetical-a (c1 c2)
  "Sort entries by first author last name from a to z."
  (let* ((a1 (cdr c1))
	 (au1 (cdr (assoc "author" a1)))
	 (a2 (cdr c2))
	 (au2 (cdr (assoc "author" a2)))
	 fa1 fa2)
    (if (or (not (stringp au1))
	    (not (stringp au2)))
	nil
      ;; we need to parse names
      (setq fa1 (car (s-split " and " au1))
	    fa2 (car (s-split " and " au2)))

      (if (string-match "," fa1)
	  ;; last, first
	  (setq fa1 (car (s-split "," fa1)))
	;; first last, ignore von names and jr
	(setq fa1 (car (last (s-split " " fa1)))))

      (if (string-match "," fa2)
	  ;; last, first
	  (setq fa2 (car (s-split "," fa2)))
	;; first last, ignore von names, and jr
	(setq fa2 (car (last (s-split " " fa2)))))
      (string< fa1 fa2))))


(defun orhc-helm-cite-sort-alphabetical-z (c1 c2)
    "Sort entries by first author last name from z to a."
  (let* ((a1 (cdr c1))
	 (au1 (cdr (assoc "author" a1)))
	 (a2 (cdr c2))
	 (au2 (cdr (assoc "author" a2)))
	 fa1 fa2)
    (if (or (not (stringp au1))
	    (not (stringp au2)))
	nil
      ;; we need to parse names to get lastname
      (setq fa1 (car (s-split " and " au1))
	    fa2 (car (s-split " and " au2)))

      (if (string-match "," fa1)
	  ;; last, first
	  (setq fa1 (car (s-split "," fa1)))
	;; first last, ignore von names and jr
	(setq fa1 (car (last (s-split " " fa1)))))

      (if (string-match "," fa2)
	  ;; last, first
	  (setq fa2 (car (s-split "," fa2)))
	;; first last, ignore von names, and jr
	(setq fa2 (car (last (s-split " " fa2)))))
      (string< fa2 fa1))))


(defun org-ref-helm-cite-sort ()
  "Sort interface for `org-ref-helm-cite'."
  (interactive)
  (let ((action (read-char "year↓ (y) year↑ (Y)
1st author↓ (a) 1st author↑ (z)
key↓ (k) key↑ (K): ")))
    (cond
     ;; sort on year
     ((eq action ?y)
      (setq orhc-sort-fn
	    (lambda (c1 c2)
	      (let* ((a1 (cdr c1))
		     (y1 (cdr (assoc "year" a1)))
		     (a2 (cdr c2))
		     (y2 (cdr (assoc "year" a2))))
		(if (or (null y1) (null y2))
		    nil
		  (> (string-to-number  y1) (string-to-number y2)))))))
     ((eq action ?Y)
      (setq orhc-sort-fn
	    (lambda (c1 c2)
	      (let* ((a1 (cdr c1))
		     (y1 (cdr (assoc "year" a1)))
		     (a2 (cdr c2))
		     (y2 (cdr (assoc "year" a2))))
		(if (or (null y1) (null y2))
		    nil
		  (< (string-to-number  y1) (string-to-number y2)))))))
     ;; sort on key
     ((eq action ?k)
      (setq orhc-sort-fn
	    (lambda (c1 c2)
	      (let* ((a1 (cdr c1))
		     (k1 (cdr (assoc "=key=" a1)))
		     (a2 (cdr c2))
		     (k2 (cdr (assoc "=key=" a2))))
		(string< k2 k1)))))
     ((eq action ?K)
      (setq orhc-sort-fn
	    (lambda (c1 c2)
	      (let* ((a1 (cdr c1))
		     (k1 (cdr (assoc "=key=" a1)))
		     (a2 (cdr c2))
		     (k2 (cdr (assoc "=key=" a2))))
		(string< k1 k2)))))
     ;; sort on first author last name
     ((eq action ?a)
      (setq orhc-sort-fn #'orhc-helm-cite-sort-alphabetical-a))
     ((eq action ?z)
      (setq orhc-sort-fn #'orhc-helm-cite-sort-alphabetical-z))
     (t (setq orhc-sort-fn nil)))
    (helm-update)
    (setq orhc-sort-fn nil)))


(defun org-ref-helm-candidate-transformer (candidates _source)
  "Transform CANDIDATES, sorting if needed.
SOURCE is ignored, but required."
  (if orhc-sort-fn
      (-sort orhc-sort-fn candidates)
    candidates))


(defun org-ref-helm-cite-action-transformer (actions candidate)
  "Compute ACTIONS for CANDIDATE."
  ;; Check for pdf and add open or get action.
  (setq actions (append
		 actions
		 '(("insert citation(s)" . org-ref-helm-cite-insert-citation)
		   ("show entry" . org-ref-helm-cite-open-entry))))

  (let ((pdf (expand-file-name
	      (concat (cdr (assoc "=key=" candidate)) ".pdf")
	      org-ref-pdf-directory)))
    (if (file-exists-p pdf)
	(setq actions (append actions
			      (list
			       (cons
				(format "Open %s" pdf)
				(lambda (_candidate)
				  (org-open-file pdf))))))
      (when (assoc "doi" candidate)
	(setq actions
	      (append actions
		      (list
		       (cons
			(format "Get PDF")
			(lambda (candidate)
			  (save-window-excursion
			    (find-file (cdr (assoc "file" candidate)))
			    (goto-char (cdr (assoc "position" candidate)))
			    (doi-utils-get-bibtex-entry-pdf))))))))))

  ;; check for url/doi
  (when (assoc "url" candidate)
    (setq actions (append actions
			  (list
			   (cons (format "Open %s"
					 (cdr (assoc "url" candidate)))
				 (lambda (x)
				   (browse-url (cdr (assoc "url" x)))))))))

  (when (assoc "doi" candidate)
    (setq actions (append actions
			  (list
			   (cons
			    (format "Open doi (%s)"
				    (cdr (assoc "doi" candidate)))
			    (lambda (x)
			      (browse-url
			       (format "http://dx.doi.org/%s"
				       (cdr (assoc "doi" x))))))))))

  ;; Notes, open or create.
  (when org-ref-notes-directory
    (let ((note-file (expand-file-name
		      (concat (cdr (assoc "=key=" candidate)) ".org")
		      org-ref-bibliography-notes)))
      (if (file-exists-p note-file)
	  (setq actions (append actions (list (cons "Open notes"
						    (lambda (_x)
						      (find-file note-file))))))
	(setq actions (append actions (list (cons "Create notes"
						  (lambda (_x)
						    (find-file note-file)))))))))

  (when org-ref-bibliography-notes
    (setq actions (append actions (list (cons "Open/create notes"
					      (lambda (x)
						(funcall org-ref-notes-function
							 (cdr (assoc "=key=" x)))))))))

  (setq actions (append
		 actions
		 '(("Add keywords" . org-ref-helm-cite-set-keywords)
		   ("copy to clipboard" . org-ref-helm-cite-copy-entries)
		   ("email" . org-ref-helm-cite-email-entries)
		   ("Insert formatted entries" . orhc-insert-formatted-citations)
		   ("Copy formatted entry" . orhc-copy-formatted-citations))))

  ;; this is where we could add WOK/scopus functions
  actions)


(defun org-ref-helm-cite-insert-citation (_candidate)
  "Insert selected CANDIDATE as cite link.
This is an action for helm, and it actually works on
`helm-marked-candidates'. Append KEYS if you are on a link.

In the `org-ref-helm-cite' buffer, \\[universal-argument] will give you
a helm menu to select a new link type for the selected entries.

A double \\[universal-argument] \\[universal-argument] will
change the key at point to the selected keys."
  (let* ((keys (cl-loop for entry in (helm-marked-candidates)
			collect (cdr (assoc "=key=" entry))))
	 (object (org-element-context))
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
       ;; no prefix. insert or append keys
       ((equal helm-current-prefix-arg nil)
	(cond
	 ;; point after :
	 ((looking-back ":" (- (point) 2))
	  (insert (concat (mapconcat 'identity keys ",") ",")))
	 ;; point on :
	 ((looking-at ":")
	  (forward-char)
	  (insert (concat (mapconcat 'identity keys ",") ",")))
	 ;; point on the cite type
	 ((-contains? org-ref-cite-types (thing-at-point 'word))
	  (re-search-forward ":")
	  (insert (concat (mapconcat 'identity keys ",") ",")))
	 ;; after ,
	 ((looking-back "," (- (point) 2))
	  (insert (concat (mapconcat 'identity keys ",") ",")))
	 ;; on comma
	 ((looking-at ",")
	  (forward-char)
	  (insert (concat (mapconcat 'identity keys ",") ",")))
	 ;; somewhere in the middle or end
	 (t
	  ;; goto next comma or end
	  (re-search-forward
	   ","
	   (org-element-property :end object) t)
	  (skip-chars-backward " ")
	  (insert (mapconcat 'identity keys ","))
	  (unless (looking-at ",") (insert ",")))))
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
               (s-join "," keys)))))))


(defun org-ref-helm-cite-init ()
  "Initializes the source, setting bibtex files from the
originating buffer, and mode of originating buffer."
  (org-ref-save-all-bibtex-buffers)
  (setq org-ref-bibtex-files (org-ref-find-bibliography))
  ;; save major-mode we came from so we can do context specific things.
  (setq org-ref-helm-cite-from major-mode)
  (message "initialized."))


(defun org-ref-helm-cite-open-entry (entry)
  "Open the selected bibtex entry in its file."
  (find-file (cdr (assoc "bibfile" entry)))
  (goto-char (cdr (assoc "position" entry)))
  (bibtex-beginning-of-entry))


(defun org-ref-helm-cite-copy-entries (_candidate)
  "Copy selected bibtex entries to the clipboard."
  (with-temp-buffer
    (cl-loop for entry in (helm-marked-candidates)
	  do
	  (save-window-excursion
	    (org-ref-helm-cite-open-entry entry)
	    (bibtex-copy-entry-as-kill))
	  (bibtex-yank)
	  (insert "\n"))
    (kill-region (point-min) (point-max))))


(defun org-ref-helm-cite-email-entries (_candidate)
  "Insert selected entries and attach pdf files to an email.
Create email unless called from an email."
  (unless (or (eq org-ref-helm-cite-from 'message-mode)
	      (eq org-ref-helm-cite-from 'mu4e-compose-mode))
    (compose-mail))
  (cl-loop for entry in (helm-marked-candidates)
	do
	(save-window-excursion
	  (org-ref-helm-cite-open-entry entry)
	  (bibtex-copy-entry-as-kill))
	(message-goto-body)
	(insert (pop bibtex-entry-kill-ring))
	(insert "\n")
	if (file-exists-p (expand-file-name
			   (concat (cdr (assoc "=key=" entry)) ".pdf")
			   org-ref-pdf-directory))
	do
	(mml-attach-file (expand-file-name
			  (concat (cdr (assoc "=key=" entry)) ".pdf")
			  org-ref-pdf-directory)))
  (message-goto-to))

(defun org-ref-helm-cite-set-keywords (_candidate)
  "Prompt for keywords, and put them on the selected entries."
  (let ((keywords (read-string "Keyword(s) comma-separated: " ))
	entry-keywords)
    (cl-loop for entry in (helm-marked-candidates)
	  do
	  (save-window-excursion
	    (org-ref-helm-cite-open-entry entry)
	    (setq entry-keywords (bibtex-autokey-get-field "keywords"))
	    (bibtex-set-field
	     "keywords"
	     (if entry-keywords
		 (concat entry-keywords ", " keywords)
	       keywords))))))

;;** Helm sources
(defvar  orhc-multiline t
  "Make helm-source multiline if non-nil.
This adds a small separator between the candidates which is a
little more readable.")

(defvar org-ref-helm-cite-source
  (helm-build-sync-source "org-ref Bibtex"
    :init #'org-ref-helm-cite-init
    :candidates #'orhc-bibtex-candidates
    :keymap 'org-ref-helm-cite-map
    :multiline orhc-multiline
    :help-message 'org-ref-helm-cite-help-message
    :filtered-candidate-transformer 'org-ref-helm-candidate-transformer
    :action-transformer 'org-ref-helm-cite-action-transformer
    :action '()))

;; Fallback sources
;; The candidates here are functions that work on `helm-pattern'.
(defvar org-ref-helm-cite-fallback-source
  nil
  "Helm fallback source.")


(setq org-ref-helm-cite-fallback-source
      (helm-build-sync-source "org-ref bibtex Fallbacks"
	:candidates '(("Google" . (lambda ()
				    (browse-url
				     (format "http://www.google.com/search?q=%s"
					     (url-hexify-string helm-pattern)))))
		      ("Google Scholar" . (lambda ()
					    (browse-url
					     (format "http://scholar.google.com/scholar?q=%s"
						     (url-hexify-string helm-pattern)))))
		      ("Crossref" . (lambda ()
				      (browse-url
				       (format
					"http://search.crossref.org/?q=%s"
					(url-hexify-string helm-pattern)))))
		      ("Pubmed" . (lambda ()
				    (browse-url
				     (format
				      "http://www.ncbi.nlm.nih.gov/pubmed/?term=%s"
				      (url-hexify-string helm-pattern)))))
		      ("Arxiv" . (lambda ()
				   (browse-url
				    (format
				     "http://arxiv.org/find/all/1/all:+AND+%s/0/1/0/all/0/1"
				     (url-hexify-string helm-pattern)))))
		      ("WebOfKnowledge" . (lambda ()
					    (browse-url
					     (format
					      "http://gateway.webofknowledge.com/gateway/Gateway.cgi?topic=%s&GWVersion=2&SrcApp=WEB&SrcAuth=HSB&DestApp=UA&DestLinkType=GeneralSearchSummary"
					      (url-hexify-string helm-pattern)))))
		      ("Scopus" . (lambda ()
				    (browse-url
				     (format
				      "http://www.scopus.com//search/submit/basic.url?field1=TITLE-ABS-KEY&searchterm1=%s"
				      (url-hexify-string helm-pattern)))))
		      )
	;; This keeps all the fallback candidates available, by tricking it into
	;; thinking every candidate is a match.
	:match (lambda (_candidate) t)
	:action (lambda (candidate) (funcall candidate))))


(defun org-ref-helm-cite ()
  "Helm interface to bibtex files for `org-ref'."
  (interactive)
  (helm :sources '(org-ref-helm-cite-source
		   org-ref-helm-cite-fallback-source)))


(defalias 'orhc 'org-ref-helm-cite)


;;* Formatted citations

(defun orhc-formatted-citation (entry)
  "Get a formatted string for entry."
  (let* ((adaptive-fill-function '(lambda () "    "))
	 (indent-tabs-mode nil)
	 (entry-type (downcase
		      (cdr (assoc "=type=" (cdr entry)))))
	 (entry-styles (cdr (assoc 'entries bibliography-style)))
	 (entry-fields
	  (progn
	    (if (cdr (assoc (intern entry-type) entry-styles))
		(cdr (assoc (intern entry-type) entry-styles))
	      (warn "%s not found. Using default." entry-type)
	      (cdr (assoc 't entry-styles)))))
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
		 entry-fields)))

    ;; this is the entry. We do this in a buffer to make it
    ;; easy to indent, fill, etc...
    (with-temp-buffer
      (insert (mapconcat (lambda (field-func)
			   (funcall field-func entry))
			 funcs
			 ""))
      (buffer-string))))


(defun orhc-formatted-citations (_candidate)
  "Return string containing formatted citations for entries in
`helm-marked-candidates'."
  (load-library
   (ido-completing-read "Style: " '("unsrt" "author-year") nil nil "unsrt"))

  (with-temp-buffer
    (cl-loop for i from 1 to (length (helm-marked-candidates))
	  for entry in (helm-marked-candidates)
	  do
	  (insert (format "%s. %s\n\n" i (orhc-formatted-citation entry))))

    (buffer-string)))


(defun orhc-insert-formatted-citations (candidate)
  "Insert formatted citations at point for selected entries."
  (insert (orhc-formatted-citations candidate)))


(defun orhc-copy-formatted-citations (candidate)
  "Copy formatted citations to clipboard for selected entries."
  (with-temp-buffer
    (orhc-insert-formatted-citations candidate)
    (kill-ring-save (point-min) (point-max))))

(provide 'org-ref-helm-cite)
;;; org-ref-helm-cite.el ends here
