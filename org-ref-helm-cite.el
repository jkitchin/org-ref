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
(require 'org-ref)

(add-to-list 'load-path
	     (expand-file-name
	      "citeproc"
	      (file-name-directory  (locate-library "org-ref"))))

(require 'org-ref-citeproc)

(defcustom org-ref-notes-directory
  "~/Dropbox/bibliography/helm-cite-notes/"
  "Directory for notes to go in.")


;;* Variables
(defvar orhc-bibtex-cache-data
  '((hashes . ())
    (candidates . ()))
  "Cache data as an alist.
'hashes is a list of cons cells (bibfile . hash)
'candidates is a list of cons cells (bibfile . candidates).
Stored persistently in `orhc-bibtex-cache-file'.")


(defvar orhc-bibtex-cache-file
  "~/.orhc-bibtex-cache"
  "File to store cached data in.")


(defvar org-ref-bibtex-files nil
  "List of bibtex files to get entries from.
This is set internally.")


(defvar orhc-candidate-formats
  '(("article" . "${pdf}${notes}|${=key=}| ${author}, ${title}, ${journal} (${year}). ${keywords}")
    ("book" . "  |${=key=}| ${author}, ${title} (${year}) ${keywords}.")
    ("inbook" . "  |${=key=}| ${author}, ${chapter} in ${title} (${year}) ${keywords}")
    ("techreport" . "  |${=key=}| ${title}, ${institution} (${year}). ${keywords}")
    ("inproceedings" . "  |${=key=}| ${author}, ${title} in ${booktitle} (${year}). ${keywords}")
    ("incollection" . "  |${=key=}| ${author}, ${title} in ${booktitle} (${year}). ${keywords}")
    ("phdthesis" . "  |${=key=}| ${author}, ${title}, ${school} (${year}). Phd thesis. ${keywords}")
    ("mastersthesis" . "  |${=key=}| ${author}, ${title}, ${school} (${year}). MS thesis. ${keywords}")
    ("misc" . "  |${=key=}| ${author}, ${title}")
    ("unpublished" . "  |${=key=}| ${author}, ${title}"))
  "Formats for candidates.
It is an alist of (=type= . s-format-string).")


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

;;* Cache functions
;; when you load, we should check the hashes and files
(defun orhc-load-cache-file ()
  "Load the cache file to set `orhc-bibtex-cache-data'."
  (when (file-exists-p orhc-bibtex-cache-file)
    (with-current-buffer (find-file-noselect orhc-bibtex-cache-file)
      (goto-char (point-min))
      (setq orhc-bibtex-cache-data (read (current-buffer))))
    (when (find-buffer-visiting orhc-bibtex-cache-file)
      (kill-buffer (find-buffer-visiting orhc-bibtex-cache-file)))))


(defun orhc-clear-cache ()
  "Clear the cache and delete `orhc-bibtex-cache-file'."
  (interactive)
  (setq orhc-bibtex-cache-data '((hashes . nil)
			       (candidates . nil)))
  (when (find-buffer-visiting orhc-bibtex-cache-file)
    (kill-buffer (find-buffer-visiting orhc-bibtex-cache-file)))
  (when (file-exists-p orhc-bibtex-cache-file)
    (delete-file orhc-bibtex-cache-file))
  (message "org-ref-helm-cite cache cleared."))


(defun orhc-bibtex-cache-up-to-date ()
  "Return if bibtex caches are up to date.
This means the hash of each bibfile is equal to the one for it in
the cache."
  (-all? 'identity
	 (cl-loop
	  for bibfile in org-ref-bibtex-files
	  collect
	  (string= (progn
		     (with-current-buffer (find-file-noselect bibfile)
		       (secure-hash 'sha256 (current-buffer))))
		   (or (cdr (assoc
			     bibfile
			     (cdr (assoc 'hashes orhc-bibtex-cache-data)))) "")))))


(defun orhc-bibtex-field-formatter (field entry)
  "Format FIELD in a bibtex parsed ENTRY.
A few fields are treated specially, e.g. authors are replaced by
comma-separated list, and I put :: around keywords to make it
easier to search specifically for them."
  (let ((s (replace-regexp-in-string
	    "^{\\|}$" ""
	    (replace-regexp-in-string
	     "\n\\|\t\\|\s+" " "
	     (or (cdr (assoc field entry)) "")))))
    (cond
     ((string= field "author")
      (mapconcat 'identity (s-split " and " s) ", "))
     ((string= field "keywords")
      (if (> (length s) 0)
	  (mapconcat (lambda (keyword)
		       (concat ":" (s-trim keyword) ":"))
		     (s-split "," s)
		     " ")
	""))
     ((string= field "pdf")
      (if (file-exists-p (expand-file-name
			  (concat (cdr (assoc "=key=" entry)) ".pdf")
			  org-ref-pdf-directory))
	  "⌘"
	" "))
     ((string= field "notes")
      (if (file-exists-p (expand-file-name
			  (concat (cdr (assoc "=key=" entry)) ".org")
			  org-ref-notes-directory))
	  "✎"
	" "))
     ;; catch all the other fields and just return them.
     (t
      s))))


(defun orhc-update-bibfile-cache (bibfile)
  "Update cache for BIBFILE.
This generates the candidates for the file. Some of this code is
adapted from `helm-bibtex-parse-bibliography'. This function runs
when called, it resets the cache for the BIBFILE."
  (with-current-buffer (find-file-noselect bibfile)
    (goto-char (point-min))
    (message "Updating cache for %s" bibfile)
    (let ((hash (secure-hash 'sha256 (current-buffer)))
	  (entries
	   (cl-loop
	    for entry-type = (parsebib-find-next-item)
	    while entry-type
	    unless (member-ignore-case entry-type
				       '("preamble" "string" "comment"))
	    collect
	    (let* ((entry (cl-loop for cons-cell in (parsebib-read-entry entry-type)
				;; we remove all properties too. they
				;; cause errors in reading/writing.
				collect
				(cons (substring-no-properties
				       (downcase (car cons-cell)))
				      (substring-no-properties
				       ;; clumsy way to remove surrounding
				       ;; brackets
				       (let ((s (cdr cons-cell)))
					 (if (or (and (s-starts-with? "{" s)
						      (s-ends-with? "}" s))
						 (and (s-starts-with? "\"" s)
						      (s-ends-with? "\"" s)))
					     (substring s 1 -1)
					   s))))))
		   (key (cdr (assoc "=key=" entry))))
	      (cons
	       ;; this is the display string for helm. We try to use the formats
	       ;; in `orhc-candidate-formats', but if there isn't one we just put
	       ;; all the fields in.
	       (s-format
		(or (cdr (assoc (downcase entry-type) orhc-candidate-formats))
		    (format "%s: %s" (cdr (assoc "=key=" entry)) entry))
		'orhc-bibtex-field-formatter
		entry)
	       ;; this is the candidate that is returned, the entry a-list +
	       ;; file and position.
	       (append entry (list (cons "file" (buffer-file-name))
				   (cons "position" (point)))))))))

      ;; Now update the cache variables for hash and entries
      (if (assoc bibfile (cdr (assoc 'candidates orhc-bibtex-cache-data)))
	  (setf (cdr (assoc bibfile
			    (cdr (assoc 'candidates orhc-bibtex-cache-data))))
		entries)
	(cl-pushnew (cons bibfile entries)
		 (cdr (assoc 'candidates orhc-bibtex-cache-data))))
      (if (assoc bibfile (cdr (assoc 'hashes orhc-bibtex-cache-data)))
	  (setf (cdr (assoc
		      bibfile
		      (cdr (assoc 'hashes orhc-bibtex-cache-data))))
		hash)
	(cl-pushnew (cons bibfile hash)
		 (cdr (assoc 'hashes orhc-bibtex-cache-data))))

      ;; And save it to disk for persistent use
      (with-temp-file orhc-bibtex-cache-file
	(print orhc-bibtex-cache-data (current-buffer))))))


(defun orhc-update-bibtex-cache ()
  "Conditionally update cache for all files in `org-ref-bibtex-files'.
Files that have the same hash as in the cache are not updated."
  (cl-loop for bibfile in org-ref-bibtex-files
	unless (string= (progn
			  (with-current-buffer (find-file-noselect bibfile)
			    (secure-hash 'sha256 (current-buffer))))
			(or (cdr
			     (assoc bibfile
				    (cdr
				     (assoc 'hashes orhc-bibtex-cache-data))))
			    ""))
	do
	(orhc-update-bibfile-cache bibfile)))


(defun orhc-helm-cite-describe-cache ()
  "Show what is in the cache."
  (interactive)
  (let ((hash-cache (cdr (assoc 'hashes orhc-bibtex-cache-data)))
	(candidates-cache (cdr (assoc 'candidates orhc-bibtex-cache-data))))
    (message "%s\n\n%s"
	     (mapconcat (lambda (h)
			  (format "%s - %s" (car h) (cdr h)))
			hash-cache "\n")
	     (mapconcat (lambda (c)
			  (format "%s - %s entries" (car c) (length (cdr c))))
			candidates-cache "\n"))))


;; Now load files and update them if needed. We do this when you load the
;; library so they are available later.

(orhc-load-cache-file)
(orhc-update-bibtex-cache)

;;* Helm functions

(defun orhc-bibtex-candidates ()
  "Return the candidates from cache for files listed in `org-ref-bibtex-files'.
Update the cache if necessary."
  ;; this only does something when the cache is out of date
  (orhc-update-bibtex-cache)
  (let ((candidates (cdr (assoc 'candidates orhc-bibtex-cache-data))))
    (apply 'append
	   (cl-loop for bibfile in org-ref-bibtex-files
		 collect (cdr (assoc bibfile candidates))))))


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
      (string> fa1 fa2))))


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
		(string> k1 k2)))))
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


(defun org-ref-helm-candidate-transformer (candidates source)
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
				(lambda (candidate)
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
  (let ((note-file (expand-file-name
		    (concat (cdr (assoc "=key=" candidate)) ".org")
		    org-ref-notes-directory)))
    (if (file-exists-p note-file)
	(setq actions (append actions (list (cons "Open notes"
						  (lambda (x)
						    (find-file note-file))))))
      (setq actions (append actions (list (cons "Create notes"
						(lambda (x)
						  (find-file note-file))))))))

  (setq actions (append
		 actions
		 '(("Add keywords" . org-ref-helm-cite-set-keywords)
		   ("copy to clipboard" . org-ref-helm-cite-copy-entries)
		   ("email" . org-ref-helm-cite-email-entries)
		   ("Insert formatted entries" . orhc-insert-formatted-citations)
		   ("Copy formatted entry" . orhc-copy-formatted-citations))))

  ;; this is where we could add WOK/scopus functions
  actions)


(defun org-ref-helm-cite-insert-citation (candidate)
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
	 ((looking-back ":")
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
	 ((looking-back ",")
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
  (setq org-ref-bibtex-files (org-ref-find-bibliography))
  ;; save major-mode we came from so we can do context specific things.
  (setq org-ref-helm-cite-from major-mode)
  (message "initialized."))


(defun org-ref-helm-cite-open-entry (entry)
  "Open the selected bibtex entry in its file."
  (find-file (cdr (assoc "file" entry)))
  (goto-char (cdr (assoc "position" entry)))
  (bibtex-beginning-of-entry))


(defun org-ref-helm-cite-copy-entries (candidate)
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


(defun org-ref-helm-cite-email-entries (candidate)
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

(defun org-ref-helm-cite-set-keywords (candidate)
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
  (let* ((spacing (or (cdr (assoc 'spacing bibliography-style)) 1))
	 (label-func (cdr (assoc 'label bibliography-style)))
	 (label-prefix (cdr (assoc 'label-prefix bibliography-style)))
	 (label-suffix (cdr (assoc 'label-suffix bibliography-style)))
	 (justification (cdr (assoc 'justification bibliography-style)))
	 (hanging-indent (cdr (assoc 'hanging-indent bibliography-style)))
	 (header (cdr (assoc 'header bibliography-style)))
	 (adaptive-fill-function '(lambda () "    "))
	 (indent-tabs-mode nil)
	 bibliography-string
	 (entry-type (downcase
		      (cdr (assoc "=type=" (cdr entry)))))
	 (key (cdr (assoc "=key=" (cdr entry))))
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


(defun orhc-formatted-citations (candidate)
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
