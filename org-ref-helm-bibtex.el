;;; org-ref-helm-bibtex.el --- Customization of helm-bibtex for org-ref  -*- lexical-binding: t; -*-

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
;; This file defines the completion engine for org-ref using `helm-bibtex'.


(declare-function 'org-ref-find-bibliography "org-ref-core.el")
(declare-function 'org-ref-get-bibtex-key-and-file "org-ref-core.el")

(defvar org-ref-get-pdf-filename-function)
(defvar org-ref-default-citation-link)
(defvar org-ref-cite-types)
(defvar org-ref-insert-link-function)
(defvar org-ref-insert-cite-function)
(defvar org-ref-insert-label-function)
(defvar org-ref-insert-ref-function)
(defvar org-ref-cite-onclick-function)
(defvar org-ref-insert-cite-key)

;;; Code:
(require 'helm-config)
(require 'helm)
(require 'helm-bibtex)
(require 'helm-utils)
(require 'org-ref-helm)
(require 'async)
(require 'package)

;;;###autoload
(defun org-ref-bibtex-completion-completion ()
  "Use helm and ‘helm-bibtex’ for completion."
  (interactive)
  ;; Define core functions for org-ref
  (setq org-ref-insert-link-function 'org-ref-helm-insert-cite-link
	org-ref-insert-cite-function 'org-ref-helm-insert-cite-link
	org-ref-insert-label-function 'org-ref-helm-insert-label-link
	org-ref-insert-ref-function 'org-ref-helm-insert-ref-link
	org-ref-cite-onclick-function 'org-ref-cite-click-helm))

(org-ref-bibtex-completion-completion)
(define-key org-mode-map
  (kbd org-ref-insert-cite-key)
  org-ref-insert-link-function)

(defcustom org-ref-bibtex-completion-actions
  '(("Insert citation" . helm-bibtex-insert-citation)
    ("Open PDF, URL or DOI" . helm-bibtex-open-any)
    ("Open URL or DOI in browser" . helm-bibtex-open-url-or-doi)
    ("Insert reference" . helm-bibtex-insert-reference)
    ("Insert BibTeX key" . helm-bibtex-insert-key)
    ("Insert BibTeX entry" . helm-bibtex-insert-bibtex)
    ("Insert formatted citation(s)" . (lambda (_)
					(insert
					 (mapconcat 'identity
						    (cl-loop for key in (helm-marked-candidates)
							     collect (org-ref-format-entry key))
						    "\n\n"))))
    ("Attach PDF to email" . helm-bibtex-add-PDF-attachment)
    ("Edit notes" . helm-bibtex-edit-notes)
    ("Show entry" . helm-bibtex-show-entry)
    ("Add keywords to entries" . org-ref-helm-tag-entries)
    ("Copy entry to clipboard" . bibtex-completion-copy-candidate)
    ("Add PDF to library" . helm-bibtex-add-pdf-to-library))
  "Cons cells of string and function to set the actions of `helm-bibtex' to.
The car of cons cell is the string describing the function.
The cdr of the the cons cell is the function to use."
  :type '(alist :key-type (string) :value-type (function))
  :group 'org-ref)


(cl-loop for i from 0 to (length org-ref-bibtex-completion-actions)
	 for ccell in org-ref-bibtex-completion-actions
	 do
	 (helm-delete-action-from-source (car ccell) helm-source-bibtex)
	 (helm-add-action-to-source
	  (car ccell)
	  (cdr ccell)
	  helm-source-bibtex))


(defcustom org-ref-bibtex-completion-format-org
  'org-ref-bibtex-completion-format-org
  "Function for how `helm-bibtex' inserts citations."
  :type 'function
  :group 'org-ref)


(setf (cdr (assoc 'org-mode bibtex-completion-format-citation-functions))
      org-ref-bibtex-completion-format-org)


(setq org-ref-insert-cite-function 'org-ref-helm-insert-cite-link
      org-ref-cite-onclick-function 'org-ref-cite-click-helm)


(defcustom org-ref-bibtex-completion-add-keywords-field t
  "Whether to add the `keywords' field to bibtex-completion."
  :group 'org-ref
  :type 'boolean)


;;* Helm bibtex setup.
(when org-ref-bibtex-completion-add-keywords-field
  (unless (or (member 'keywords bibtex-completion-additional-search-fields)
              (member "keywords" bibtex-completion-additional-search-fields))
    ;; Both symbol and string values are accepted, but b-c-a-s-f's
    ;; Custom :type specifies string.
    (push "keywords" bibtex-completion-additional-search-fields))
  (let ((display-format
         (alist-get t bibtex-completion-display-formats)))
    (unless (string-match-p "{keywords:"
                            display-format)
      (setf (alist-get t bibtex-completion-display-formats)
            (concat display-format " ${keywords:31}")))))

(defun bibtex-completion-copy-candidate (_candidate)
  "Copy the selected bibtex entries to the clipboard.
Used as a new action in `helm-bibtex'.
CANDIDATE is ignored."
  (with-temp-buffer
    (bibtex-mode)
    (mapc #'insert-file-contents
	  (-flatten (list bibtex-completion-bibliography)))

    (let ((entries '()))
      (cl-loop for bibtex-key in (helm-marked-candidates)
	       do
	       (goto-char (point-min))
	       (re-search-forward (concat "^@\\(" parsebib--bibtex-identifier
					  "\\)[[:space:]]*[\(\{][[:space:]]*"
					  (regexp-quote bibtex-key)
					  "[[:space:]]*,"))

	       (cl-pushnew  (buffer-substring
			     (bibtex-beginning-of-entry)
			     (bibtex-end-of-entry))
			    entries))

      (with-temp-buffer
	(dolist (entry entries)
	  (insert (format "%s\n\n" entry)))
	(kill-new (buffer-string))))))



(defun org-ref-helm-tag-entries (_candidates)
  "Set tags on selected bibtex entries from `helm-bibtex'.
User is prompted for tags.  This function is called from `helm-bibtex'.
Argument CANDIDATES helm candidates."
  (message "")
  (let* ((keys (helm-marked-candidates))
	 (entry (bibtex-completion-get-entry (car keys)))
	 (field (cdr (assoc-string "keywords" entry)))
	 (value (when field (replace-regexp-in-string "^{\\|}$" "" field)))
	 (keywords (read-string "Keywords (comma separated): "
				(when (and value (not (equal "" value)))
				  (concat value ", ")))))
    (cl-loop for key in keys
	     do
	     (save-window-excursion
	       (bibtex-completion-show-entry (list key))
	       ;; delete keyword field if empty
	       (if (equal "" keywords)
		   (save-restriction
		     (bibtex-narrow-to-entry)
		     (goto-char (car (cdr (bibtex-search-forward-field "keywords" t))))
		     (bibtex-kill-field))
		 (bibtex-set-field
		  "keywords"
		  (concat
		   (if (listp keywords)
		       (if (string-match value keywords)
		  	   (and (replace-match "")
		  		(mapconcat 'identity keywords ", "))
		  	 (mapconcat 'identity keywords ", "))
		     ;; remove trailing comma
		     (replace-regexp-in-string ", $" "" keywords)))))
	       (save-buffer)))))


(defun org-ref-bibtex-completion-format-org (keys)
  "Insert selected KEYS as cite link.
Append KEYS if you are on a link.

Technically, this function should return a string that is
inserted by helm.  This function does the insertion and gives helm
an empty string to insert.  This lets us handle appending to a
link properly.

In the `helm-bibtex' buffer, \\[universal-argument] will give you a helm menu to
select a new link type for the selected entries.

A double \\[universal-argument] \\[universal-argument] will
change the key at point to the selected keys."
  (let* ((object (org-element-context))
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
	   (org-element-property :end object) 'mv)
	  (skip-chars-backward " ")
	  (skip-chars-backward "]")
	  (unless (looking-at ",") (insert ","))
	  (insert (mapconcat 'identity keys ",")))))
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
      (insert
       (concat
	(when org-ref-prefer-bracket-links "[[")
	(if (equal helm-current-prefix-arg '(4))
	    (helm :sources `((name . "link types")
			     (candidates . ,org-ref-cite-types)
			     (action . (lambda (x) x))))
	  org-ref-default-citation-link)
	":"
	(s-join "," keys)
	(when org-ref-prefer-bracket-links "]]"))))))
  ;; return empty string for helm
  "")


(defun org-ref-format-citation (keys)
  "Formatter for org-ref citation commands.
Prompt for the command and additional arguments if the commands can
take any. If point is inside a citation link, append KEYS. Otherwise
prompt for pre/post text. Prompts can also be switched off by setting
the variable `bibtex-completion-cite-prompt-for-optional-arguments' to
nil. To enable this formatter, add it to
`bibtex-completion-format-citation-functions'. For example:

\(setf (cdr (assoc 'org-mode bibtex-completion-format-citation-functions)) 'org-ref-format-citation)

Note also that pre text is preceded by a double colon, for example:

\[[cite:key][See::Chapter 1]], which exports to:

\\cite[See][Chapter 1]{key}."
  ;; Check if point is inside a cite link
  (let ((link (org-element-context))
	end path)
    (if (-contains? org-ref-cite-types (org-element-property :type link))
	(progn
	  (setq end (org-element-property :end link)
		path (org-element-property :path link))
	  (goto-char end)
	  (skip-chars-backward " ")
	  ;; Check if link has pre/post text
	  (if (looking-back "\]" (line-beginning-position))
	      (progn
		(re-search-backward path nil t)
		(re-search-forward "\]" nil t)
		(backward-char 1)
		(format ",%s" (s-join "," keys))))
	  (format ",%s" (s-join "," keys)))
      (let* ((initial (when bibtex-completion-cite-default-as-initial-input bibtex-completion-cite-default-command))
	     (default
	       (unless bibtex-completion-cite-default-as-initial-input bibtex-completion-cite-default-command))
	     (default-info
	       (if default (format " (default \"%s\")" default) ""))
	     (cite-command
	      (completing-read (format "Cite command%s: " default-info)
			       bibtex-completion-cite-commands nil nil initial
			       'bibtex-completion-cite-command-history default nil)))
	(if (member cite-command '("nocite" "supercite"))  ; These don't want arguments.
	    (format "%s:%s" cite-command (s-join "," keys))
	  (let ((text (if bibtex-completion-cite-prompt-for-optional-arguments
			  (read-from-minibuffer "Pre/post text: ")
			"")))
	    (if (string= "" text)
		(format "%s:%s" cite-command (s-join "," keys))
	      (format "[[%s:%s][%s]]" cite-command (s-join "," keys) text))))))))


(defvar bibtex-completion-cached-candidates)
(defvar bibtex-completion-bibliography-hash)

;;;###autoload
(defun org-ref-helm-load-completions-async ()
  "Load the bibtex files into helm sources asynchronously.
For large bibtex files, the initial call to ‘org-ref-helm-insert-cite-link’
can take a long time to load the completion sources.  This function loads
the completion sources in the background so the initial call to ‘org-ref-helm-insert-cite-link’ is much faster."
  (interactive)
  (async-start
   `(lambda (&optional formatter)
      (require 'package)
      (package-initialize)
      (require 'helm-bibtex)
      ,(async-inject-variables "bibtex-compl.*")

      (with-temp-buffer
	(mapc #'insert-file-contents
	      (-flatten (list bibtex-completion-bibliography)))
	;; Check hash of bibliography and reparse if necessary:
	(let ((bibliography-hash (secure-hash 'sha256 (current-buffer))))
	  (unless (and bibtex-completion-cached-candidates
		       (string= bibtex-completion-bibliography-hash bibliography-hash))
	    (message "Loading bibliography ...")
	    (let* ((entries (bibtex-completion-parse-bibliography))
		   (entries (bibtex-completion-resolve-crossrefs entries))
		   (entries (bibtex-completion-prepare-entries entries))
		   (entries (nreverse entries))
		   (entries
		    (--map (cons (bibtex-completion-clean-string
				  (s-join " " (-map #'cdr it))) it)
			   entries)))
	      (setq bibtex-completion-cached-candidates
		    (if (functionp formatter)
			(funcall formatter entries)
		      entries)))
	    (setq bibtex-completion-bibliography-hash bibliography-hash))
	  (cons bibliography-hash bibtex-completion-cached-candidates))))
   (lambda (result)
     (setq bibtex-completion-cached-candidates (cdr result))
     (setq bibtex-completion-bibliography-hash (car result))
     (message "Finished loading org-ref completions"))))




;;;###autoload
(defun org-ref-helm-insert-cite-link (&optional arg)
  "Insert a citation link with `helm-bibtex'.
With one prefix ARG, insert a ref link.
With two prefix ARGs, insert a label link."
  (interactive "P")
  ;; save all bibtex buffers so we get the most up-to-date selection. I find
  ;; that I often edit a bibliography and forget to save it, so the newest entry
  ;; does not show in helm-bibtex.
  (org-ref-save-all-bibtex-buffers)
  (cond
   ((equal arg nil)
    (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
      (helm-bibtex)))
   ((equal arg '(4))
    (org-ref-helm-insert-ref-link))
   ((equal arg '(16))
    (org-ref-helm-insert-label-link))))


;; add our own fallback entries where we want them. These appear in reverse
;; order of adding in the menu
(setq bibtex-completion-fallback-options
      (-insert-at 1 '("Crossref" . "http://search.crossref.org/?q=%s") bibtex-completion-fallback-options))


(setq bibtex-completion-fallback-options
      (-insert-at
       1
       '("Scopus" . "http://www.scopus.com/scopus/search/submit/xadvanced.url?searchfield=TITLE-ABS-KEY(%s)")
       bibtex-completion-fallback-options))


(setq bibtex-completion-fallback-options
      (-insert-at 1 '("WOS" . "http://gateway.webofknowledge.com/gateway/Gateway.cgi?topic=%s&GWVersion=2&SrcApp=WEB&SrcAuth=HSB&DestApp=UA&DestLinkType=GeneralSearchSummary") bibtex-completion-fallback-options))

(defun org-ref-cite-candidates ()
  "Generate the list of possible candidates for click actions on a cite link.
Checks for pdf and doi, and add appropriate functions."
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         ;; bibfile could be nil for certain type of entry
         ;; in that case we would just use current value of bibtex-completion-bibliography
         (bibfile (cdr results))
         (bibtex-completion-bibliography (if bibfile (list bibfile) bibtex-completion-bibliography))
         (entry (bibtex-completion-get-entry key))
         (pdf-file (funcall org-ref-get-pdf-filename-function key))
         (pdf-bibtex-completion (car (bibtex-completion-find-pdf key)))
         (notes-p (cdr (assoc "=has-note=" entry)))
         (url (if bibfile (save-excursion
			    (with-temp-buffer
			      (insert-file-contents bibfile)
			      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
			      (bibtex-search-entry key)
			      (bibtex-autokey-get-field "url")))
		""))
         (doi (if bibfile (save-excursion
			    (with-temp-buffer
			      (insert-file-contents bibfile)
			      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
			      (bibtex-search-entry key)
			      ;; I like this better than bibtex-url which does not always find
			      ;; the urls
			      (bibtex-autokey-get-field "doi")))
		""))
         (candidates `(("Quit" . org-ref-citation-at-point)
                       ("Open bibtex entry" . org-ref-open-citation-at-point))))
    ;; for some reason, when there is no doi or url, they are returned as "". I
    ;; prefer nil so we correct this here.
    (when (string= doi "") (setq doi nil))
    (when (string= url "") (setq url nil))

    ;; Conditional pdf functions
    ;; try with org-ref first
    (cond ((file-exists-p pdf-file)
    	   (cl-pushnew
    	    '("Open pdf" . (lambda ()
    			     (funcall org-ref-open-pdf-function)))
    	    candidates))

	  ;; try with bibtex-completion
    	  (pdf-bibtex-completion
    	   (cl-pushnew
    	    '("Open pdf" . (lambda ()
    			     (funcall org-ref-open-pdf-function)))
    	    candidates))

	  ;; try with doi
    	  (t
    	   (cl-pushnew
    	    '("Try to get pdf" . (lambda ()
    				   (save-window-excursion
    				     (org-ref-open-citation-at-point)
    				     (bibtex-beginning-of-entry)
    				     (doi-utils-get-bibtex-entry-pdf))))
    	    candidates)))

    (if notes-p
	(cl-pushnew
	 '("Open notes" . org-ref-open-notes-at-point)
	 candidates)
      (cl-pushnew
       '("Add notes" . org-ref-open-notes-at-point)
       candidates))

    ;; conditional url and doi functions
    (when (or url doi)
      (cl-pushnew
       '("Open in browser" . org-ref-open-url-at-point)
       candidates))

    (when doi
      (mapc (lambda (x)
              (cl-pushnew x candidates))
            `(("WOS" . org-ref-wos-at-point)
              ("Related articles in WOS" . org-ref-wos-related-at-point)
              ("Citing articles in WOS" . org-ref-wos-citing-at-point)
              ("Google Scholar" . org-ref-google-scholar-at-point)
              ("Pubmed" . org-ref-pubmed-at-point)
              ("Crossref" . org-ref-crossref-at-point))))

    (cl-pushnew
     '("Insert new citation" . (lambda ()
                                 (org-ref-helm-insert-cite-link nil)))
     candidates)

    (cl-pushnew
     '("Delete key at point" . org-ref-delete-key-at-point)
     candidates)

    ;;  This is kind of clunky. We store the key at point. Add the new ref. Get
    ;;  it off the end, and put it in the original position.
    (cl-pushnew
     '("Replace key at point" . org-ref-replace-key-at-point)
     candidates)

    (cl-pushnew
     '("Delete citation at point" . org-ref-delete-cite-at-point)
     candidates)

    (when bibtex-completion-cite-prompt-for-optional-arguments
      (cl-pushnew
       '("Update pre/post text" . org-ref-update-pre-post-text)
       candidates))

    (cl-pushnew
     '("Sort keys by year" . org-ref-sort-citation-link)
     candidates)

    (cl-pushnew
     '("Copy formatted citation to clipboard" . org-ref-copy-cite-as-summary)
     candidates)

    (cl-pushnew
     '("Copy key to clipboard" . (lambda ()
                                   (kill-new
                                    (car (org-ref-get-bibtex-key-and-file)))))
     candidates)

    (cl-pushnew
     '("Copy bibtex entry to file" . org-ref-copy-entry-at-point-to-file)
     candidates)

    (cl-pushnew
     '("Email bibtex entry and pdf" . (lambda ()
                                        (save-excursion
                                          (org-ref-open-citation-at-point)
                                          (org-ref-email-bibtex-entry))))
     candidates)

    ;; add Scopus functions. These work by looking up a DOI to get a Scopus
    ;; EID. This may only work for Scopus articles. Not all DOIs are recognized
    ;; in the Scopus API. We only load these if you have defined a
    ;; `*scopus-api-key*', which is required to do the API queries. See
    ;; `scopus'. These functions are appended to the candidate list.
    (when (and (boundp '*scopus-api-key*) *scopus-api-key*)
      (cl-pushnew
       '("Open in Scopus" . (lambda ()
                              (let ((eid (scopus-doi-to-eid (org-ref-get-doi-at-point))))
                                (if eid
                                    (scopus-open-eid eid)
                                  (message "No EID found.")))))
       candidates)

      (cl-pushnew
       '("Scopus citing articles" . (lambda ()
                                      (let ((url (scopus-citing-url
                                                  (org-ref-get-doi-at-point))))
                                        (if url
                                            (browse-url url)
                                          (message "No url found.")))))
       candidates)

      (cl-pushnew
       '("Scopus related by authors" . (lambda ()
                                         (let ((url (scopus-related-by-author-url
                                                     (org-ref-get-doi-at-point))))
                                           (if url
                                               (browse-url url)
                                             (message "No url found.")))))
       candidates)

      (cl-pushnew
       '("Scopus related by references" . (lambda ()
                                            (let ((url (scopus-related-by-references-url
                                                        (org-ref-get-doi-at-point))))
                                              (if url
                                                  (browse-url url)
                                                (message "No url found.")))))
       candidates)

      (cl-pushnew
       '("Scopus related by keywords" . (lambda ()
                                          (let ((url (scopus-related-by-keyword-url
                                                      (org-ref-get-doi-at-point))))
                                            (if url
                                                (browse-url url)
                                              (message "No url found.")))))
       candidates))

    ;; finally return a numbered list of the candidates
    (cl-loop for i from 0
             for cell in (reverse candidates)
             collect (cons (format "%2s. %s" i (car cell))
                           (cdr cell)))))


(defvar org-ref-helm-user-candidates '()
  "List of user-defined candidates to act when clicking on a cite link.
This is a list of cons cells '((\"description\" . action)). The
action function should not take an argument, and should assume
point is on the cite key of interest.")

;; example of adding your own function
(add-to-list
 'org-ref-helm-user-candidates
 '("Open pdf in emacs" . (lambda ()
                           (find-file
                            (concat
                             (file-name-as-directory org-ref-pdf-directory)
                             (car (org-ref-get-bibtex-key-and-file))
                             ".pdf"))))
 t)

;;;###autoload
(defun org-ref-cite-click-helm (_key)
  "Open helm for actions on a cite link.
subtle points.

1. get name and candidates before entering helm because we need
the org-buffer.

2. switch back to the org buffer before evaluating the
action.  most of them need the point and buffer.

KEY is returned for the selected item(s) in helm."
  (interactive)
  (let ((name (org-ref-format-entry (org-ref-get-bibtex-key-under-cursor)))
        (candidates (org-ref-cite-candidates))
        (cb (current-buffer)))

    (helm :sources `(((name . ,name)
                      (candidates . ,candidates)
                      (action . (lambda (f)
                                  (switch-to-buffer ,cb)
                                  (funcall f))))
                     ((name . "User functions")
                      (candidates . ,org-ref-helm-user-candidates)
                      (action . (lambda (f)
                                  (switch-to-buffer ,cb)
                                  (funcall f))))))))


;; browse labels

(defun org-ref-browser-label-source ()
  (let ((labels (org-ref-get-labels)))
    (helm-build-sync-source "Browse labels"
      :follow 1
      :candidates labels
      :action '(("Browse labels" . (lambda (label)
				     (with-selected-window (selected-window)
				       (org-open-link-from-string
					(format "ref:%s" label)))))))))

;; browse citation links

(defun org-ref-browser-transformer (candidates)
  "Add counter to candidates."
  (let ((counter 0))
    (cl-loop for i in candidates
	     collect (format "%s %s" (cl-incf counter) i))))

(defun org-ref-browser-display (candidate)
  "Strip counter from candidates."
  (replace-regexp-in-string "^[0-9]+? " "" candidate))

;;;###autoload
(defun org-ref-browser (&optional arg)
  "Quickly browse label links in helm.
With a prefix ARG, browse citation links."
  (interactive "P")
  (if arg
      (let ((keys nil)
	    (alist nil))
	(widen)
	(outline-show-all)
	(org-element-map (org-element-parse-buffer) 'link
	  (lambda (link)
	    (let ((plist (nth 1 link)))
	      (when (-contains? org-ref-cite-types (plist-get plist ':type))
		(let ((start (org-element-property :begin link)))
		  (dolist (key
			   (org-ref-split-and-strip-string (plist-get plist ':path)))
		    (setq keys (append keys (list key)))
		    (setq alist (append alist (list (cons key start))))))))))
	(let ((counter 0)
	      count-key-pos)
	  ;; the idea here is to create an alist with ("counter key" .
	  ;; position) to produce unique candidates
	  (setq count-key-pos (mapcar (lambda (x)
					(cons
					 (format "%s %s" (cl-incf counter) (car x)) (cdr x)))
				      alist))
	  ;; push mark to restore position with C-u C-SPC
	  (push-mark (point))
	  ;; move point to the first citation link in the buffer
	  (goto-char (cdr (assoc (caar alist) alist)))
	  (helm :sources
		(helm-build-sync-source "Browse citation links"
		  :follow 1
		  :candidates keys
		  :candidate-transformer 'org-ref-browser-transformer
		  :real-to-display 'org-ref-browser-display
		  :persistent-action (lambda (candidate)
				       (helm-goto-char
					(cdr (assoc candidate count-key-pos))))
		  :action `(("Open menu" . ,(lambda (candidate)
					      (helm-goto-char
					       (cdr (assoc candidate count-key-pos)))
					      (org-open-at-point)))))
		:candidate-number-limit 10000
		:buffer "*helm browser*")))
    (helm :sources (org-ref-browser-label-source)
	  :buffer "*helm labels*")))



(provide 'org-ref-helm-bibtex)
;;; org-ref-helm-bibtex.el ends here
