;;; org-ref-bibliography-links.el --- Bibliography and bibliographystyle links

;;; Commentary:
;;

;;; Code:

(defcustom org-ref-latex-bib-resolve-func #'file-relative-name
  "Function to expand paths to the bibliography file on latex export.
Use this to convert a path to what you want, e.g. relative to
some directory, absolute, etc."
  :type 'function
  :group 'org-ref)


;;* bibliography* links

(defun org-ref-get-bibfile-path (bibfile)
  "Get a path to BIBFILE as local file, or using kpsewhich.
This should allow you to use a bib file that is setup with TeX
variables like BIBINPUTS."
  (or (when (file-exists-p bibfile) bibfile)
      (let ((f (replace-regexp-in-string
		"\n$" ""
		(shell-command-to-string (format "kpsewhich %s" bibfile)))))
	(unless (string= "" f)
	  f))))


(defun org-ref-bibliography*-export (cmd bibfiles _desc backend)
  "Exporting function for bibliography links.
To be used as a partial function e.g.
 (apply-partially \"bibliography\" 'org-ref-bibliography*-export)
Argument CMD is the command it should export to.
Argument BIBFILES is a comma-separated list of strings.
Argument BACKEND is the export backend."
  (cond
   ((eq backend 'latex)
    (format "%s{%s}"
	    cmd
	    (replace-regexp-in-string
	     "\\.bib" ""
	     (mapconcat
	      'identity
	      (mapcar
	       (lambda (f)
		 (funcall org-ref-latex-bib-resolve-func (org-ref-get-bibfile-path f)))
	       (split-string bibfiles ","))
	      ","))))))


(defun org-ref-bibliography-activate (start end path _bracketp)
  "Activate a bibliography link.
Adds a warning face to non-existent or invalid bib-files.
START and END are the bounds of the link.
PATH is a comma-separated list of bibfiles."
  (goto-char start)
  (cl-loop for p in (split-string path ",") do
	   (setq p (string-trim p))
	   (search-forward p)
	   (put-text-property (match-beginning 0) (match-end 0) 'org-ref-bibfile p)

	   ;; activate files that don't exist
	   (when (not (file-exists-p p))
	     (put-text-property (match-beginning 0) (match-end 0)
				'face 'font-lock-warning-face)
	     (put-text-property (match-beginning 0) (match-end 0)
				'help-echo "This file was not found."))

	   (when (file-exists-p p)
	     ;; Let's do a validation, but only if it has changed since the last time we checked.
	     (let* ((mod-time-last-check (or (get-text-property (match-beginning 0)
								'mod-time-last-check)
					     '(0 0 0 0)))
		    (last-modified (file-attribute-modification-time (file-attributes p)))
		    (bibtex-valid))
	       (if (time-equal-p mod-time-last-check last-modified)
		   (setq bibtex-valid (get-text-property (match-beginning 0) 'bibtex-valid))

		 ;; the times were not equal, so we check and store the state.
		 (setq bibtex-valid (save-match-data
				      (with-current-buffer (find-file-noselect p)
					(bibtex-validate))))
		 (put-text-property (match-beginning 0)
				    (match-end 0)
				    'mod-time-last-check
				    last-modified)
		 (put-text-property (match-beginning 0)
				    (match-end 0)
				    'bibtex-valid
				    bibtex-valid))

	       (unless bibtex-valid
		 (put-text-property (match-beginning 0) (match-end 0)
				    'face 'font-lock-warning-face)
		 (put-text-property (match-beginning 0) (match-end 0)
				    'help-echo "This file did not pass `bibtex-validate'."))))))



(defun org-ref-bibliography*-follow (_path)
  "Function to follow bibliography links."
  (interactive)
  (find-file (org-ref-get-bibfile-path (get-text-property (point) 'org-ref-bibfile))))


(defun org-ref-printbibliography-export (options _desc backend)
  "Export function for printbibliography links.
Argument OPTIONS are the options used for the command.
Optional argument BACKEND is the export backend."
  (cond
   ((eq backend 'latex)
    ;; write out the biblatex bibliography command
    (format "\\printbibliography%s"
	    (if (not (string= "" options))
		(format "[%s]" options)
	      "")))))


(defun org-ref-insert-bibliography-link ()
  "Insert a bibliography link for the files used in this buffer."
  (interactive)
  (let* ((cite-links (org-element-map (org-element-parse-buffer) 'link
		       (lambda (lnk)
			 (when (member (org-element-property :type lnk) org-ref-cite-types)
			   lnk))))
	 (keys (delete-dups (cl-loop for cl in cite-links append
				     (cl-loop for ref in (plist-get (org-ref-parse-cite-path
								     (org-element-property :path cl))
								    :references)
					      collect (plist-get ref :key)))))
	 (files (delete-dups (cl-loop for key in keys collect
				      (save-window-excursion
					(bibtex-completion-show-entry (list key))
					(buffer-file-name))))))
    (insert (format "bibliography:%s" (string-join files ",")))))


;; ** bibliography* links

(org-link-set-parameters "bibliography"
			 :follow #'org-ref-bibliography*-follow
			 :help-echo "Bibliography link"
			 :export (apply-partially 'org-ref-bibliography*-export "\\bibliography")
			 :activate-func #'org-ref-bibliography-activate
			 :face #'org-link)


(org-link-set-parameters "nobibliography"
			 :help-echo "No bibliography link"
			 :activate-func #'org-ref-bibliography-activate
			 :follow #'org-ref-bibliography*-follow
			 :export (apply-partially 'org-ref-bibliography*-export "\\nobibliography")
			 :face #'org-link)


(org-link-set-parameters "printbibliography"
			 :export #'org-ref-printbibliography-export)

;; Note I removed the addbibresource link, it goes in the header, not the document.


;; *** bibliographystyle

(defun org-ref-bibliography-styles ()
  "Return a list of known bibliography styles."
  (mapcar 'file-name-nondirectory
	  (mapcar 'file-name-sans-extension
		  (-flatten
		   (mapcar (lambda (path)
			     (setq path (replace-regexp-in-string "!" "" path))
			     (when (file-directory-p path)
			       (f-entries path (lambda (f) (f-ext? f "bst")) t)))
			   (split-string
			    ;; https://tex.stackexchange.com/questions/431948/get-a-list-of-installed-bibliography-styles-with-kpsewhich?noredirect=1#comment1082436_431948
			    (shell-command-to-string "kpsewhich -expand-path '$BSTINPUTS'")
			    ":"))))))



(defun org-ref-bibliographystyle-complete-link (&optional arg)
  "Completion function for bibliography style links.
ARG is a not used."
  (when (executable-find "kpsewhich")
    (concat "bibliographystyle:"
	    (completing-read "Style: " (org-ref-bibliography-styles)))))


(defun org-ref-bibliographystyle-activate (start _end path _bracketp)
  "Activation function for bibliography styles.
START is the beginning position of the link.
Optional argument PATH contains the selected style."
  (unless (member path (org-ref-bibliography-styles))
    (goto-char start)
    (search-forward path)
    (put-text-property (match-beginning 0) (match-end 0) 'face 'font-lock-warning-face)
    (put-text-property (match-beginning 0) (match-end 0) 'help-echo "Unrecognized style")))


(defun org-ref-bibliographystyle-export (style _desc backend)
  "Export function for bibliographystyle links.
Argument STYLE is the desired style.
Optional argument BACKEND is the export backend."
  (cond
   ((or (eq backend 'latex)
	(eq backend 'beamer))
    ;; write out the latex bibliography command
    (format "\\bibliographystyle{%s}" style))))

(org-link-set-parameters "bibliographystyle"
			 :complete #'org-ref-bibliographystyle-complete-link
			 :activate-func #'org-ref-bibliographystyle-activate
			 :export #'org-ref-bibliographystyle-export)

(provide 'org-ref-bibliography-links)

;;; org-ref-bibliography-links.el ends here
