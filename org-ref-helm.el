;;; org-ref-helm.el --- Generic helm functions for org-ref  -*- lexical-binding: t; -*-

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

;; These are not specific to helm-bibtex.

;;; Code:

(declare-function 'org-ref-get-bibtex-key-and-file "org-ref-core.el")
(declare-function 'org-ref-bad-file-link-candidates "org-ref-core.el")
(declare-function 'org-ref-get-labels "org-ref-core.el")
(declare-function 'org-ref-bad-cite-candidates "org-ref-core.el")
(declare-function 'org-ref-bad-ref-candidates "org-ref-core.el")
(declare-function 'org-ref-bad-label-candidates "org-ref-core.el")

(require 'helm)
(require 'org-element)
(require 'org-ref-core)

;;;###autoload
(defun org-ref-helm-insert-label-link ()
  "Insert label link at point.
Helm will display existing labels in the current buffer to avoid
duplication."
  (interactive)
  (let ((labels (org-ref-get-labels)))
    (helm :sources `(,(helm-build-sync-source "Existing labels"
			:candidates labels
			:action (lambda (label)
				  (with-helm-current-buffer
				    (org-open-link-from-string
				     (format "ref:%s" label)))))
		     ,(helm-build-dummy-source "Create new label"
			:action (lambda (label)
				  (with-helm-current-buffer
				    (insert (concat "label:" label))))))
	  :buffer "*helm labels*")))


;;;###autoload
(defun org-ref-helm-insert-ref-link ()
  "Helm menu to insert ref links to labels in the document.
If you are on link, replace with newly selected label.  Use
\\[universal-argument] to insert a different kind of ref link.
Use a double \\[universal-argument] \\[universal-argument] to insert a
[[#custom-id]] link"
  (interactive)
  (let* ((labels (org-ref-get-labels))
         (contexts (mapcar 'org-ref-get-label-context labels))
         (cb (current-buffer)))

    (helm :input (thing-at-point 'word)
          :sources `(((name . "Available labels to ref")
                      (multiline)
                      (candidates . ,(cl-loop for label in labels
                                              for context in contexts
                                              ;; we do some kludgy adding spaces
                                              ;; and bars to make it "easier" to
                                              ;; see in helm.
                                              collect (cons (concat
                                                             label "\n"
                                                             (mapconcat
                                                              (lambda (x)
                                                                (concat "   |" x))
                                                              (split-string context "\n")
                                                              "\n"
                                                              ) "\n\n") label)))
                      ;; default action to replace or insert ref link.
                      (action . (lambda (label)
                                  (switch-to-buffer ,cb)

                                  (cond
                                   ;;  no prefix or on a link
                                   ((equal helm-current-prefix-arg nil)
                                    (let* ((object (org-element-context))
                                           (last-char
					    (save-excursion
					      (goto-char (org-element-property :end object))
					      (backward-char)
					      (if (looking-at " ")
						  " "
						""))))
                                      (if (-contains? '("ref" "eqref" "pageref" "nameref")
                                                      (org-element-property :type object))
                                          ;; we are on a link, so replace it.
                                          (setf
                                           (buffer-substring
                                            (org-element-property :begin object)
                                            (org-element-property :end object))
                                           (concat
                                            (replace-regexp-in-string
                                             (org-element-property :path object)
                                             label
                                             (org-element-property :raw-link object))
                                            last-char))
                                        ;; insert a new link
                                        (insert
                                         (concat
                                          "ref:" label))
                                        )))
                                   ;; one prefix, alternate ref link
                                   ((equal helm-current-prefix-arg '(4))
                                    (insert
                                     (concat
                                      (helm :sources '((name . "Ref link types")
                                                       (candidates . ("ref" "eqref"
								      "pageref" "nameref"))
                                                       (action . (lambda (x) x))))
                                      ":" label)))
                                   ;; two prefixes, insert section custom-id link
                                   ((equal helm-current-prefix-arg '(16))
                                    (insert
                                     (format "[[#%s]]" label)))))))))))

;;;###autoload
(defun org-ref ()
  "Opens a helm interface to actions for `org-ref'.
Shows bad citations, ref links and labels.
This widens the file so that all links go to the right place."
  (interactive)
  ;; (widen)
  ;; (org-cycle '(64))
  (let ((cb (current-buffer))
        (bad-citations (org-ref-bad-cite-candidates))
        (bad-refs (org-ref-bad-ref-candidates))
        (bad-labels (org-ref-bad-label-candidates))
        (bad-files (org-ref-bad-file-link-candidates))
        (bib-candidates '()))

    ;; setup bib-candidates. This checks a variety of things in the
    ;; bibliography, bibtex files. check for which bibliographies are used
    (cl-pushnew
     (cons (format  "Using these bibtex files: %s"
                    (org-ref-find-bibliography))
           (lambda () nil))
     bib-candidates)

    ;; Check bibliography style exists
    (save-excursion
      (goto-char 0)
      (unless (re-search-forward "bibliographystyle:\\|\\\\bibliographystyle{" nil t)
        (cl-pushnew
	 (cons "No bibliographystyle found."
	       (lambda ()
		 (switch-to-buffer "*org-ref*")
		 (erase-buffer)
		 (insert "No bibliography style found. This may be ok, if your latex class style sets that up, but if not this is an error. Try adding something like:
bibliographystyle:unsrt
at the end of you file.
")
		 (org-mode)))
	 bib-candidates)))

    ;; Check if latex knows of the bibliographystyle. We only check links here.
    ;;  I also assume this style exists as a bst file that kpsewhich can find.
    (save-excursion
      (goto-char 0)
      (when (re-search-forward "bibliographystyle:" nil t)
        ;; on a link. get style
        (let ((path (org-element-property :path (org-element-context))))
          (unless (= 0 (shell-command (format "kpsewhich %s.bst" path)))
            (cl-pushnew
	     (cons (format "bibliographystyle \"%s\" may be unknown" path)
		   (lambda ()
		     (goto-char 0)
		     (re-search-forward "bibliographystyle:")))
	     bib-candidates)))))

    ;; check for multiple bibliography links
    (let* ((bib-links (-filter
                       (lambda (el)
                         (string= (org-element-property :type el) "bibliography"))
                       (org-element-map (org-element-parse-buffer) 'link 'identity)))
           (n-bib-links (length bib-links)))

      (when (> n-bib-links 1)
        (mapc (lambda (link)
                (setq
                 bib-candidates
                 (append
                  bib-candidates
                  (list (cons (format  "Multiple bibliography link: %s"
				       (org-element-property :raw-link link))
                              `(lambda ()
                                 (goto-char ,(org-element-property :begin link))))))))
              bib-links)))

    ;; Check for bibliography files existence.
    (mapc (lambda (bibfile)
            (unless (file-exists-p bibfile)
              (cl-pushnew
	       (cons
		(format "%s does not exist." bibfile)
		(lambda ()
		  (message "Non-existent bibfile.")))
	       bib-candidates)))
          (org-ref-find-bibliography))

    ;; check for spaces in bibliography
    (let ((bibfiles (mapcar 'expand-file-name
                            (org-ref-find-bibliography))))
      (mapc (lambda (bibfile)
              (when (string-match " " bibfile)
                (cl-pushnew
                 (cons (format "One or more spaces found in path to %s" bibfile)
                       (lambda ()
                         (message "No spaces are allowed in bibtex file paths. We recommend replacing them with -. Underscores usually cause other problems if you don't know what you are doing.")))
		 bib-candidates)))
            bibfiles))

    ;; validate bibtex files
    (let ((bibfiles (mapcar 'expand-file-name
                            (org-ref-find-bibliography))))
      (mapc
       (lambda (bibfile)
         (unless (with-current-buffer
                     (find-file-noselect bibfile)
                   (bibtex-validate))
           (cl-pushnew
	    (cons
	     (format  "Invalid bibtex file found. %S" bibfile)
	     `(lambda ()
		(find-file ,bibfile)))
	    bib-candidates)))
       bibfiles))


    (helm :sources `(((name . "Bad citations")
                      (candidates . ,bad-citations)
                      (action . (lambda (marker)
                                  (switch-to-buffer (marker-buffer marker))
                                  (goto-char marker)
				  (org-show-entry))))
                     ;;
                     ((name . "Multiply defined labels")
                      (candidates . ,bad-labels)
                      (action . (lambda (marker)
                                  (switch-to-buffer (marker-buffer marker))
                                  (goto-char marker)
				  (org-show-entry))))
                     ;;
                     ((name . "Bad ref links")
                      (candidates . ,bad-refs)
                      (action . (lambda (marker)
                                  (switch-to-buffer (marker-buffer marker))
                                  (goto-char marker)
				  (org-show-entry))))
                     ;;
                     ((name . "Bad file links")
                      (candidates . ,bad-files)
                      (lambda (marker)
                        (switch-to-buffer (marker-buffer marker))
                        (goto-char marker)
			(org-show-entry)))

                     ((name . "Bibliography")
                      (candidates . ,bib-candidates)
                      (action . (lambda (x)
                                  (switch-to-buffer ,cb)
                                  (funcall x))))
		     ((name . "Miscellaneous")
		      (candidates . (,(format "org-latex-prefer-user-labels = %s"
					      org-latex-prefer-user-labels)
				     ,(format "bibtex-dialect = %s" bibtex-dialect)))
		      (action . nil))
                     ;;
                     ((name . "Utilities")
                      (candidates . (("Check buffer again" . org-ref)
                                     ("Insert citation" . helm-bibtex)
                                     ("Insert label link" . org-ref-helm-insert-label-link)
                                     ("Insert ref link" . org-ref-helm-insert-ref-link)
                                     ("List of figures" . org-ref-list-of-figures)
                                     ("List of tables" . org-ref-list-of-tables)
                                     ("Table of contents" . helm-org-in-buffer-headings)))
                      (action . (lambda (x)
                                  (switch-to-buffer ,cb)
                                  (funcall x))))
                     ;;
                     ((name . "Document utilities")
                      (candidates . (("Spell check document" . ispell)))
                      (action . (lambda (x)
                                  (switch-to-buffer ,cb)
                                  (funcall x))))
                     ;; Exports
                     ((name . "Export functions")
                      (candidates . (("Extract cited entries" . org-ref-extract-bibtex-entries)
                                     ("Export to html and open" . (lambda ()
                                                                    (org-open-file
                                                                     (org-html-export-to-html))))
                                     ("Export to pdf and open" . (lambda ()
                                                                   (org-open-file
                                                                    (org-latex-export-to-pdf))))
                                     ("Export to manuscript pdf and open" . ox-manuscript-export-and-build-and-open)
                                     ("Export submission manuscript pdf and open" . ox-manuscript-build-submission-manuscript-and-open)))
                      (action . (lambda (x)
                                  (switch-to-buffer ,cb)
                                  (funcall x))))))))


;;;###autoload
(defun helm-tag-bibtex-entry ()
  "Helm interface to add keywords to a bibtex entry.
Run this with the point in a bibtex entry."
  (interactive)
  (let ((keyword-source `((name . "Existing keywords")
                          (candidates . ,(org-ref-bibtex-keywords))
                          (action . (lambda (candidate)
                                      (org-ref-set-bibtex-keywords
                                       (mapconcat
                                        'identity
                                        (helm-marked-candidates)
                                        ", "))))))
        (fallback-source `((name . "Add new keywords")
                           (dummy)
                           (action . (lambda (candidate)
                                       (org-ref-set-bibtex-keywords helm-pattern))))))
    (helm :sources `(,keyword-source ,fallback-source))))

(provide 'org-ref-helm)
;;; org-ref-helm.el ends here
