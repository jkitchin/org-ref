;;; org-ref-ivy.el --- org-ref with ivy completion -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2021  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; URL: https://github.com/jkitchin/org-ref
;; Version: 1.0
;; Keywords: org-mode, cite, ref, label
;; Package-Requires: ((org-ref "0") (ivy-bibtex "0"))

;; This file is not currently part of GNU Emacs.

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

(require 'org-ref-core)
(require 'ivy-bibtex)


;; all these functions are defined in ivy-bibtex, but not in a variable. Here
;; you can set them as you see fit.
(defcustom org-ref-citation-alternate-insert-actions
  '(("p" ivy-bibtex-open-pdf "Open PDF file (if present)")
    ("u" ivy-bibtex-open-url-or-doi "Open URL or DOI in browser")
    ;; this insert-citation only inserts an org-ref cite.
    ;; ("c" ivy-bibtex-insert-citation "Insert citation")
    ("R" ivy-bibtex-insert-reference "Insert formatted reference")
    ("k" ivy-bibtex-insert-key "Insert BibTeX key")
    ("b" ivy-bibtex-insert-bibtex "Insert BibTeX entry")
    ("a" ivy-bibtex-add-PDF-attachment "Attach PDF to email")
    ("e" ivy-bibtex-edit-notes "Edit notes")
    ("s" ivy-bibtex-show-entry "Show entry")
    ("l" ivy-bibtex-add-pdf-to-library "Add PDF to library")
    ("f" (lambda (_candidate) (ivy-bibtex-fallback ivy-text)) "Fallback options"))
  "Alternate actions to do instead of inserting.")


(defun org-ref-cite-insert-ivy ()
  "Function for inserting a citation."
  (interactive)

  (ivy-set-actions
   'org-ref-cite-insert-ivy
   org-ref-citation-alternate-insert-actions)

  (unless bibtex-completion-display-formats-internal
    (bibtex-completion-init))

  (let* ((bibtex-completion-bibliography (org-ref-find-bibliography))
	 (candidates (if org-ref-buffer-local-candidates
			 org-ref-buffer-local-candidates
		       ;; trigger cached variables
		       (org-ref-valid-keys)
		       org-ref-buffer-local-candidates))
	 (choice (ivy-read "BibTeX entries: " candidates
			   :preselect (ivy-thing-at-point)
			   :action '(1
				     ("o" (lambda (candidate)
					    (org-ref-insert-cite-key
					     (cdr (assoc "=key=" (cdr candidate)))))
				      "insert")
				     ("r" (lambda (candidate)
					    (let* ((object (org-element-context))
						   (type (org-element-property :type object))
						   (begin (org-element-property :begin object))
						   (end (org-element-property :end object))
						   (link-string (org-element-property :path object))
						   (data (org-ref-parse-cite-path link-string))
						   (references (plist-get data :references))
						   (cp (point))
						   (key)
						   keys i)
					      ;;   We only want this to work on citation links
					      (when (-contains? org-ref-cite-types type)
						(setq key (org-ref-get-bibtex-key-under-cursor))
						(if (null key)
						    ;; delete the whole cite
						    (cl--set-buffer-substring begin end "")
						  (setq i (seq-position
							   references key
							   (lambda (el key)
							     (string= key
								      (plist-get el :key)))))
						  (setf (plist-get (nth i references) :key)
							(cdr (assoc "=key=" (cdr candidate))))
						  (setq data (plist-put data :references references))
						  (save-excursion
						    (goto-char begin)
						    (re-search-forward link-string)
						    (replace-match (org-ref-interpret-cite-data data)))
						  (goto-char cp)))))
				      "Replace key at point"))
			   :caller 'org-ref-cite-insert-ivy)))))


(setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
      org-ref-insert-cite-function 'org-ref-cite-insert-ivy
      org-ref-insert-label-function 'org-ref-insert-label-link
      org-ref-insert-ref-function 'org-ref-insert-ref-link
      org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body)))


(provide 'org-ref-ivy)

;;; org-ref-ivy.el ends here
