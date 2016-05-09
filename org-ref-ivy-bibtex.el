;;; org-ref-ivy-bibtex.el --- Use ivy for completion in org-ref  -*- lexical-binding: t; -*-

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

;;

;;; Code:
(unless (require 'ivy nil t)
  (message "Org-ref is installing `ivy'...")
  (let ((package-archives '(("gnu"         . "http://elpa.gnu.org/packages/")
			    ("melpa" . "http://melpa.org/packages/"))))
    (package-initialize)
    (package-refresh-contents)
    (package-install 'ivy))
  (require 'ivy))
(require 'org-ref-bibtex)

;;;###autoload
(defun org-ref-ivy-bibtex-completion ()
  "Use ivy for completion."
  (interactive)
  ;; Define core functions for org-ref
  (setq org-ref-insert-link-function 'org-ref-insert-link
	org-ref-insert-cite-function 'org-ref-ivy-insert-cite-link
	org-ref-insert-label-function 'org-ref-ivy-insert-label-link
	org-ref-insert-ref-function 'org-ref-ivy-insert-ref-link
	org-ref-cite-onclick-function (lambda (_) (org-ref-cite-hydra/body)))

  ;; define key for inserting citations
  (define-key org-mode-map
    (kbd org-ref-insert-cite-key)
    org-ref-insert-link-function))

(org-ref-ivy-bibtex-completion)

(define-key org-mode-map (kbd "C-c C-r") 'ivy-resume)

;; messages in minibuffer interfere with hydra menus.
(setq org-ref-show-citation-on-enter nil)


(defun or-looking-forward-cite ()
  "Return if point is in the position before a citation."
  (save-excursion
    (forward-char)
    (-contains? org-ref-cite-types
		(org-element-property
		 :type
		 (org-element-context)))))


(defun or-looking-back-cite ()
  "Return if point is in the position after a citation."
  (save-excursion
    (forward-char -1)
    (-contains? org-ref-cite-types
		(org-element-property
		 :type
		 (org-element-context)))))


(defun or-ivy-bibtex-insert-cite (entry)
  "Insert a citation for ENTRY.
ENTRY is selected from `orhc-bibtex-candidates'."
  (with-current-buffer (ivy-state-buffer ivy-last)
    (org-ref-insert-key-at-point (list (cdr (assoc "=key=" entry))))))


(defun or-ivy-bibtex-open-pdf (entry)
  "Open the pdf associated with ENTRY.
ENTRY is selected from `orhc-bibtex-candidates'."
  (with-ivy-window
    (let ((pdf (expand-file-name
		(format "%s.pdf"
			(cdr (assoc "=key=" entry)))
		org-ref-pdf-directory)))
      (if (file-exists-p pdf)
	  (org-open-file pdf)
	(message "No pdf found for %s" (cdr (assoc "=key=" entry)))))))


(defun or-ivy-bibtex-open-notes (entry)
  "Open the notes associated with ENTRY.
ENTRY is selected from `orhc-bibtex-candidates'."
  (with-ivy-window
    (find-file (expand-file-name
		(format "%s.org"
			(cdr (assoc "=key=" entry)))
		org-ref-notes-directory))))


(defun or-ivy-bibtex-open-entry (entry)
  "Open the bibtex file at ENTRY.
ENTRY is selected from `orhc-bibtex-candidates'."
  (find-file (cdr (assoc "bibfile" entry)))
  (goto-char (cdr (assoc "position" entry)))
  (bibtex-beginning-of-entry))


(defun or-ivy-bibtex-copy-entry (entry)
  "Copy selected bibtex ENTRY to the clipboard."
  (with-temp-buffer
    (save-window-excursion
      (or-ivy-bibtex-open-entry entry)
      (bibtex-copy-entry-as-kill))
    (bibtex-yank)
    (kill-region (point-min) (point-max))))


(defun or-ivy-bibtex-open-url (entry)
  "Open the URL associated with ENTRY.
ENTRY is selected from `orhc-bibtex-candidates'."
  (let ((url (cdr (assoc "url" entry))))
    (if url
	(browse-url url)
      (message "No url found for %s" (cdr (assoc "=key=" entry))))))


(defun or-ivy-bibtex-open-doi (entry)
  "Open the DOI associated with ENTRY.
ENTRY is selected from `orhc-bibtex-candidates'."
  (let ((doi (cdr (assoc "doi" entry))))
    (if doi
	(browse-url (format "http://dx.doi.org/%s" doi))
      (message "No doi found for %s" (cdr (assoc "=key=" entry))))))


(defun or-ivy-bibtex-set-keywords (entry)
  "Prompt for keywords, and put them on the selected ENTRY."
  (let ((keywords (read-string "Keyword(s) comma-separated: " ))
	entry-keywords)
    (save-window-excursion
      (or-ivy-bibtex-open-entry entry)
      (setq entry-keywords (bibtex-autokey-get-field "keywords"))
      (bibtex-set-field
       "keywords"
       (if (> (length entry-keywords) 0)
	   (concat entry-keywords ", " keywords)
	 keywords)))))


(defun or-ivy-bibtex-email-entry (entry)
  "Insert selected ENTRY and attach pdf file to an email.
Create email unless called from an email."
  (with-ivy-window
    (unless (memq major-mode '(message-mode mu4e-compose-mode))
      (compose-mail))
    (save-window-excursion
      (or-ivy-bibtex-open-entry entry)
      (bibtex-copy-entry-as-kill))
    (message-goto-body)
    (insert (pop bibtex-entry-kill-ring))
    (insert "\n")
    (let ((pdf (expand-file-name
		(format "%s.pdf"
			(cdr (assoc "=key=" entry)))
		org-ref-pdf-directory)))
      (if (file-exists-p pdf)
	  (mml-attach-file pdf)))
    (message-goto-to)))


(defun or-ivy-bibtex-formatted-citation (entry)
  "Return string containing formatted citations for ENTRY."
  (let ((enable-recursive-minibuffers t))
    (ivy-read "Style: " '("unsrt" "author-year")
	      :action 'load-library
	      :require-match t
	      :preselect "unsrt"
	      :caller 'or-ivy-formatted-citation)
    (format "%s\n\n" (orhc-formatted-citation entry))))


(defun or-ivy-bibtex-insert-formatted-citation (entry)
  "Insert formatted citations at point for selected ENTRY."
  (with-ivy-window
    (insert (or-ivy-bibtex-formatted-citation entry))))


(defun or-ivy-bibtex-copy-formatted-citation (entry)
  "Copy formatted citation to clipboard for ENTRY."
  (kill-new (or-ivy-bibtex-formatted-citation entry)))


(defun or-ivy-bibtex-add-entry (entry)
  "Open a bibliography file and move point to the end, in order to add a new bibtex entry. ENTRY is selected from `orhc-bibtex-candidates' but ignored."
  (ivy-read "bibtex file: " org-ref-bibtex-files
	    :require-match t
	    :action 'find-file
	    :caller 'or-ivy-bibtex-add-entry)
  (widen)
  (goto-char (point-max))
  (unless (bolp)
    (insert "\n")))


(defvar org-ref-ivy-cite-actions
  '(("b" or-ivy-bibtex-open-entry "Open bibtex entry")
    ("B" or-ivy-bibtex-copy-entry "Copy bibtex entry")
    ("p" or-ivy-bibtex-open-pdf "Open pdf")
    ("n" or-ivy-bibtex-open-notes "Open notes")
    ("u" or-ivy-bibtex-open-url "Open url")
    ("d" or-ivy-bibtex-open-doi "Open doi")
    ("k" or-ivy-bibtex-set-keywords "Add keywords")
    ("e" or-ivy-bibtex-email-entry "Email entry")
    ("f" or-ivy-bibtex-insert-formatted-citation "Insert formatted citation")
    ("F" or-ivy-bibtex-copy-formatted-citation "Copy formatted citation")
    ("a" or-ivy-bibtex-add-entry "Add bibtex entry"))
  "List of additional actions for `org-ref-ivy-insert-cite-link' (the default action being to insert a citation).")

(defvar org-ref-ivy-cite-re-builder 'ivy--regex-ignore-order
  "Regex builder to use in `org-ref-ivy-insert-cite-link'. Can be set to nil to use Ivy's default).")


(defun org-ref-ivy-insert-cite-link (&optional arg)
  "ivy function for interacting with bibtex."
  (interactive "P")
  (setq org-ref-bibtex-files (if arg
				 org-ref-default-bibliography
			       (org-ref-find-bibliography)))
  (ivy-read "Open: " (orhc-bibtex-candidates)
	    :require-match t
	    :re-builder org-ref-ivy-cite-re-builder
	    :action 'or-ivy-bibtex-insert-cite
	    :caller 'org-ref-ivy-insert-cite-link))

(ivy-set-actions
 'org-ref-ivy-insert-cite-link
 org-ref-ivy-cite-actions)



(defun org-ref-ivy-insert-label-link ()
  "Insert a label with ivy."
  (interactive)
  (insert
   (concat "label:"
	   (ivy-read "label: " (org-ref-get-labels)))))


(defun org-ref-ivy-insert-ref-link ()
  "Insert a ref link with ivy."
  (interactive)
  (insert
   (concat "ref:"
	   (ivy-read "ref: " (org-ref-get-labels) :require-match t))))


(require 'hydra)
(setq hydra-is-helpful t)

(defhydra org-ref-cite-hydra (:color blue)
  "
_p_: Open pdf     _w_: WOS          _g_: Google Scholar _K_: Copy citation to clipboard
_u_: Open url     _r_: WOS related  _P_: Pubmed         _k_: Copy key to clipboard
_n_: Open notes   _c_: WOS citing   _C_: Crossref       _f_: Copy bibtex entry to file
_o_: Open entry   _e_: Email entry and pdf
"
  ("o" org-ref-open-citation-at-point nil)
  ("p" org-ref-open-pdf-at-point nil)
  ("n" org-ref-open-notes-at-point nil)
  ("u" org-ref-open-url-at-point nil)
  ("w" org-ref-wos-at-point nil)
  ("r" org-ref-wos-related-at-point nil)
  ("c" org-ref-wos-citing-at-point nil)
  ("g" org-ref-google-scholar-at-point nil)
  ("P" org-ref-pubmed-at-point nil)
  ("C" org-ref-crossref-at-point nil)
  ("K" org-ref-copy-entry-as-summary nil)
  ("k" (progn
	 (kill-new
	  (car (org-ref-get-bibtex-key-and-file))))
   nil)
  ("f" org-ref-copy-entry-at-point-to-file nil)

  ("e" (save-excursion
	 (org-ref-open-citation-at-point)
	 (org-ref-email-bibtex-entry))
   nil))


(provide 'org-ref-ivy-bibtex)
;;; org-ref-ivy-bibtex.el ends here
