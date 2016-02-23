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
(require 'swiper)
(require 'org-ref-bibtex)

;;;###autoload
(defun org-ref-ivy-bibtex-completion ()
  "Use helm and ‘helm-bibtex’ for completion."
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
  (let ((pdf (expand-file-name
	      (format "%s.pdf"
		      (cdr (assoc "=key=" entry)))
	      org-ref-pdf-directory)))
    (if (file-exists-p pdf)
	(org-open-file pdf)
      (message "No pdf found for %s" (cdr (assoc "=key=" entry))))))


(defun or-ivy-bibtex-open-entry (entry)
  "Open the bibtex file at ENTRY.
ENTRY is selected from `orhc-bibtex-candidates'."
  (find-file (cdr (assoc "bibfile" entry)))
  (goto-char (cdr (assoc "position" entry)))
  (bibtex-beginning-of-entry))


(defun org-ref-ivy-insert-cite-link ()
  "ivy function for interacting with bibtex."
  (interactive)
  (setq org-ref-bibtex-files (org-ref-find-bibliography))
  (ivy-read "Open: " (orhc-bibtex-candidates)
	    :require-match t
	    :action '(1
		      ("i" or-ivy-bibtex-insert-cite "Insert citation")
		      ("o" or-ivy-bibtex-open-entry "Open entry")
		      ("p" or-ivy-bibtex-open-pdf "open Pdf")
		      ("q" nil "quit"))))

(add-to-list 'ivy-re-builders-alist (cons 'org-ref-ivy-insert-cite-link
					  'ivy--regex-ignore-order))


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
