;;; org-ref-latex.el --- org-ref functionality for LaTeX files  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Keywords: languages

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

;;; Commentary: Make cites in LaTeX documents clickable, and with tooltips.
;; We use font-lock to add some functionality to the

;;

;;; Code:
(require 'org-ref)


(defvar org-ref-latex-cite-re
  (concat "\\\\\\(" (mapconcat
		     (lambda (x)
		       (replace-regexp-in-string "\*" "\\\\*" x))
		     org-ref-cite-types
		     "\\|")
	  "\\)"
	  "\\(\\[[^}]*\\)?"		; optional []
	  "{\\([^}]*\\)}")
  "Regexp for LaTeX citations. \citetype[optional]{some,keys}.
The clickable part are the keys.")


(defun org-ref-latex-get-key ()
  "Figure out what key the cursor is on."
  (let (start end)
    ;; look back for , or {
    (save-excursion
      (re-search-backward ",\\|{")
      (setq start (+ 1 (point))))

    ;; look forward to , or }
    (save-excursion
      (re-search-forward ",\\|}")
      (setq end (- (point) 1)))
    (buffer-substring start end)))


(defun org-ref-latex-jump-to-bibtex (&optional key)
  "Jump to the KEY at point."
  (let ((results (org-ref-get-bibtex-key-and-file
		  (or key (org-ref-latex-get-key)))))
    (find-file (cdr results))
    (bibtex-search-entry (car results))))


(defun org-ref-latex-click ()
  "Jump to entry clicked on."
  (interactive)
  (helm :sources '(((name . "Actions")
		    (candidates . (("Open Bibtex entry" . org-ref-latex-jump-to-bibtex)
				   ("Bibtex entry menu" . (lambda ()
							    (org-ref-latex-jump-to-bibtex)
							    (org-ref-bibtex-hydra/body)))))
		      (action . (lambda (f)
				  (funcall f)))))))


(defun org-ref-next-latex-cite (&optional limit)
  "Font-lock function to make cites in LaTeX documents clickable."
  (when (re-search-forward org-ref-latex-cite-re limit t)
    (setq font-lock-extra-managed-props (delq 'help-echo font-lock-extra-managed-props))
    (add-text-properties
     (match-beginning 3)
     (match-end 3)
     `(mouse-face
       highlight
       local-map ,(let ((map (copy-keymap latex-mode-map)))
		    (define-key map [mouse-1]
		      'org-ref-latex-click)
		    map)
       help-echo (lambda (window object position)
		   (save-excursion
		     (goto-char position)
		     (let ((s (org-ref-get-citation-string-at-point
			       (org-ref-latex-get-key))))
		       (with-temp-buffer
			 (insert s)
			 (fill-paragraph)
			 (buffer-string)))))))))


(defun org-ref-latex-cite-on ()
  "Add the font-lock on for citations."
  (font-lock-add-keywords
   nil
   '((org-ref-next-latex-cite 3 font-lock-constant-face))))

(add-hook 'latex-mode-hook 'org-ref-latex-cite-on)
(add-hook 'LaTeX-mode-hook 'org-ref-latex-cite-on)

(provide 'org-ref-latex)
;;; org-ref-latex.el ends here
