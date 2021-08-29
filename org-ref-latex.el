;;; org-ref-latex.el --- org-ref functionality for LaTeX files  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2021  John Kitchin

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
;; We use font-lock to add some functionality to the keys.

;;; Code:
(require 'org-ref-core)

(defvar latex-mode-map)
(defvar org-ref-cite-types)


(defvar org-ref-latex-cite-re
  (concat "\\\\\\(?1:" (mapconcat
			(lambda (x)
			  (replace-regexp-in-string "\\*" "\\\\*" x))
			org-ref-cite-types
			"\\|")
	  "\\)"
	  "\\(?2:\\[[^}]*\\)?"		; optional []
	  "\\(3?:\\[[^}]*\\)?"		; optional []
	  "{\\(?4:[^}]*\\)}")           ; group 4 contains the keys
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
    (buffer-substring-no-properties start end)))


(defun org-ref-latex-get-bibliography ()
  "Find bibliographies in the tex file"
  (let ((bibliography '()))
    (goto-char (point-min))
    (while (re-search-forward "\\\\bibliography{\\(?1:.*\\)}" nil t)
      (setq bibliography (append bibliography
				 (mapcar (lambda (f)
					   (concat f ".bib"))
					 (split-string (match-string 1) ",")))))
    (goto-char (point-min))
    (while (re-search-forward "\\\\addbibresource{\\(?1:.*\\)}" nil t)
      (setq bibliography (append bibliography (list (match-string 1)))))

    bibliography))

;;;###autoload
(defun org-ref-latex-debug ()
  (interactive)
  (message-box "%S\n%S\n%S\n%S"
	       (org-ref-latex-get-key)
	       (org-ref-latex-get-bibliography)
	       (org-ref-get-bibtex-key-and-file (org-ref-latex-get-key))
	       (ignore-errors
		 (org-ref-latex-help-echo nil nil (point)))))


(defun org-ref-latex-jump-to-bibtex ()
  "Jump to the KEY at point."
  (interactive)
  (let ((bibtex-completion-bibliography (org-ref-latex-get-bibliography)))
    (bibtex-completion-show-entry (list (org-ref-latex-get-key)))))


(defun org-ref-latex-help-echo (window _object position)
  "Get tool tip for a key in WINDOW for OBJECT at POSITION."
  (with-selected-window window
    (save-excursion
      (goto-char position)
      (let ((bibtex-completion-bibliography (org-ref-latex-get-bibliography)))
	(bibtex-completion-apa-format-reference (org-ref-latex-get-key))))))


(defun org-ref-next-latex-cite (&optional limit)
  "Font-lock function to make cites in LaTeX documents clickable."
  (when (re-search-forward org-ref-latex-cite-re limit t)
    (setq font-lock-extra-managed-props (delq 'help-echo font-lock-extra-managed-props))
    (add-text-properties
     (match-beginning 4)
     (match-end 4)
     `(mouse-face
       highlight
       local-map ,(let ((map (copy-keymap latex-mode-map)))
		    (define-key map [mouse-1]
		      #'org-ref-latex-jump-to-bibtex)
		    map)
       help-echo org-ref-latex-help-echo))))


(defun org-ref-latex-cite-on ()
  "Add the font-lock on for citations."
  (font-lock-add-keywords
   'latex-mode
   '((org-ref-next-latex-cite 4 font-lock-constant-face))))

(add-hook 'latex-mode-hook 'org-ref-latex-cite-on)


(provide 'org-ref-latex)
;;; org-ref-latex.el ends here
