;;; org-ref-latex-test.el --- Tests for org-ref-latex.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>

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
;; Tests for org-ref-latex.el, specifically for bibliography detection
;; in single-file and multi-file LaTeX projects.

;;; Code:

(require 'ert)
(require 'tex-mode)

;; Load the functions directly from org-ref-latex.el
;; We load the file directly to avoid dependency issues
(let ((latex-file (expand-file-name "org-ref-latex.el"
                                     (file-name-directory
                                      (directory-file-name
                                       (file-name-directory
                                        (or load-file-name (buffer-file-name))))))))
  (unless (fboundp 'org-ref-latex-get-bibliography)
    ;; Load just the functions we need by evaluating the file
    (with-temp-buffer
      (insert-file-contents latex-file)
      (goto-char (point-min))
      ;; Find and eval the helper function
      (when (search-forward "(defun org-ref-latex--scan-buffer-for-bibliography ()" nil t)
        (goto-char (match-beginning 0))
        (eval (read (current-buffer))))
      ;; Find and eval the main function
      (goto-char (point-min))
      (when (search-forward "(defun org-ref-latex-get-bibliography ()" nil t)
        (goto-char (match-beginning 0))
        (eval (read (current-buffer)))))))

(defvar org-ref-latex-test-dir
  (expand-file-name "latex-multifile"
                    (file-name-directory
                     (or load-file-name (buffer-file-name))))
  "Directory containing test LaTeX files.")

(ert-deftest test-org-ref-latex-get-bib-single-bibtex ()
  "Test bibliography extraction from single BibTeX file.
File uses \\bibliography{ref1,ref2} format."
  (let ((test-file (expand-file-name "single-file.tex" org-ref-latex-test-dir)))
    (with-current-buffer (find-file-noselect test-file)
      (unwind-protect
          (let ((result (org-ref-latex-get-bibliography)))
            (should (equal result '("references.bib" "extra-refs.bib"))))
        (kill-buffer)))))

(ert-deftest test-org-ref-latex-get-bib-single-biblatex ()
  "Test bibliography extraction from single BibLaTeX file.
File uses \\addbibresource{file.bib} format with multiple resources."
  (let ((test-file (expand-file-name "biblatex-single.tex" org-ref-latex-test-dir)))
    (with-current-buffer (find-file-noselect test-file)
      (unwind-protect
          (let ((result (org-ref-latex-get-bibliography)))
            (should (equal result '("bibliography.bib" "supplements.bib"))))
        (kill-buffer)))))

(ert-deftest test-org-ref-latex-get-bib-main-file ()
  "Test bibliography extraction from main file in multi-file project.
Main file contains the \\bibliography command."
  (let ((test-file (expand-file-name "main.tex" org-ref-latex-test-dir)))
    (with-current-buffer (find-file-noselect test-file)
      (unwind-protect
          (let ((result (org-ref-latex-get-bibliography)))
            (should (equal result '("references.bib" "database.bib"))))
        (kill-buffer)))))

(ert-deftest test-org-ref-latex-get-bib-chapter-uses-master ()
  "Test bibliography extraction from chapter file using TeX-master.
Chapter file has no \\bibliography command but should find it in master file.
This is the core functionality for issue #1047."
  (let ((test-file (expand-file-name "chapter1.tex" org-ref-latex-test-dir)))
    (with-current-buffer (find-file-noselect test-file)
      (unwind-protect
          (progn
            ;; Set TeX-master manually (file-local variables don't work in batch mode)
            (setq-local TeX-master "main")
            (should (equal TeX-master "main"))
            ;; Test that we find bibliography from main.tex
            (let ((result (org-ref-latex-get-bibliography)))
              (should (equal result '("references.bib" "database.bib")))))
        (kill-buffer)))))

(ert-deftest test-org-ref-latex-get-bib-biblatex-chapter ()
  "Test bibliography from BibLaTeX chapter using TeX-master.
Chapter uses master file with \\addbibresource commands."
  (let ((test-file (expand-file-name "biblatex-chapter.tex" org-ref-latex-test-dir)))
    (with-current-buffer (find-file-noselect test-file)
      (unwind-protect
          (progn
            ;; Set TeX-master manually (file-local variables don't work in batch mode)
            (setq-local TeX-master "biblatex-main")
            (should (equal TeX-master "biblatex-main"))
            ;; Test that we find bibliography from biblatex-main.tex
            (let ((result (org-ref-latex-get-bibliography)))
              (should (equal result '("bibliography.bib" "extra-sources.bib")))))
        (kill-buffer)))))

(ert-deftest test-org-ref-latex-get-bib-no-bibliography ()
  "Test behavior when no bibliography exists and no valid master.
Should return empty list, not error."
  (with-temp-buffer
    (latex-mode)
    ;; Set TeX-master to t (current file is master)
    (setq-local TeX-master t)
    (insert "\\documentclass{article}\n")
    (insert "\\begin{document}\n")
    (insert "No bibliography here.\n")
    (insert "\\end{document}\n")
    (let ((result (org-ref-latex-get-bibliography)))
      (should (equal result '())))))

(provide 'org-ref-latex-test)
;;; org-ref-latex-test.el ends here
