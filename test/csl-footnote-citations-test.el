;;; csl-footnote-citations-test.el --- Tests for CSL footnote citation support -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for footnote citation support in CSL export (issue #993)
;; These tests verify that citation types like footcite, footfullcite, etc.
;; are properly handled when exporting to non-LaTeX formats using citeproc-el.

;;; Code:

(require 'ert)

;; Try to load org-ref-export, but tests can still run without it for pure functions
(condition-case nil
    (require 'org-ref-export)
  (error nil))

;;; Test 1: Footnote citation type detection

(ert-deftest test-org-ref-footnote-cite-type-p-footcite ()
  "Test that footcite is detected as a footnote citation type."
  (should (org-ref-footnote-cite-type-p "footcite")))

(ert-deftest test-org-ref-footnote-cite-type-p-footfullcite ()
  "Test that footfullcite is detected as a footnote citation type."
  (should (org-ref-footnote-cite-type-p "footfullcite")))

(ert-deftest test-org-ref-footnote-cite-type-p-footcitetext ()
  "Test that footcitetext is detected as a footnote citation type."
  (should (org-ref-footnote-cite-type-p "footcitetext")))

(ert-deftest test-org-ref-footnote-cite-type-p-footcites ()
  "Test that footcites is detected as a footnote citation type."
  (should (org-ref-footnote-cite-type-p "footcites")))

(ert-deftest test-org-ref-footnote-cite-type-p-smartcite ()
  "Test that smartcite is detected as a footnote citation type."
  (should (org-ref-footnote-cite-type-p "smartcite")))

(ert-deftest test-org-ref-footnote-cite-type-p-regular-cite ()
  "Test that regular cite is NOT a footnote citation type."
  (should-not (org-ref-footnote-cite-type-p "cite")))

(ert-deftest test-org-ref-footnote-cite-type-p-citet ()
  "Test that citet is NOT a footnote citation type."
  (should-not (org-ref-footnote-cite-type-p "citet")))

(ert-deftest test-org-ref-footnote-cite-type-p-parencite ()
  "Test that parencite is NOT a footnote citation type."
  (should-not (org-ref-footnote-cite-type-p "parencite")))


;;; Test 2: Footnote numbering

(ert-deftest test-org-ref-footnote-counter-initialization ()
  "Test that footnote counter starts at 0 or 1."
  (let ((org-ref-footnote-counter 0))
    (should (numberp org-ref-footnote-counter))
    (should (>= org-ref-footnote-counter 0))))

(ert-deftest test-org-ref-get-next-footnote-number ()
  "Test that footnote numbers increment sequentially."
  (let ((org-ref-footnote-counter 0))
    (should (= 1 (org-ref-get-next-footnote-number)))
    (should (= 2 (org-ref-get-next-footnote-number)))
    (should (= 3 (org-ref-get-next-footnote-number)))))


;;; Test 3: CSL style note support detection

(ert-deftest test-org-ref-csl-style-supports-notes-p-chicago-fullnote ()
  "Test that chicago-fullnote style is detected as supporting notes."
  ;; This should detect note-capable styles
  (should (org-ref-csl-style-supports-notes-p "chicago-fullnote-bibliography.csl")))

(ert-deftest test-org-ref-csl-style-supports-notes-p-chicago-note ()
  "Test that chicago-note style is detected as supporting notes."
  (should (org-ref-csl-style-supports-notes-p "chicago-note-bibliography.csl")))

(ert-deftest test-org-ref-csl-style-supports-notes-p-author-date ()
  "Test that chicago-author-date is NOT detected as supporting notes."
  (should-not (org-ref-csl-style-supports-notes-p "chicago-author-date.csl")))

(ert-deftest test-org-ref-csl-style-supports-notes-p-apa ()
  "Test that APA style is NOT detected as supporting notes."
  (should-not (org-ref-csl-style-supports-notes-p "apa.csl")))


;;; Test 4: Citation data structure with note-index

(ert-deftest test-org-ref-citation-with-note-index ()
  "Test that footnote citations get a note-index parameter."
  (with-temp-buffer
    (org-mode)
    (insert "#+csl-style: chicago-fullnote-bibliography.csl\n")
    (insert "#+bibliography: test.bib\n\n")
    (insert "Some text [[footcite:&smith2020]].\n")
    (insert "More text [[footcite:&jones2021]].\n")
    (insert "\nbibliography:test.bib\n")

    ;; This test verifies the internal data structure
    ;; The first footcite should have :note-index 1
    ;; The second footcite should have :note-index 2
    (let* ((org-ref-footnote-counter 0)
           (citations (org-ref-get-cite-links))
           (first-cite (car citations))
           (second-cite (cadr citations)))

      ;; Should have 2 citations
      (should (= 2 (length citations)))

      ;; First citation should be footcite type
      (should (string= "footcite" (org-element-property :type first-cite)))

      ;; When we process this citation, it should get note-index 1
      ;; We'll test this by checking what gets passed to citeproc
      ;; This is a structural test - implementation will make this pass
      (should (= 1 (org-ref-get-citation-note-index first-cite)))
      (should (= 2 (org-ref-get-citation-note-index second-cite))))))


;;; Test 5: Export output with footnote citations

(ert-deftest test-org-ref-export-footcite-to-html ()
  "Test that footcite exports correctly to HTML."
  (skip-unless (featurep 'citeproc))
  (with-temp-buffer
    (org-mode)
    (insert "#+csl-style: chicago-fullnote-bibliography-16th-edition.csl\n")
    (insert "#+bibliography: "
            (expand-file-name "test/test.bib"
                              (file-name-directory (locate-library "org-ref")))
            "\n\n")
    (insert "Some text [[footcite:&smith2020]].\n\n")
    (insert "bibliography:"
            (expand-file-name "test/test.bib"
                              (file-name-directory (locate-library "org-ref")))
            "\n")

    ;; Export to HTML (using org-ref CSL preprocessor)
    (let* ((org-export-before-parsing-hook '(org-ref-csl-preprocess-buffer))
           (html-output (org-export-as 'html)))

      ;; The output should contain a footnote reference
      ;; The exact format depends on how we implement it, but it should:
      ;; 1. Have a footnote number or marker
      ;; 2. Have the citation content in a footnote
      (should (string-match-p "footnote\\|<sup>\\|\\[fn:" html-output)))))


;;; Test 6: Warning for incompatible CSL styles

(ert-deftest test-org-ref-warn-on-footnote-cite-without-note-style ()
  "Test that using footcite with non-note CSL style produces a warning."
  (skip-unless (featurep 'citeproc))
  (with-temp-buffer
    (org-mode)
    ;; Use a non-note style (author-date)
    (insert "#+csl-style: chicago-author-date-16th-edition.csl\n")
    (insert "#+bibliography: test.bib\n\n")
    (insert "Some text [[footcite:&smith2020]].\n")
    (insert "\nbibliography:test.bib\n")

    ;; Should produce a warning
    (let ((warning-produced nil))
      (cl-letf (((symbol-function 'warn)
                 (lambda (&rest _args) (setq warning-produced t))))

        ;; Try to process the buffer
        (condition-case err
            (org-ref-process-buffer 'html)
          (error nil))

        ;; Should have warned about incompatible style
        (should warning-produced)))))


(provide 'csl-footnote-citations-test)
;;; csl-footnote-citations-test.el ends here
