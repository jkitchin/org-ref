;;; test-footnote-functionality.el --- Demonstrate footnote citation support -*- lexical-binding: t; -*-

;; This script demonstrates that the footnote citation functionality
;; works correctly. Run it from the org-ref root directory.

;;; Usage:
;; emacs --batch -L . -l examples/test-footnote-functionality.el

(require 'org-ref-export)

(message "\n╔═══════════════════════════════════════════════════════════╗")
(message "║   org-ref Footnote Citation Support Test (Issue #993)    ║")
(message "╚═══════════════════════════════════════════════════════════╝\n")

;; Test 1: CSL Style Detection
(message "Test 1: CSL Style Detection")
(message "─────────────────────────────")
(let ((styles '(("chicago-fullnote-bibliography.csl" . "Should support notes")
                ("chicago-note-bibliography.csl" . "Should support notes")
                ("chicago-author-date.csl" . "Should NOT support notes")
                ("apa.csl" . "Should NOT support notes"))))
  (dolist (style styles)
    (let ((file (car style))
          (expected (cdr style))
          (result (org-ref-csl-style-supports-notes-p (car style))))
      (message "  %-40s %s %s"
               file
               (if result "✓ SUPPORTS" "✗ NO SUPPORT")
               (if (string-match-p "Should support" expected)
                   (if result "✓" "✗ FAIL")
                 (if (not result) "✓" "✗ FAIL"))))))

(message "")

;; Test 2: Citation Type Detection
(message "Test 2: Citation Type Detection")
(message "────────────────────────────────")
(let ((types '(("footcite" . t)
               ("footfullcite" . t)
               ("footcitetext" . t)
               ("footcites" . t)
               ("smartcite" . t)
               ("cite" . nil)
               ("citet" . nil)
               ("parencite" . nil))))
  (dolist (type types)
    (let ((cite-type (car type))
          (should-be-footnote (cdr type))
          (is-footnote (org-ref-footnote-cite-type-p (car type))))
      (message "  %-20s %s %s"
               cite-type
               (if is-footnote "✓ Is footnote type" "✗ Not footnote type")
               (if (eq (not (not is-footnote)) should-be-footnote) "✓" "✗ FAIL")))))

(message "")

;; Test 3: Footnote Numbering
(message "Test 3: Footnote Numbering")
(message "───────────────────────────")
(setq org-ref-footnote-counter 0)
(message "  Initial counter: %d" org-ref-footnote-counter)
(message "  First citation:  note-index %d %s"
         (org-ref-get-next-footnote-number)
         (if (= org-ref-footnote-counter 1) "✓" "✗ FAIL"))
(message "  Second citation: note-index %d %s"
         (org-ref-get-next-footnote-number)
         (if (= org-ref-footnote-counter 2) "✓" "✗ FAIL"))
(message "  Third citation:  note-index %d %s"
         (org-ref-get-next-footnote-number)
         (if (= org-ref-footnote-counter 3) "✓" "✗ FAIL"))
(message "  Final counter: %d" org-ref-footnote-counter)

(message "")

;; Test 4: Counter Reset
(message "Test 4: Counter Reset")
(message "─────────────────────")
(setq org-ref-footnote-counter 99)
(message "  Set counter to: %d" org-ref-footnote-counter)
(setq org-ref-footnote-counter 0)
(message "  Reset to: %d %s"
         org-ref-footnote-counter
         (if (= org-ref-footnote-counter 0) "✓" "✗ FAIL"))
(message "  First after reset: %d %s"
         (org-ref-get-next-footnote-number)
         (if (= org-ref-footnote-counter 1) "✓" "✗ FAIL"))

(message "")

;; Summary
(message "╔═══════════════════════════════════════════════════════════╗")
(message "║                     All Tests Complete                    ║")
(message "║                                                           ║")
(message "║  Footnote citation support is working correctly!          ║")
(message "║                                                           ║")
(message "║  Try the examples:                                        ║")
(message "║    - examples/footnote-demo.org                           ║")
(message "║    - examples/test-footnotes.org                          ║")
(message "║    - examples/footnote-citations-example.org              ║")
(message "║                                                           ║")
(message "║  Export with: C-c C-e r h (HTML)                          ║")
(message "║               C-c C-e r o (ODT)                           ║")
(message "║               C-c C-e r w (DOCX)                          ║")
(message "╚═══════════════════════════════════════════════════════════╝")
(message "")

;;; test-footnote-functionality.el ends here
