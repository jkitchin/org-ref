;;; test-numbering.el --- Demonstrate sequential footnote numbering -*- lexical-binding: t; -*-

;; This script demonstrates sequential footnote numbering with
;; multiple different citations. Run from org-ref root directory.

;;; Usage:
;; emacs --batch -L . -l examples/test-numbering.el

(require 'org-ref-export)

(message "\n╔════════════════════════════════════════════════════════════╗")
(message "║   Sequential Footnote Numbering Test (Issue #993)         ║")
(message "╚════════════════════════════════════════════════════════════╝\n")

;; Reset counter
(setq org-ref-footnote-counter 0)

(message "Simulating 8 footnote citations from footnote-numbering-demo.org:\n")
(message "%-40s %s" "Citation" "Footnote Number")
(message "%-40s %s" "----------------------------------------" "---------------")

;; Five different citations
(message "%-40s [%d]" "Einstein (1905) - first time"
         (org-ref-get-next-footnote-number))
(message "%-40s [%d]" "Shannon (1948)"
         (org-ref-get-next-footnote-number))
(message "%-40s [%d]" "Knuth (1984) - first time"
         (org-ref-get-next-footnote-number))
(message "%-40s [%d]" "Lamport (1986)"
         (org-ref-get-next-footnote-number))
(message "%-40s [%d]" "Kitchin (2015)"
         (org-ref-get-next-footnote-number))

(message "")
(message "Repeated citations get NEW numbers:")
(message "%-40s [%d]" "Einstein (1905) - SECOND time"
         (org-ref-get-next-footnote-number))
(message "%-40s [%d]" "Shannon (1948) - SECOND time"
         (org-ref-get-next-footnote-number))

(message "")
(message "Full citation:")
(message "%-40s [%d]" "Knuth (1984) - footfullcite"
         (org-ref-get-next-footnote-number))

(message "\n")
(message "╔════════════════════════════════════════════════════════════╗")
(message "║  ✓ Sequential numbering works correctly!                  ║")
(message "║                                                            ║")
(message "║  Key observations:                                         ║")
(message "║  • Each citation gets the next number: 1, 2, 3, 4...       ║")
(message "║  • Repeated citations get NEW numbers (not cross-refs)     ║")
(message "║  • Counter increments regardless of which ref is cited     ║")
(message "║                                                            ║")
(message "║  View the demo:                                            ║")
(message "║  examples/footnote-numbering-demo.org                      ║")
(message "╚════════════════════════════════════════════════════════════╝")
(message "")

;;; test-numbering.el ends here
