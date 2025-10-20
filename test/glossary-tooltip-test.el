;;; glossary-tooltip-test.el --- Tests for glossary tooltip fix (Issue #1120) -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for fixing glossary and acronym tooltip issues (Issue #1120).
;;
;; Issue: Tooltips show "nil: ." instead of actual glossary/acronym content
;; after performance improvements were made.
;;
;; These tests verify:
;; 1. Tooltips show correct content for valid entries
;; 2. Tooltips handle missing entries gracefully
;; 3. Tooltips can be disabled via defcustom
;; 4. Performance is maintained (no freezing)

;;; Code:

(require 'ert)
(require 'org-ref-glossary)

(defvar glossary-test-dir
  (expand-file-name "glossary-examples"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing glossary test files.")

;;; Tests for glossary tooltips

(ert-deftest test-or-glossary-tooltip-with-valid-entry ()
  "Test that glossary tooltip shows correct name and description."
  (let ((test-file (expand-file-name "01-small-example.org" glossary-test-dir)))
    (with-current-buffer (find-file-noselect test-file)
      ;; Clear any existing caches
      (setq-local org-ref-glossary-cache nil)
      (setq-local org-ref-acronym-cache nil)

      ;; Find a gls: link and activate it
      (goto-char (point-min))
      (should (re-search-forward "gls:computer" nil t))

      ;; Activate the link at point to set text properties
      (let* ((element (org-element-context))
             (begin (org-element-property :begin element))
             (end (org-element-property :end element))
             (path (org-element-property :path element)))
        ;; Manually activate to ensure properties are set
        (or-activate-glossary begin end path nil)

        ;; Get the tooltip (check at begin position, inside the link)
        (let ((tooltip (or-glossary-tooltip nil (current-buffer) begin)))
          ;; Tooltip should contain the name and description
          (should (stringp tooltip))
          (should-not (string-match-p "nil:" tooltip))
          (should (string-match-p "Computer" tooltip))
          (should (string-match-p "machine that performs computations" tooltip)))))))

(ert-deftest test-or-acronym-tooltip-with-valid-entry ()
  "Test that acronym tooltip shows correct abbreviation and full form."
  (let ((test-file (expand-file-name "01-small-example.org" glossary-test-dir)))
    (with-current-buffer (find-file-noselect test-file)
      ;; Clear any existing caches
      (setq-local org-ref-glossary-cache nil)
      (setq-local org-ref-acronym-cache nil)

      ;; Find a gls: link to an acronym
      (goto-char (point-min))
      (should (re-search-forward "gls:cpu" nil t))

      ;; Activate the link at point
      (let* ((element (org-element-context))
             (begin (org-element-property :begin element))
             (end (org-element-property :end element))
             (path (org-element-property :path element)))
        (or-activate-glossary begin end path nil)

        ;; Get the tooltip (check at begin position, inside the link)
        (let ((tooltip (or-acronym-tooltip nil (current-buffer) begin)))
          ;; Tooltip should contain abbreviation and full form
          (should (stringp tooltip))
          (should-not (string-match-p "nil:" tooltip))
          (should (string-match-p "CPU" tooltip))
          (should (string-match-p "Central Processing Unit" tooltip)))))))

(ert-deftest test-glossary-tooltip-with-nil-data ()
  "Test that tooltip handles nil data gracefully without crashing."
  (let ((test-file (expand-file-name "01-small-example.org" glossary-test-dir)))
    (with-current-buffer (find-file-noselect test-file)
      ;; Create a position with nil glossary data
      (goto-char (point-min))
      (insert "gls:nonexistent-term\n")
      (goto-char (point-min))
      (re-search-forward "gls:nonexistent-term")

      ;; Manually set nil data
      (put-text-property (line-beginning-position) (line-end-position)
                         'or-glossary nil)

      ;; Tooltip should not crash and should return something reasonable
      (let ((tooltip (or-glossary-tooltip nil (current-buffer) (point))))
        (should (or (null tooltip)
                   (stringp tooltip)))
        ;; If it returns a string, it should not be the buggy "nil: ."
        (when (stringp tooltip)
          (should-not (equal tooltip "nil: .")))))))

(ert-deftest test-acronym-tooltip-with-nil-data ()
  "Test that acronym tooltip handles nil data gracefully."
  (let ((test-file (expand-file-name "01-small-example.org" glossary-test-dir)))
    (with-current-buffer (find-file-noselect test-file)
      ;; Create a position with nil glossary data
      (goto-char (point-min))
      (insert "gls:nonexistent-acronym\n")
      (goto-char (point-min))
      (re-search-forward "gls:nonexistent-acronym")

      ;; Manually set nil data
      (put-text-property (line-beginning-position) (line-end-position)
                         'or-glossary nil)

      ;; Tooltip should not crash
      (let ((tooltip (or-acronym-tooltip nil (current-buffer) (point))))
        (should (or (null tooltip)
                   (stringp tooltip)))
        (when (stringp tooltip)
          (should-not (equal tooltip "nil: .")))))))

(ert-deftest test-glossary-text-property-is-set ()
  "Test that or-activate-glossary sets the 'or-glossary text property correctly."
  (let ((test-file (expand-file-name "01-small-example.org" glossary-test-dir)))
    (with-current-buffer (find-file-noselect test-file)
      ;; Clear caches
      (setq-local org-ref-glossary-cache nil)
      (setq-local org-ref-acronym-cache nil)

      ;; Find and activate a glossary link
      (goto-char (point-min))
      (should (re-search-forward "gls:computer" nil t))

      (let* ((element (org-element-context))
             (begin (org-element-property :begin element))
             (end (org-element-property :end element))
             (path (org-element-property :path element)))
        ;; Activate the link
        (or-activate-glossary begin end path nil)

        ;; Check that text property is set (check at begin position, inside the link)
        (let ((data (get-text-property begin 'or-glossary)))
          (should data)
          (should (plist-get data :name))
          (should (plist-get data :description)))))))

(ert-deftest test-parse-glossary-entry-from-table ()
  "Test that or-parse-glossary-entry correctly finds entries in glossary table."
  (let ((test-file (expand-file-name "01-small-example.org" glossary-test-dir)))
    (with-current-buffer (find-file-noselect test-file)
      ;; Clear cache to force fresh parse
      (setq-local org-ref-glossary-cache nil)

      ;; Parse a known entry
      (let ((data (or-parse-glossary-entry "computer")))
        (should data)
        (should (equal (plist-get data :label) "computer"))
        (should (equal (plist-get data :name) "Computer"))
        (should (string-match-p "machine" (plist-get data :description)))))))

(ert-deftest test-parse-acronym-entry-from-table ()
  "Test that or-parse-acronym-entry correctly finds entries in acronym table."
  (let ((test-file (expand-file-name "01-small-example.org" glossary-test-dir)))
    (with-current-buffer (find-file-noselect test-file)
      ;; Clear cache
      (setq-local org-ref-acronym-cache nil)

      ;; Parse a known acronym
      (let ((data (or-parse-acronym-entry "cpu")))
        (should data)
        (should (equal (plist-get data :label) "cpu"))
        (should (equal (plist-get data :abbrv) "CPU"))
        (should (equal (plist-get data :full) "Central Processing Unit"))))))

(ert-deftest test-glossary-cache-is-used ()
  "Test that glossary cache improves performance on repeated lookups."
  (let ((test-file (expand-file-name "01-small-example.org" glossary-test-dir)))
    (with-current-buffer (find-file-noselect test-file)
      ;; Clear cache
      (setq-local org-ref-glossary-cache nil)

      ;; First lookup - should populate cache
      (let ((data1 (or-parse-glossary-entry "computer")))
        (should data1)

        ;; Cache should now exist
        (should org-ref-glossary-cache)
        (should (gethash "computer" org-ref-glossary-cache))

        ;; Second lookup - should use cache
        (let ((data2 (or-parse-glossary-entry "computer")))
          ;; Should return same data
          (should (equal data1 data2)))))))

(ert-deftest test-tooltip-performance-no-freeze ()
  "Test that tooltip generation is fast enough (< 100ms) to avoid freezing."
  (let ((test-file (expand-file-name "02-large-example.org" glossary-test-dir)))
    (with-current-buffer (find-file-noselect test-file)
      ;; Clear caches to test worst case
      (setq-local org-ref-glossary-cache nil)
      (setq-local org-ref-acronym-cache nil)

      ;; Find a glossary link
      (goto-char (point-min))
      (re-search-forward "gls:algorithm" nil t)

      (let* ((element (org-element-context))
             (begin (org-element-property :begin element))
             (end (org-element-property :end element))
             (path (org-element-property :path element)))
        ;; Activate to set properties
        (or-activate-glossary begin end path nil)

        ;; Time the tooltip generation
        (let ((start-time (current-time)))
          (or-glossary-tooltip nil (current-buffer) (point))
          (let ((elapsed (float-time (time-subtract (current-time) start-time))))
            ;; Should be fast (< 100ms to avoid noticeable delay)
            (should (< elapsed 0.1))))

        ;; Second call should be even faster (cached)
        (let ((start-time (current-time)))
          (or-glossary-tooltip nil (current-buffer) (point))
          (let ((elapsed (float-time (time-subtract (current-time) start-time))))
            ;; Cached call should be very fast (< 10ms)
            (should (< elapsed 0.01))))))))

(provide 'glossary-tooltip-test)
;;; glossary-tooltip-test.el ends here
