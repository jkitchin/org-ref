;;; multi-file-refs-test.el --- Tests for multi-file references -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for multi-file reference support in org-ref (Issue #1021).
;; Tests label collection from included files via #+INCLUDE directives.

;;; Code:

(require 'ert)
(require 'org-ref-ref-links)

(defvar multi-file-test-dir
  (expand-file-name "multi-file-refs"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing multi-file test documents.")

(ert-deftest test-org-ref-get-included-files ()
  "Test that we can discover files included via #+INCLUDE directives."
  (let ((main-file (expand-file-name "main.org" multi-file-test-dir)))
    (with-current-buffer (find-file-noselect main-file)
      (let ((included-files (org-ref-get-included-files)))
        ;; Should find both chapter1.org and chapter2.org
        (should (= 2 (length included-files)))
        ;; Check that both files are in the list
        (should (cl-some (lambda (f) (string-match-p "chapter1\\.org$" f)) included-files))
        (should (cl-some (lambda (f) (string-match-p "chapter2\\.org$" f)) included-files))
        ;; Files should be absolute paths
        (should (cl-every #'file-name-absolute-p included-files))))))

(ert-deftest test-org-ref-file-changed-p ()
  "Test timestamp-based change detection."
  ;; Clear the timestamp cache
  (clrhash org-ref-file-timestamps)

  (let ((test-file (expand-file-name "chapter1.org" multi-file-test-dir)))
    ;; First check should return t (file not yet scanned)
    (should (org-ref-file-changed-p test-file))

    ;; Mark file as scanned
    (org-ref-mark-file-scanned test-file)

    ;; Second check should return nil (file hasn't changed)
    (should-not (org-ref-file-changed-p test-file))

    ;; Clear cache again
    (clrhash org-ref-file-timestamps)))

(ert-deftest test-org-ref-scan-buffer-for-labels ()
  "Test that we can scan a buffer for labels."
  (let ((chapter1-file (expand-file-name "chapter1.org" multi-file-test-dir)))
    (with-current-buffer (find-file-noselect chapter1-file)
      (let ((labels (org-ref-scan-buffer-for-labels)))
        ;; Should find multiple labels
        (should (> (length labels) 0))

        ;; Check for specific labels we expect
        (should (assoc "fig:sample-figure" labels))
        (should (assoc "eq:important" labels))
        (should (assoc "tab:data" labels))
        (should (assoc "sec:introduction" labels))
        (should (assoc "eq:theory" labels))

        ;; Labels should have context
        (let ((fig-label (assoc "fig:sample-figure" labels)))
          (should (stringp (cdr fig-label)))
          (should (> (length (cdr fig-label)) 0)))))))

(ert-deftest test-org-ref-get-labels-single-file ()
  "Test single-file mode still works (backward compatibility)."
  (let ((chapter1-file (expand-file-name "chapter1.org" multi-file-test-dir))
        (org-ref-enable-multi-file-references nil))
    (with-current-buffer (find-file-noselect chapter1-file)
      ;; Clear buffer-local cache
      (setq org-ref-label-cache nil
            org-ref-buffer-chars-modified-tick nil)

      (let ((labels (org-ref-get-labels)))
        ;; Should find labels from this file
        (should (assoc "fig:sample-figure" labels))
        (should (assoc "eq:important" labels))

        ;; Should NOT find labels from chapter2 (not included)
        (should-not (assoc "fig:another-figure" labels))
        (should-not (assoc "eq:result" labels))))))

(ert-deftest test-org-ref-get-labels-multi-file-basic ()
  "Test that multi-file mode finds labels from included files."
  (let ((main-file (expand-file-name "main.org" multi-file-test-dir))
        (org-ref-enable-multi-file-references t))
    ;; Clear caches
    (clrhash org-ref-project-label-cache)
    (clrhash org-ref-file-timestamps)

    (with-current-buffer (find-file-noselect main-file)
      (let ((labels (org-ref-get-labels)))
        ;; Should find labels from chapter1.org
        (should (assoc "fig:sample-figure" labels))
        (should (assoc "eq:important" labels))
        (should (assoc "tab:data" labels))
        (should (assoc "sec:introduction" labels))
        (should (assoc "eq:theory" labels))

        ;; Should find labels from chapter2.org
        (should (assoc "fig:another-figure" labels))
        (should (assoc "eq:result" labels))
        (should (assoc "tab:results" labels))
        (should (assoc "sec:analysis" labels))))))

(ert-deftest test-org-ref-multi-file-cache-efficiency ()
  "Test that cached labels are reused when files haven't changed."
  (let ((main-file (expand-file-name "main.org" multi-file-test-dir))
        (org-ref-enable-multi-file-references t))
    ;; Clear caches
    (clrhash org-ref-project-label-cache)
    (clrhash org-ref-file-timestamps)

    (with-current-buffer (find-file-noselect main-file)
      ;; First call - should scan all files
      (let ((labels1 (org-ref-get-labels)))
        (should (> (length labels1) 0))

        ;; Second call - should use cache (files haven't changed)
        (let ((labels2 (org-ref-get-labels)))
          ;; Should get same labels
          (should (= (length labels1) (length labels2)))

          ;; Verify all labels are still there
          (dolist (label labels1)
            (should (assoc (car label) labels2))))))))

(ert-deftest test-org-ref-multi-file-from-chapter ()
  "Test multi-file references from within an included file."
  (let ((chapter2-file (expand-file-name "chapter2.org" multi-file-test-dir))
        (org-ref-enable-multi-file-references nil))
    ;; When multi-file mode is disabled, chapter2 should only see its own labels
    (with-current-buffer (find-file-noselect chapter2-file)
      (setq org-ref-label-cache nil
            org-ref-buffer-chars-modified-tick nil)

      (let ((labels (org-ref-get-labels)))
        ;; Should find its own labels
        (should (assoc "fig:another-figure" labels))
        (should (assoc "eq:result" labels))

        ;; Should NOT find labels from chapter1 (multi-file mode disabled)
        (should-not (assoc "eq:important" labels))))))

(ert-deftest test-org-ref-scan-file-for-labels ()
  "Test scanning an external file for labels."
  (let* ((chapter1-file (expand-file-name "chapter1.org" multi-file-test-dir))
         (labels (org-ref-scan-file-for-labels chapter1-file)))

    ;; Should find labels without opening the file in a regular buffer
    (should (assoc "fig:sample-figure" labels))
    (should (assoc "eq:important" labels))
    (should (assoc "tab:data" labels))))

(ert-deftest test-org-ref-multi-file-no-duplicates ()
  "Test that duplicate labels across files are deduplicated."
  (let ((main-file (expand-file-name "main.org" multi-file-test-dir))
        (org-ref-enable-multi-file-references t))
    ;; Clear caches
    (clrhash org-ref-project-label-cache)
    (clrhash org-ref-file-timestamps)

    (with-current-buffer (find-file-noselect main-file)
      (let ((labels (org-ref-get-labels)))
        ;; Count occurrences of each label
        (dolist (label labels)
          (let* ((label-name (car label))
                 (count (cl-count label-name labels :key #'car :test #'string=)))
            ;; Each label should appear only once
            (should (= count 1))))))))

(ert-deftest test-org-ref-find-label-in-file ()
  "Test finding a label in an external file."
  (let ((chapter1-file (expand-file-name "chapter1.org" multi-file-test-dir)))
    ;; Search for a label that exists
    (let ((pos (org-ref-find-label-in-file "eq:important" chapter1-file)))
      (should pos)
      (should (numberp pos))
      (should (> pos 0)))

    ;; Search for a label that doesn't exist
    (let ((pos (org-ref-find-label-in-file "nonexistent-label" chapter1-file)))
      (should-not pos))))

(ert-deftest test-org-ref-jump-to-local-label ()
  "Test jumping to a label in the current file."
  (let ((chapter1-file (expand-file-name "chapter1.org" multi-file-test-dir))
        (org-ref-enable-multi-file-references nil))
    (with-current-buffer (find-file-noselect chapter1-file)
      ;; Go to the beginning of the buffer
      (goto-char (point-min))

      ;; Simulate clicking on a ref link by setting the path
      (org-ref-ref-jump-to "eq:important")

      ;; Should have jumped to the label
      (let ((context (buffer-substring (line-beginning-position) (line-end-position))))
        (should (string-match-p "eq:important" context))))))

(ert-deftest test-org-ref-jump-to-included-file ()
  "Test jumping to a label in an included file."
  (let ((main-file (expand-file-name "main.org" multi-file-test-dir))
        (org-ref-enable-multi-file-references t))
    (with-current-buffer (find-file-noselect main-file)
      ;; Go to the beginning of the buffer
      (goto-char (point-min))

      ;; Try to jump to a label that's in chapter1.org
      (org-ref-ref-jump-to "eq:important")

      ;; Should have opened chapter1.org and jumped to the label
      (should (string-match-p "chapter1\\.org" (buffer-file-name)))
      (let ((context (buffer-substring (line-beginning-position) (line-end-position))))
        (should (string-match-p "eq:important" context)))

      ;; Clean up - go back to main.org
      (find-file main-file))))

(ert-deftest test-org-ref-jump-to-second-included-file ()
  "Test jumping to a label in the second included file."
  (let ((main-file (expand-file-name "main.org" multi-file-test-dir))
        (org-ref-enable-multi-file-references t))
    (with-current-buffer (find-file-noselect main-file)
      ;; Go to the beginning of the buffer
      (goto-char (point-min))

      ;; Try to jump to a label that's in chapter2.org
      (org-ref-ref-jump-to "eq:result")

      ;; Should have opened chapter2.org and jumped to the label
      (should (string-match-p "chapter2\\.org" (buffer-file-name)))
      (let ((context (buffer-substring (line-beginning-position) (line-end-position))))
        (should (string-match-p "eq:result" context)))

      ;; Clean up - go back to main.org
      (find-file main-file))))

;; Performance benchmark (not a test failure if slow, just informative)
(ert-deftest test-org-ref-multi-file-performance ()
  "Benchmark multi-file label collection performance."
  (let ((main-file (expand-file-name "main.org" multi-file-test-dir))
        (org-ref-enable-multi-file-references t))
    ;; Clear caches to measure full scan time
    (clrhash org-ref-project-label-cache)
    (clrhash org-ref-file-timestamps)

    (with-current-buffer (find-file-noselect main-file)
      ;; First call - full scan
      (let ((start-time (current-time)))
        (org-ref-get-labels)
        (let ((elapsed (float-time (time-subtract (current-time) start-time))))
          ;; Should complete in reasonable time (< 1 second for 3 small files)
          (message "Multi-file scan time: %.3f seconds" elapsed)
          (should (< elapsed 1.0))))

      ;; Second call - should be much faster (cached)
      (let ((start-time (current-time)))
        (org-ref-get-labels)
        (let ((elapsed (float-time (time-subtract (current-time) start-time))))
          ;; Cached call should be very fast (< 0.1 seconds)
          (message "Cached scan time: %.3f seconds" elapsed)
          (should (< elapsed 0.1)))))))

(provide 'multi-file-refs-test)
;;; multi-file-refs-test.el ends here
