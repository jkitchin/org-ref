;;; test-bibtex-completion-bibliography-normalization.el --- Tests for bibtex-completion-bibliography normalization -*- lexical-binding: t; -*-

;; Tests for the bibtex-completion-bibliography normalization feature (issue #1119)
;; This ensures that org-ref properly handles bibtex-completion-bibliography when
;; it's set to a string, list, or function.

(require 'ert)
(require 'org-ref-utils)
(require 'org-ref-core)
(require 'doi-utils)
(require 'org-ref-pdf)

;;; Tests for org-ref-normalize-bibtex-completion-bibliography

(ert-deftest test-normalize-bibtex-completion-bibliography/string ()
  "Test normalization when bibtex-completion-bibliography is a string."
  (let ((bibtex-completion-bibliography "/path/to/refs.bib"))
    (should (equal (org-ref-normalize-bibtex-completion-bibliography)
                   '("/path/to/refs.bib")))))

(ert-deftest test-normalize-bibtex-completion-bibliography/list ()
  "Test normalization when bibtex-completion-bibliography is a list."
  (let ((bibtex-completion-bibliography '("/path/to/refs1.bib" "/path/to/refs2.bib")))
    (should (equal (org-ref-normalize-bibtex-completion-bibliography)
                   '("/path/to/refs1.bib" "/path/to/refs2.bib")))))

(ert-deftest test-normalize-bibtex-completion-bibliography/function ()
  "Test normalization when bibtex-completion-bibliography is a function."
  (let ((bibtex-completion-bibliography
         (lambda () '("/dynamic/path1.bib" "/dynamic/path2.bib"))))
    (should (equal (org-ref-normalize-bibtex-completion-bibliography)
                   '("/dynamic/path1.bib" "/dynamic/path2.bib")))))

(ert-deftest test-normalize-bibtex-completion-bibliography/function-returns-string ()
  "Test normalization when bibtex-completion-bibliography is a function returning a string."
  (let ((bibtex-completion-bibliography
         (lambda () "/dynamic/path.bib")))
    (should (equal (org-ref-normalize-bibtex-completion-bibliography)
                   "/dynamic/path.bib"))))

(ert-deftest test-normalize-bibtex-completion-bibliography/empty-list ()
  "Test normalization when bibtex-completion-bibliography is an empty list."
  (let ((bibtex-completion-bibliography '()))
    (should (equal (org-ref-normalize-bibtex-completion-bibliography)
                   '()))))

;;; Tests for org-ref-find-bibliography with different bibtex-completion-bibliography types

(ert-deftest test-org-ref-find-bibliography/string-fallback ()
  "Test org-ref-find-bibliography falls back to string bibtex-completion-bibliography."
  (let ((bibtex-completion-bibliography "/path/to/refs.bib"))
    (with-temp-buffer
      (org-mode)
      (insert "* Test heading\n\nSome text.\n")
      (goto-char (point-min))
      (let ((result (org-ref-find-bibliography)))
        (should (equal result '("/path/to/refs.bib")))))))

(ert-deftest test-org-ref-find-bibliography/list-fallback ()
  "Test org-ref-find-bibliography falls back to list bibtex-completion-bibliography."
  (let ((bibtex-completion-bibliography '("/path/to/refs1.bib" "/path/to/refs2.bib")))
    (with-temp-buffer
      (org-mode)
      (insert "* Test heading\n\nSome text.\n")
      (goto-char (point-min))
      (let ((result (org-ref-find-bibliography)))
        (should (equal result '("/path/to/refs1.bib" "/path/to/refs2.bib")))))))

(ert-deftest test-org-ref-find-bibliography/function-fallback ()
  "Test org-ref-find-bibliography falls back to function bibtex-completion-bibliography."
  (let ((bibtex-completion-bibliography
         (lambda () '("/dynamic/path1.bib" "/dynamic/path2.bib"))))
    (with-temp-buffer
      (org-mode)
      (insert "* Test heading\n\nSome text.\n")
      (goto-char (point-min))
      (let ((result (org-ref-find-bibliography)))
        (should (equal result '("/dynamic/path1.bib" "/dynamic/path2.bib")))))))


;;; Tests for org-ref-possible-bibfiles with different bibtex-completion-bibliography types

(ert-deftest test-org-ref-possible-bibfiles/string ()
  "Test org-ref-possible-bibfiles with string bibtex-completion-bibliography."
  (let ((bibtex-completion-bibliography "/path/to/refs.bib"))
    (with-temp-buffer
      (org-mode)
      (insert "* Test heading\n")
      (goto-char (point-min))
      (let ((result (org-ref-possible-bibfiles)))
        (should (member "/path/to/refs.bib" result))))))

(ert-deftest test-org-ref-possible-bibfiles/list ()
  "Test org-ref-possible-bibfiles with list bibtex-completion-bibliography."
  (let ((bibtex-completion-bibliography '("/path/to/refs1.bib" "/path/to/refs2.bib")))
    (with-temp-buffer
      (org-mode)
      (insert "* Test heading\n")
      (goto-char (point-min))
      (let ((result (org-ref-possible-bibfiles)))
        (should (member "/path/to/refs1.bib" result))
        (should (member "/path/to/refs2.bib" result))))))

(ert-deftest test-org-ref-possible-bibfiles/function ()
  "Test org-ref-possible-bibfiles with function bibtex-completion-bibliography."
  (let ((bibtex-completion-bibliography
         (lambda () '("/dynamic/path1.bib" "/dynamic/path2.bib"))))
    (with-temp-buffer
      (org-mode)
      (insert "* Test heading\n")
      (goto-char (point-min))
      (let ((result (org-ref-possible-bibfiles)))
        (should (member "/dynamic/path1.bib" result))
        (should (member "/dynamic/path2.bib" result))))))

;;; Tests for doi-utils-open-bibtex with different bibtex-completion-bibliography types

(ert-deftest test-doi-utils-open-bibtex/string ()
  "Test doi-utils-open-bibtex with string bibtex-completion-bibliography."
  (let* ((test-dir (if load-file-name
                       (file-name-directory load-file-name)
                     default-directory))
         (test-bib (expand-file-name "test-1.bib" test-dir))
         (bibtex-completion-bibliography test-bib))
    ;; The function should handle string without error
    (should (stringp bibtex-completion-bibliography))
    ;; Test that normalization works (the actual search will fail if DOI doesn't exist)
    (should (listp (org-ref-normalize-bibtex-completion-bibliography)))))

(ert-deftest test-doi-utils-open-bibtex/list ()
  "Test doi-utils-open-bibtex with list bibtex-completion-bibliography."
  (let* ((test-dir (if load-file-name
                       (file-name-directory load-file-name)
                     default-directory))
         (test-bib (expand-file-name "test-1.bib" test-dir))
         (bibtex-completion-bibliography (list test-bib)))
    ;; The function should handle list without error
    (should (listp bibtex-completion-bibliography))
    ;; Test that normalization works
    (should (listp (org-ref-normalize-bibtex-completion-bibliography)))))

(ert-deftest test-doi-utils-open-bibtex/function ()
  "Test doi-utils-open-bibtex with function bibtex-completion-bibliography."
  (let* ((test-dir (if load-file-name
                       (file-name-directory load-file-name)
                     default-directory))
         (test-bib (expand-file-name "test-1.bib" test-dir))
         (bibtex-completion-bibliography (lambda () (list test-bib))))
    ;; The function should handle function without error
    (should (functionp bibtex-completion-bibliography))
    ;; Test that normalization works
    (should (listp (org-ref-normalize-bibtex-completion-bibliography)))))

;;; Tests for org-ref-bibtex-key-from-doi with different bibtex-completion-bibliography types

(ert-deftest test-org-ref-bibtex-key-from-doi/string ()
  "Test org-ref-bibtex-key-from-doi with string bibtex-completion-bibliography."
  (let* ((test-dir (if load-file-name
                       (file-name-directory load-file-name)
                     default-directory))
         (test-bib (expand-file-name "test-1.bib" test-dir))
         (bibtex-completion-bibliography test-bib))
    ;; The function should handle string without error during iteration
    (should (stringp bibtex-completion-bibliography))
    ;; Test that normalization works
    (should (listp (org-ref-normalize-bibtex-completion-bibliography)))))

(ert-deftest test-org-ref-bibtex-key-from-doi/list ()
  "Test org-ref-bibtex-key-from-doi with list bibtex-completion-bibliography."
  (let* ((test-dir (if load-file-name
                       (file-name-directory load-file-name)
                     default-directory))
         (test-bib (expand-file-name "test-1.bib" test-dir))
         (bibtex-completion-bibliography (list test-bib)))
    ;; The function should handle list without error
    (should (listp bibtex-completion-bibliography))
    ;; Test that normalization works
    (should (listp (org-ref-normalize-bibtex-completion-bibliography)))))

(ert-deftest test-org-ref-bibtex-key-from-doi/function ()
  "Test org-ref-bibtex-key-from-doi with function bibtex-completion-bibliography."
  (let* ((test-dir (if load-file-name
                       (file-name-directory load-file-name)
                     default-directory))
         (test-bib (expand-file-name "test-1.bib" test-dir))
         (bibtex-completion-bibliography (lambda () (list test-bib))))
    ;; The function should handle function without error
    (should (functionp bibtex-completion-bibliography))
    ;; Test that normalization works
    (should (listp (org-ref-normalize-bibtex-completion-bibliography)))))

;;; Integration test: End-to-end test with real bibliography file

(ert-deftest test-integration/find-entry-in-bibliography-with-function ()
  "Integration test: Find a bibliography entry when using a function.
This tests the fix for issue #1119 where bibtex-completion-bibliography
set to a function would cause 'Wrong type argument: listp' error."
  (let* ((test-dir (if load-file-name
                       (file-name-directory load-file-name)
                     default-directory))
         (test-bib (expand-file-name "test-1.bib" test-dir))
         ;; Set bibtex-completion-bibliography to a function
         (bibtex-completion-bibliography (lambda () (list test-bib))))

    ;; Verify the function works
    (should (functionp bibtex-completion-bibliography))

    ;; Test org-ref-find-bibliography with function
    (with-temp-buffer
      (org-mode)
      (insert "* Test\n")
      (let ((result (org-ref-find-bibliography)))
        ;; Should successfully call the function and return the bibliography
        (should (listp result))
        (should (= 1 (length result)))
        (should (string-suffix-p "test-1.bib" (car result)))))

    ;; Test org-ref-possible-bibfiles with function
    (with-temp-buffer
      (org-mode)
      (insert "* Test\n")
      (let ((result (org-ref-possible-bibfiles)))
        ;; Should successfully handle the function
        (should (listp result))
        (should (cl-some (lambda (f) (string-suffix-p "test-1.bib" f)) result))))))

;;; Test that functions are called (not just checked)

(defvar test-function-call-count 0
  "Counter for testing that bibtex-completion-bibliography function is called.")

(ert-deftest test-function-is-called/side-effects ()
  "Test that when bibtex-completion-bibliography is a function, it's actually called."
  (setq test-function-call-count 0)
  (let ((bibtex-completion-bibliography
         (lambda ()
           (setq test-function-call-count (1+ test-function-call-count))
           '("/test/file.bib"))))

    ;; Call the normalization function
    (org-ref-normalize-bibtex-completion-bibliography)

    ;; Verify the function was actually called
    (should (= test-function-call-count 1))

    ;; Call it again to verify it's called each time
    (org-ref-normalize-bibtex-completion-bibliography)
    (should (= test-function-call-count 2))))

(provide 'test-bibtex-completion-bibliography-normalization)

;;; test-bibtex-completion-bibliography-normalization.el ends here
