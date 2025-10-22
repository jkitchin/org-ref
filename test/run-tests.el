;;; run-tests.el --- Test runner for org-ref -*- lexical-binding: t; -*-

;; This file provides a simple way to run all org-ref tests.
;; It can be used standalone or via ert-runner.

;;; Commentary:
;;
;; Usage:
;;   emacs -batch -L . -L test -l test/run-tests.el
;;
;; This will automatically load and run all *-test.el files in the test directory.

;;; Code:

(require 'ert)

;; Add current directory and test directory to load path
(add-to-list 'load-path (expand-file-name "."))
(add-to-list 'load-path (expand-file-name "test"))

;; Load all test files
(dolist (test-file (directory-files (expand-file-name "test")
                                    t
                                    ".*-test\\.el\\'"))
  (message "Loading test file: %s" test-file)
  (load-file test-file))

;; If running in batch mode, run the tests
(when noninteractive
  (ert-run-tests-batch-and-exit))

(provide 'run-tests)

;;; run-tests.el ends here
