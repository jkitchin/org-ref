;;; test-native-compilation-warnings.el --- Test for native compilation warnings -*- lexical-binding: t; -*-

;; Test to ensure all org-ref .el files compile without warnings about
;; undefined functions.

(require 'ert)
(require 'bytecomp)

(defun test-org-ref--get-source-files ()
  "Return list of all org-ref .el files to test."
  (let* ((org-ref-dir
          (cond
           ;; When loaded normally, use the file's location
           ((or load-file-name buffer-file-name)
            (expand-file-name ".." (file-name-directory (or load-file-name
                                                            buffer-file-name))))
           ;; When run from batch, look for test directory in current directory
           ((file-directory-p "test")
            default-directory)
           ;; If in test directory, go up one level
           ((string-match-p "/test/?$" default-directory)
            (expand-file-name ".." default-directory))
           (t
            (error "Cannot determine org-ref directory. Run from org-ref root or test directory")))))
    (unless (file-directory-p org-ref-dir)
      (error "org-ref directory not found: %s" org-ref-dir))
    (directory-files org-ref-dir t "\\.el\\'")))

(defun test-org-ref--compile-and-check-warnings (source-file)
  "Compile SOURCE-FILE and return list of undefined function warnings."
  (let* ((byte-compile-dest-file-function
          (lambda (source)
            (concat (file-name-sans-extension source) ".elc-test")))
         (output-file (funcall byte-compile-dest-file-function source-file))
         (byte-compile-error-on-warn nil)
         (warning-prefix-alist nil)  ; Suppress warning prefix
         (undefined-warnings '()))

    ;; Clear warnings buffer
    (when (get-buffer "*Compile-Log*")
      (with-current-buffer "*Compile-Log*"
        (let ((inhibit-read-only t))
          (erase-buffer))))

    (unwind-protect
        (progn
          ;; Byte compile the file
          (byte-compile-file source-file)

          ;; Extract warnings from the compile log buffer
          (when (get-buffer "*Compile-Log*")
            (with-current-buffer "*Compile-Log*"
              (goto-char (point-min))
              (while (re-search-forward "^.*:\\(.*\\)Warning: \\(.*not known to be defined.*\\)" nil t)
                (let ((warning-text (match-string 2)))
                  (push (list :string warning-text
                             :file source-file)
                        undefined-warnings)))))

          undefined-warnings)

      ;; Clean up: delete the test compilation output
      (when (file-exists-p output-file)
        (delete-file output-file)))))

(ert-deftest test-org-ref/no-undefined-function-warnings ()
  "Test that all org-ref .el files compile without undefined function warnings.
This test byte-compiles all org-ref source files and checks that there are
no warnings about functions not being known to be defined."
  (let* ((source-files (test-org-ref--get-source-files))
         (all-warnings '()))

    ;; Compile each file and collect warnings
    (dolist (source-file source-files)
      (message "Compiling %s..." (file-name-nondirectory source-file))
      (let ((warnings (test-org-ref--compile-and-check-warnings source-file)))
        (when warnings
          (setq all-warnings (append all-warnings warnings)))))

    ;; Report any undefined function warnings found
    (when all-warnings
      (message "\n=== Undefined Function Warnings Found ===")
      (dolist (warning all-warnings)
        (message "%s: %s"
                (file-name-nondirectory (plist-get warning :file))
                (plist-get warning :string)))
      (message "=========================================\n")
      (message "Total warnings: %d" (length all-warnings)))

    ;; The test passes if there are no undefined function warnings
    (should (null all-warnings))))


(ert-deftest test-org-ref/native-compile-clean ()
  "Test that all org-ref files native-compile without warnings.
This test only runs if native compilation is available in the current Emacs.
It checks for warnings from the native compiler about undefined functions."
  :expected-result (if (and (fboundp 'native-comp-available-p)
                            (native-comp-available-p))
                       :passed
                     :skipped)

  (skip-unless (and (fboundp 'native-comp-available-p)
                    (native-comp-available-p)))

  (let* ((source-files (test-org-ref--get-source-files))
         (native-compile-target-directory
          (expand-file-name "test-native-compile" temporary-file-directory))
         (all-files-with-warnings '()))

    ;; Create target directory
    (make-directory native-compile-target-directory t)

    ;; Clear the warnings buffer before starting
    (when (get-buffer "*Warnings*")
      (with-current-buffer "*Warnings*"
        (erase-buffer)))

    (unwind-protect
        (progn
          ;; Native compile each file synchronously
          (dolist (source-file source-files)
            (when (fboundp 'native-compile)
              (condition-case err
                  (progn
                    (message "Native compiling %s..." (file-name-nondirectory source-file))
                    (native-compile source-file
                                   (expand-file-name
                                    (file-name-nondirectory source-file)
                                    native-compile-target-directory))

                    ;; Check for undefined function warnings in the warnings buffer
                    (when (get-buffer "*Warnings*")
                      (with-current-buffer "*Warnings*"
                        (let ((warnings-text (buffer-string)))
                          (when (and (not (string= warnings-text ""))
                                    (string-match-p "not known to be defined" warnings-text))
                            (push (cons (file-name-nondirectory source-file)
                                       warnings-text)
                                  all-files-with-warnings)))))

                    ;; Clear warnings buffer for next file
                    (when (get-buffer "*Warnings*")
                      (with-current-buffer "*Warnings*"
                        (erase-buffer))))
                (error
                 (message "Native compilation error for %s: %S"
                         (file-name-nondirectory source-file) err)))))

          ;; Report any warnings found
          (when all-files-with-warnings
            (message "\n=== Native Compilation Warnings Found ===")
            (dolist (file-warnings all-files-with-warnings)
              (message "\nFile: %s" (car file-warnings))
              (message "%s" (cdr file-warnings)))
            (message "=========================================\n")
            (message "Total files with warnings: %d" (length all-files-with-warnings)))

          ;; The test passes if there are no undefined function warnings
          (should (null all-files-with-warnings)))

      ;; Clean up: delete the test compilation output
      (when (file-directory-p native-compile-target-directory)
        (delete-directory native-compile-target-directory t)))))

(provide 'test-native-compilation-warnings)

;;; test-native-compilation-warnings.el ends here
