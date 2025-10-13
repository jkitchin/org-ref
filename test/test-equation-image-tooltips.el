;;; test-equation-image-tooltips.el --- Tests for equation image tooltips -*- lexical-binding: t; -*-

;; Tests for the equation image tooltip feature (issue #1064)

(require 'ert)
(require 'org-ref-ref-links)

;;; Test helper functions

(defun test-create-mock-overlay (begin end image-file)
  "Create a mock org-latex-preview overlay for testing.
BEGIN and END define the overlay region, IMAGE-FILE is the path to the image."
  (let ((ov (make-overlay begin end)))
    (overlay-put ov 'org-overlay-type 'org-latex-overlay)
    (overlay-put ov 'display
                 (list 'image
                       :type 'png
                       :file image-file
                       :ascent 'center))
    ov))

;;; Tests for org-ref-find-overlay-with-image

(ert-deftest test-find-overlay-with-image/found ()
  "Test finding an overlay with an image display property."
  (with-temp-buffer
    (insert "\\begin{equation}\n\\label{eq:test}\nE = mc^2\n\\end{equation}")
    (let* ((begin (point-min))
           (end (point-max))
           (temp-image "/tmp/test-image.png")
           (ov (test-create-mock-overlay begin end temp-image))
           (result (org-ref-find-overlay-with-image begin end)))
      (should result)
      (should (eq (car result) 'image))
      (should (equal (plist-get (cdr result) :file) temp-image))
      (delete-overlay ov))))

(ert-deftest test-find-overlay-with-image/not-found ()
  "Test that nil is returned when no overlay exists."
  (with-temp-buffer
    (insert "\\begin{equation}\n\\label{eq:test}\nE = mc^2\n\\end{equation}")
    (let ((result (org-ref-find-overlay-with-image (point-min) (point-max))))
      (should (null result)))))

;;; Tests for org-ref-get-preview-image-at-label

(ert-deftest test-get-preview-image/with-label ()
  "Test finding preview image for a label."
  (with-temp-buffer
    (org-mode)
    (insert "\\begin{equation}\n\\label{eq:test}\nE = mc^2\n\\end{equation}")
    (let* ((temp-image "/tmp/test-equation.png")
           (ov (test-create-mock-overlay (point-min) (point-max) temp-image)))
      (should (org-ref-get-preview-image-at-label "eq:test"))
      (let ((image-spec (org-ref-get-preview-image-at-label "eq:test")))
        (should image-spec)
        (should (eq (car image-spec) 'image))
        (should (equal (plist-get (cdr image-spec) :file) temp-image)))
      (delete-overlay ov))))

(ert-deftest test-get-preview-image/no-label ()
  "Test that nil is returned for non-existent label."
  (with-temp-buffer
    (org-mode)
    (insert "\\begin{equation}\n\\label{eq:test}\nE = mc^2\n\\end{equation}")
    (should (null (org-ref-get-preview-image-at-label "eq:nonexistent")))))

(ert-deftest test-get-preview-image/with-name ()
  "Test finding preview image using #+name: label."
  (with-temp-buffer
    (org-mode)
    (insert "#+name: eq:named\n\\begin{equation}\nE = mc^2\n\\end{equation}")
    (let* ((temp-image "/tmp/test-named.png")
           (begin (+ (point-min) (length "#+name: eq:named\n")))
           (end (point-max))
           (ov (test-create-mock-overlay begin end temp-image)))
      (should (org-ref-get-preview-image-at-label "eq:named"))
      (let ((image-spec (org-ref-get-preview-image-at-label "eq:named")))
        (should image-spec)
        (should (eq (car image-spec) 'image)))
      (delete-overlay ov))))

;;; Tests for caching

(ert-deftest test-preview-image-caching ()
  "Test that preview image results are cached properly."
  (with-temp-buffer
    (org-mode)
    (insert "\\begin{equation}\n\\label{eq:cache}\nE = mc^2\n\\end{equation}")
    (let* ((temp-image "/tmp/test-cache.png")
           (ov (test-create-mock-overlay (point-min) (point-max) temp-image)))

      ;; First call should populate cache
      (org-ref-get-preview-image-at-label "eq:cache")
      (should org-ref-preview-image-cache)
      (should org-ref-preview-cache-tick)

      ;; Second call should use cache (same tick)
      (let ((cached-result (org-ref-get-preview-image-at-label "eq:cache")))
        (should cached-result)
        (should (eq (car cached-result) 'image)))

      (delete-overlay ov))))

(ert-deftest test-preview-cache-invalidation ()
  "Test that cache is invalidated when buffer is modified."
  (with-temp-buffer
    (org-mode)
    (insert "\\begin{equation}\n\\label{eq:inval}\nE = mc^2\n\\end{equation}")
    (let* ((temp-image "/tmp/test-invalid.png")
           (ov (test-create-mock-overlay (point-min) (point-max) temp-image)))

      ;; Populate cache
      (org-ref-get-preview-image-at-label "eq:inval")
      (let ((old-tick org-ref-preview-cache-tick))

        ;; Modify buffer
        (goto-char (point-max))
        (insert "\n")

        ;; Cache should be invalidated on next call
        (org-ref-get-preview-image-at-label "eq:inval")
        (should (not (equal org-ref-preview-cache-tick old-tick))))

      (delete-overlay ov))))

;;; Tests for org-ref-ref-help-echo

(ert-deftest test-help-echo/returns-context-when-disabled ()
  "Test that help-echo returns text context when feature is disabled."
  (with-temp-buffer
    (org-mode)
    (insert "\\begin{equation}\n\\label{eq:disabled}\nE = mc^2\n\\end{equation}\n\n")
    (insert "See eqref:eq:disabled")
    (let ((org-ref-show-equation-images-in-tooltips nil)
          (pos (- (point) 1)))
      ;; Set the label property as org-ref-ref-activate would
      (put-text-property (- (point) 11) (point) 'org-ref-ref-label "eq:disabled")

      (let ((result (org-ref-ref-help-echo nil nil pos)))
        ;; Should return context string, not try to show image
        (should (stringp result))))))

(ert-deftest test-help-echo/returns-context-when-no-image ()
  "Test that help-echo returns text when no preview image exists."
  (with-temp-buffer
    (org-mode)
    (insert "\\begin{equation}\n\\label{eq:noimage}\nE = mc^2\n\\end{equation}\n\n")
    (insert "See eqref:eq:noimage")
    (let ((org-ref-show-equation-images-in-tooltips t)
          (pos (- (point) 1)))
      ;; Set the label property
      (put-text-property (- (point) 11) (point) 'org-ref-ref-label "eq:noimage")

      (let ((result (org-ref-ref-help-echo nil nil pos)))
        ;; Should return context string since no overlay exists
        (should (stringp result))))))

(ert-deftest test-help-echo/messages-image-when-available ()
  "Test that help-echo calls message with image when preview exists."
  (with-temp-buffer
    (org-mode)
    (insert "\\begin{equation}\n\\label{eq:hasimage}\nE = mc^2\n\\end{equation}\n\n")
    (insert "See eqref:eq:hasimage")

    (let* ((org-ref-show-equation-images-in-tooltips t)
           (temp-image "/tmp/test-help-echo.png")
           (eq-begin (point-min))
           (eq-end (- (point-max) 23))  ; before "See eqref..."
           (ov (test-create-mock-overlay eq-begin eq-end temp-image))
           (pos (- (point) 1))
           (message-called nil)
           (message-arg nil))

      ;; Set the label property
      (put-text-property (- (point) 11) (point) 'org-ref-ref-label "eq:hasimage")

      ;; Create temp image file so file-exists-p succeeds
      (write-region "" nil temp-image)

      ;; Mock display-graphic-p to return t for this test
      (cl-letf (((symbol-function 'display-graphic-p)
                 (lambda () t))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq message-called t)
                   (setq message-arg (car args)))))

        (let ((result (org-ref-ref-help-echo nil nil pos)))
          ;; Should have called message with propertized string (only in GUI mode)
          (should message-called)
          (should (stringp message-arg))
          (should (get-text-property 0 'display message-arg))
          ;; Should still return context for fallback tooltip
          (should (stringp result))))

      (delete-file temp-image)
      (delete-overlay ov))))

;;; Tests for edge cases

(ert-deftest test-help-echo/no-label-property ()
  "Test help-echo when point has no org-ref-ref-label property."
  (with-temp-buffer
    (org-mode)
    (insert "Some text without a label")
    (let ((org-ref-show-equation-images-in-tooltips t)
          (pos (point)))
      (let ((result (org-ref-ref-help-echo nil nil pos)))
        ;; Should return nil or context gracefully
        (should (or (null result) (stringp result)))))))

(ert-deftest test-help-echo/missing-file ()
  "Test help-echo when image file doesn't exist."
  (with-temp-buffer
    (org-mode)
    (insert "\\begin{equation}\n\\label{eq:missing}\nE = mc^2\n\\end{equation}\n\n")
    (insert "See eqref:eq:missing")

    (let* ((org-ref-show-equation-images-in-tooltips t)
           (nonexistent-image "/tmp/nonexistent-12345.png")
           (eq-begin (point-min))
           (eq-end (- (point-max) 22))
           (ov (test-create-mock-overlay eq-begin eq-end nonexistent-image))
           (pos (- (point) 1)))

      ;; Set the label property
      (put-text-property (- (point) 11) (point) 'org-ref-ref-label "eq:missing")

      (let ((result (org-ref-ref-help-echo nil nil pos)))
        ;; Should fall back to text context when file doesn't exist
        (should (stringp result)))

      (delete-overlay ov))))

(provide 'test-equation-image-tooltips)

;;; test-equation-image-tooltips.el ends here
