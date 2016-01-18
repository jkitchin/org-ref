;;; org-ref-test.el --- Tests for org-ref

;;; Commentary:
;; (progn (eval-buffer) (ert t))

;;; Code:
(require 'org-ref)

(when (require 'undercover nil t)
  (undercover "org-ref.el" (:exclude "*-test.el")))

(ert-deftest plain-and-simple ()
  (should t))

(ert-deftest split-key-1 ()
  "Check if keys are split correctly"
  (equal
   (org-ref-split-and-strip-string " key1,key2 ")
   '("key1" "key2")))

(ert-deftest split-key-2 ()
  "Check if keys are split correctly"
  (should
   (equal
    (org-ref-split-and-strip-string " key1 ")
    '("key1"))))


(ert-deftest find-bib ()
  "Check if we find the bibliography correctly."
  (find-file "tests/test-1.org")
  (should
   (equal
    '("test-1.bib")
    (org-ref-find-bibliography))))


(ert-deftest key-file ()
  "Check we find a key in a file"
  (find-file "tests/test-1.org")
  (should
   (equal
    '("kitchin-2015-examp" . "test-1.bib")
    (org-ref-get-bibtex-key-and-file "kitchin-2015-examp"))))

(provide 'org-ref-test)

;;; org-ref-test.el ends here
