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
  (should
   (equal
    (org-ref-split-and-strip-string " key1,key2 ")
    '("key1" "key2"))))

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


(ert-deftest key-file-p ()
  "Check `org-ref-key-in-file-p'"
  (should
   (not
    (null
     (org-ref-key-in-file-p "kitchin-2015-examp" "tests/test-1.bib")))))

(ert-deftest key-file-p-nil ()
  "Check `org-ref-key-in-file-p' for non-existent key"
  (should
   (null
    (org-ref-key-in-file-p "kitchin-examp" "tests/test-1.bib"))))


(ert-deftest key-file ()
  "Check we find a key in a file"
  (should
   (equal
    '("kitchin-2015-examp" . "tests/test-1.bib")
    (let ((org-ref-default-bibliography '("tests/test-1.bib")))
      (org-ref-get-bibtex-key-and-file "kitchin-2015-examp")))))


;; * tests based on org framework
(load-file "tests/org-test.el")

(ert-deftest orlm ()
  (org-test-with-temp-text
      "cite:kitchin-2015-examp

bibliography:tests/test-1.bib
"
    (should
     (string= "Kitchin, John R., \"Examples of Effective Data Sharing in Scientific Publishing\", ACS Catalysis, 5:3894-3899 (2015)"
	      (org-ref-link-message)))))

(ert-deftest orlm-nil ()
  (org-test-with-temp-text
      "cite:kitchin-2015

bibliography:tests/test-1.bib
"
    (should
     (string= "!!! No entry found !!!"
	      (org-ref-link-message)))))


(provide 'org-ref-test)

;;; org-ref-test.el ends here
