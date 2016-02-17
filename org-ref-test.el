;;; org-ref-test.el --- Tests for org-ref

;;; Commentary:
;; Run all tests (progn (eval-buffer) (ert t))
;; New tests: (ert :new)
;; Run failed tests (ert :failed)
;; In the ert buffer:
;; r  rerun the test
;; d rerun with debugger
;; b show backtrace
;; . jump to test

;;; Code:
(require 'org-ref)
(load-file "tests/org-test.el")

(when (require 'undercover nil t)
  (undercover "org-ref.el" (:exclude "*-test.el")))


(ert-deftest or-split-key-1 ()
  "Check if keys are split correctly"
  (should
   (equal
    (org-ref-split-and-strip-string " key1,key2 ")
    '("key1" "key2"))))


(ert-deftest or-split-key-2 ()
  "Check if keys are split correctly"
  (should
   (equal
    (org-ref-split-and-strip-string " key1 ")
    '("key1"))))


(ert-deftest or-find-bib ()
  "Check if we find the bibliography correctly."
  (should
   (equal
    '("test-1.bib")
    (with-temp-buffer
      (insert-file-contents-literally "tests/test-1.org")
      (org-ref-find-bibliography)))))


(ert-deftest or-key-file-p ()
  "Check `org-ref-key-in-file-p'"
  (should
   (not
    (null
     (org-ref-key-in-file-p "kitchin-2015-examp" "tests/test-1.bib")))))


(ert-deftest or-key-file-p-nil ()
  "Check `org-ref-key-in-file-p' for non-existent key"
  (should
   (null
    (org-ref-key-in-file-p "bad-key" "tests/test-1.bib"))))


(ert-deftest or-key-file ()
  "Check we find a key in a file"
  (should
   (equal
    '("kitchin-2015-examp" . "tests/test-1.bib")
    (let ((org-ref-default-bibliography '("tests/test-1.bib")))
      (org-ref-get-bibtex-key-and-file "kitchin-2015-examp")))))


;; * tests based on org framework

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


(ert-deftest or-get-pdf ()
  (should
   (string=
    "kitchin-2015.pdf"
    (org-test-with-temp-text
	"cite:kitchin-2015"
      (let ((org-ref-pdf-directory nil))
	(org-ref-get-pdf-filename (org-ref-get-bibtex-key-under-cursor)))))))

(ert-deftest or-get-key ()
  (should
   (string=
    "kitchin-2015"
    (org-test-with-temp-text
	"cite:kitchin-2015"
      (org-ref-get-bibtex-key-under-cursor)))))

(ert-deftest or-get-key1 ()
  (should
   (string=
    "key1"
    (org-test-with-temp-text
	"cite:key1,key2"
      (goto-char 5)
      (org-ref-get-bibtex-key-under-cursor)))))

(ert-deftest or-get-key2 ()
  (should
   (string=
    "key2"
    (org-test-with-temp-text
	"cite:key1,key2"
      (goto-char 11)
      (org-ref-get-bibtex-key-under-cursor)))))

(ert-deftest orfb-1 ()
  (should
   (equal
    '("test.bib")
    (org-test-with-temp-text
	"bibliography:test.bib"
      (org-ref-find-bibliography)))))

(ert-deftest orfb-1a ()
  (should
   (equal
    '("test.bib" "test2.bib")
    (org-test-with-temp-text
	"bibliography:test.bib,test2.bib"
      (org-ref-find-bibliography)))))

(ert-deftest orfb-2 ()
  (should
   (equal
    '("test.bib")
    (org-test-with-temp-text
	"
\\bibliography{test}"
      (org-ref-find-bibliography)))))

(ert-deftest orfb-2a ()
  (should
   (equal
    '("test.bib" "test2.bib")
    (org-test-with-temp-text
	"
\\bibliography{test,test2}"
      (org-ref-find-bibliography)))))


(ert-deftest orfb-3 ()
  (should
   (equal
    '("test.bib")
    (org-test-with-temp-text
	"
\\addbibresource{test.bib}"
      (org-ref-find-bibliography)))))


(ert-deftest orfb-3a ()
  (should
   (equal
    '("test.bib" "test2.bib")
    (org-test-with-temp-text
	"
\\addbibresource{test.bib, test2.bib}"
      (org-ref-find-bibliography)))))

(ert-deftest orfb-4 ()
  (should
   (equal
    '("test.bib")
    (org-test-with-temp-text
	""
      (let ((org-ref-default-bibliography '("test.bib")))
	(org-ref-find-bibliography))))))


;; * bibtex tests We rely alot on bibtex functionality. These are tests to make
;; sure it works as we expect. I don't have clear evidence, but I feel like I
;; have had trouble with the in the past.
(ert-deftest bib-1 ()
  "test finding an entry in a temp-buffer"
  (should
   (= 1 (with-temp-buffer
	  (insert "@article{rippmann-2013-rethin,
  author =	 {Matthias Rippmann and Philippe Block},
  title =	 {Rethinking Structural Masonry: Unreinforced, Stone-Cut Shells},
  journal =	 {Proceedings of the ICE - Construction Materials},
  volume =	 166,
  number =	 6,
  pages =	 {378-389},
  year =	 2013,
  doi =		 {10.1680/coma.12.00033},
  url =		 {http://dx.doi.org/10.1680/coma.12.00033},
  date_added =	 {Mon Jun 1 09:11:23 2015},
}")
	  (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
	  (bibtex-search-entry "rippmann-2013-rethin")))))


(ert-deftest bib-1a ()
  "Test finding an entry from an existing file."
  (should
   (not (null
	 (with-temp-buffer
	   (insert-file-contents "tests/test-1.bib")
	   (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
	   (bibtex-search-entry "kitchin-2015-examp"))))))


(ert-deftest bib-2 ()
  "Test for null entry"
  (should
   (null (with-temp-buffer
	   (insert-file-contents "tests/test-1.bib")
	   (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
	   (bibtex-search-entry "bad-key")))))




(provide 'org-ref-test)

;;; org-ref-test.el ends here
