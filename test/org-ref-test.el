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


;;* basic tests
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



(ert-deftest swap-1 ()
  "org swap test"
  (should
   (equal
    '(b a)
    (org-ref-swap-keys 0 1 '(a b)))))

(ert-deftest swap-2 ()
  "org swap test"
  (should
   (equal
    '(a c b)
    (org-ref-swap-keys 1 2 '(a b c)))))


;;* Messages on links
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

(ert-deftest orlm-ref-1 ()
  (should
   (string=
    "!!! NO CONTEXT FOUND !!!count: 0"
    (org-test-with-temp-text
	"ref:one

cite:kitchin-2015

bibliography:tests/test-1.bib
"
      (org-ref-link-message)))))


(ert-deftest orlm-ref-2 ()
  (should
   (string=
    "
#+caption: some text label:one
count: 1"
    (org-test-with-temp-text
	"ref:one

#+caption: some text label:one
"
      (org-ref-link-message)))))

(ert-deftest orlm-ref-3 ()
  (should
   (string=
    "
\\begin{equation}\\label{one}
4
\\end{equation}
count: 1"
    (org-test-with-temp-text
	"eqref:one

\\begin{equation}\\\label{one}
4
\\end{equation}
"
      (org-ref-link-message)))))

(ert-deftest orlm-ref-4 ()
  (should
   (string=
    "
label:one
count: 2"
    (org-test-with-temp-text
	"eqref:one

\\begin{equation}\\\label{one}
4
\\end{equation}

label:one
"
      (org-ref-link-message)))))


(ert-deftest orlm-label-1 ()
  (org-test-with-temp-text
      "label:one

"
    (should
     (string= "1 occurence"
	      (org-ref-link-message)))))


(ert-deftest orlm-label-2 ()
  (org-test-with-temp-text
      "label:one

label:one

"
    (should
     (string= "2 occurences"
	      (org-ref-link-message)))))


;;* get pdf/key
(ert-deftest or-get-pdf ()
  (should
   (string=
    "kitchin-2015.pdf"
    (org-test-with-temp-text
	"cite:kitchin-2015"
      (let ((org-ref-pdf-directory nil))
	(org-ref-get-pdf-filename (org-ref-get-bibtex-key-under-cursor)))))))

(ert-deftest or-get-pdf-2 ()
  (should
   (string=
    "test/kitchin-2015.pdf"
    (org-test-with-temp-text
	"cite:kitchin-2015"
      (let ((org-ref-pdf-directory "test"))
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

;;* get bibliography
(ert-deftest orfb-1 ()
  "test a single bibliography link."
  (should
   (equal
    '("test.bib")
    (org-test-with-temp-text
     "bibliography:test.bib"
     (org-ref-find-bibliography)))))

(ert-deftest orfb-1a ()
  "Get multiple bib files."
  (should
   (equal
    '("test.bib" "test2.bib")
    (org-test-with-temp-text
     "bibliography:test.bib,test2.bib"
     (org-ref-find-bibliography)))))

(ert-deftest orfb-2 ()
  "Get bibfile in latex forma."
  (should
   (equal
    '("test.bib")
    (org-test-with-temp-text
     "
\\bibliography{test}"
     (org-ref-find-bibliography)))))

(ert-deftest orfb-2a ()
  "multiple bibliographies in latex form"
  (should
   (equal
    '("test.bib" "test2.bib")
    (org-test-with-temp-text
     "
\\bibliography{test,test2}"
     (org-ref-find-bibliography)))))


(ert-deftest orfb-3 ()
  "addbibresource form of bibliography."
  (should
   (equal
    '("test.bib")
    (org-test-with-temp-text
     "
\\addbibresource{test.bib}"
     (org-ref-find-bibliography)))))


(ert-deftest orfb-3a ()
  "Multiple bibfiles in addbibresource."
  (should
   (equal
    '("test.bib" "test2.bib")
    (org-test-with-temp-text
     "
\\addbibresource{test.bib, test2.bib}"
     (org-ref-find-bibliography)))))

(ert-deftest orfb-4 ()
  "getting default bibfile in file with no bib specification."
  (should
   (equal
    '("test.bib")
    (org-test-with-temp-text
	""
      (let ((org-ref-default-bibliography '("test.bib")))
	(org-ref-find-bibliography))))))


(ert-deftest unique-keys ()
  (should
   (equal '("kitchin-2008-alloy" "kitchin-2004-role")
	  (org-test-with-temp-text
	      "cite:kitchin-2008-alloy,kitchin-2004-role

cite:kitchin-2004-role

bibliography:tests/test-1.bib
"
	    (org-ref-get-bibtex-keys)))))

(ert-deftest unique-keys-sort ()
  (should
   (equal '("kitchin-2004-role" "kitchin-2008-alloy")
	  (org-test-with-temp-text
	      "cite:kitchin-2008-alloy,kitchin-2004-role

cite:kitchin-2004-role

bibliography:tests/test-1.bib
"
	    (org-ref-get-bibtex-keys t)))))


(ert-deftest get-doi ()
  (should
   (string=
    "10.1103/PhysRevB.77.075437"
    (org-test-with-temp-text
	"cite:kitchin-2008-alloy

bibliography:tests/test-1.bib
"
      (org-ref-get-doi-at-point)))))


;;* bibtex tests We rely a lot on bibtex functionality. These are tests to make
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


;;* test labels
(ert-deftest get-labels-1 ()
  (should
   (equal
    '("test")
    (org-test-with-temp-text
     "#+label: test"
     (org-ref-get-org-labels)))))

(ert-deftest get-labels-2 ()
  (should
   (equal
    '("test")
    (org-test-with-temp-text
     "\\label{test}"
     (org-ref-get-latex-labels)))))

(ert-deftest get-labels-3 ()
  (should
   (equal
    '("test")
    (org-test-with-temp-text
     "
#+tblname: test
| 1 |"
     (org-ref-get-tblnames)))))

(ert-deftest get-labels-4 ()
  (should
   (equal
    '("test")
    (org-test-with-temp-text
     "* header
  :PROPERTIES:
  :CUSTOM_ID: test
  :END:
"
     (org-ref-get-custom-ids)))))

(ert-deftest get-labels-5 ()
  (should
   (= 5
      (length
       (org-test-with-temp-text
	"* header
  :PROPERTIES:
  :CUSTOM_ID: test
  :END:

#+tblname: one
|3|

** subsection \\label{three}
  :PROPERTIES:
  :CUSTOM_ID: two
  :END:

label:four
"
	(org-ref-get-labels))))))


;;* bad cites/labels/refs

(ert-deftest bad-cites ()
  (should
   (= 2
      (length
       (org-test-with-temp-text
	"cite:bad1  cite:bad2"
	(org-ref-bad-cite-candidates))))))


(ert-deftest bad-ref ()
  (should
   (= 5
      (length
       (org-test-with-temp-text
	   "ref:bad1  ref:bad2 eqref:bad3 pageref:bad4 nameref:bad5"
	 (org-ref-bad-ref-candidates))))))

(ert-deftest multiple-labels ()
  (should
   (= 4
      (length
       (org-test-with-temp-text
	"
label:one
\\label{one}
#+tblname: one
| 3|

#+label:one"
	(org-ref-bad-label-candidates))))))


(ert-deftest bad-file-link ()
  (should
   (= 3
      (length
       (org-test-with-temp-text
	   "
file:not.here  [[./or.here]].

We should catch  \\attachfile{latex.style} too.

Why don't we catch [[attachfile:filepath]] or attachfile:some.file?
I think they must be defined in jmax, and are unknown links.
"
	 (org-ref-bad-file-link-candidates))))))



(ert-deftest swap-link-1 ()
  (should
   (string= "cite:key2,key1"
	    (org-test-with-temp-text
	     "cite:key1,key2"
	     (goto-char 6)
	     (org-ref-swap-citation-link 1)
	     (buffer-string)))))

(ert-deftest swap-link-2 ()
  (should
   (string= "cite:key1,key2"
	    (org-test-with-temp-text
	     "cite:key2,key1"
	     (goto-char 6)
	     (org-ref-swap-citation-link 1)
	     (buffer-string)))))

;;* next/prev key

(ert-deftest parse-link-1 ()
  (should
   (equal
    '(("key1" 6 10) ("key2" 11 15))
    (org-test-with-temp-text
     "cite:key1,key2"
     (org-ref-parse-cite)))))

(ert-deftest next-link-1 ()
  (should
   (= 11
      (org-test-with-temp-text
       "cite:key1,key2"
       (goto-char 6)
       (org-ref-next-key) (point)))))


(ert-deftest next-link-2 ()
  (should
   (= 16
      (org-test-with-temp-text
       "cite:key3 cite:key1,key2"
       (goto-char 6)
       (org-ref-next-key) (point)))))

(ert-deftest prev-link-1 ()
  (should
   (= 6
      (org-test-with-temp-text
       "cite:key1,key2"
       (goto-char 11)
       (org-ref-previous-key) (point)))))

;;* delete replace keys
(ert-deftest del-key-1 ()
  (should
   (string= "cite:key2 test"
	    (org-test-with-temp-text
	     "cite:key1,key2 test"
	     (goto-char 6)
	     (org-ref-delete-key-at-point)
	     (buffer-string)))))

(ert-deftest del-key-2 ()
  (should
   (string= "cite:key1 test"
	    (org-test-with-temp-text
	     "cite:key1,key2 test"
	     (goto-char 11)
	     (org-ref-delete-key-at-point)
	     (buffer-string)))))

(ert-deftest del-key-3 ()
  (should
   (string= "cite:key1 text"
	    (org-test-with-temp-text
	     "cite:key1,key2 text"
	     (goto-char 11)
	     (org-ref-delete-key-at-point)
	     (buffer-string)))))

(ert-deftest del-key-4 ()
  (should
   (string= "cite:key2 text"
	    (org-test-with-temp-text
	     "cite:key1,key2 text"
	     (goto-char 6)
	     (org-ref-delete-key-at-point)
	     (buffer-string)))))

(ert-deftest del-key-5 ()
  (should
   (string= "[[cite:key2]] text"
	    (org-test-with-temp-text
	     "[[cite:key1,key2]] text"
	     (goto-char 6)
	     (org-ref-delete-key-at-point)
	     (buffer-string)))))

(ert-deftest del-cite-1 ()
  (should
   (string= "at text"
	    (org-test-with-temp-text
	     "at [[cite:key1,key2]] text"
	     (goto-char 6)
	     (org-ref-delete-cite-at-point)
	     (buffer-string)))))

(ert-deftest del-cite-2 ()
  (should
   (string= "at text"
	    (org-test-with-temp-text
	     "at citenum:key1,key2 text"
	     (goto-char 6)
	     (org-ref-delete-cite-at-point)
	     (buffer-string)))))

(ert-deftest rep-key-1 ()
  (should
   (string= "at citenum:key3,key2 text"
	    (org-test-with-temp-text
	     "at citenum:key1,key2 text"
	     (goto-char 12)
	     (org-ref-replace-key-at-point "key3")
	     (buffer-string)))))

(ert-deftest rep-key-2 ()
  (should
   (string= "at citenum:key1,key3 text"
	    (org-test-with-temp-text
	     "at citenum:key1,key2 text"
	     (goto-char 17)
	     (org-ref-replace-key-at-point "key3")
	     (buffer-string)))))

(ert-deftest rep-key-3 ()
  (should
   (string= "at citenum:key1,key3,key5 text"
	    (org-test-with-temp-text
	     "at citenum:key1,key2 text"
	     (goto-char 17)
	     (org-ref-replace-key-at-point "key3,key5")
	     (buffer-string)))))

(ert-deftest rep-key-4 ()
  (should
   (string= "at citenum:key3,key5,key2 text"
	    (org-test-with-temp-text
	     "at citenum:key1,key2 text"
	     (goto-char 12)
	     (org-ref-replace-key-at-point "key3,key5")
	     (buffer-string)))))


(ert-deftest sort-by-year ()
  (should
   (string= "cite:kitchin-2004-role,kitchin-2008-alloy"
	    (org-test-with-temp-text
	     "cite:kitchin-2008-alloy,kitchin-2004-role

bibliography:tests/test-1.bib
"
	     (org-ref-sort-citation-link)))))



;;* insert keys
(ert-deftest ins-key-1 ()
  (should
   (string= "cite:key1"
	    (org-test-with-temp-text
	     ""
	     (org-ref-insert-key-at-point '("key1"))
	     (buffer-string)))))


(ert-deftest ins-key-2 ()
  (should
   (string= "cite:key2,key1"
	    (org-test-with-temp-text
	     "cite:key1"
	     (org-ref-insert-key-at-point '("key2"))
	     (buffer-string)))))


(ert-deftest ins-key-2a ()
  (should
   (string= "cite:key1,key2,key3"
	    (org-test-with-temp-text
	     "cite:key1,key2"
	     (goto-char 12)
	     (org-ref-insert-key-at-point '("key3"))
	     (buffer-string)))))


(ert-deftest ins-key-3 ()
  (should
   (string= "cite:key1,key2"
	    (org-test-with-temp-text
	     "cite:key1"
	     (goto-char 6)
	     (org-ref-insert-key-at-point '("key2"))
	     (buffer-string)))))


(ert-deftest ins-key-4 ()
  (should
   (string= "cite:key1,key3,key2"
	    (org-test-with-temp-text
	     "cite:key1,key2"
	     (goto-char 6)
	     (org-ref-insert-key-at-point '("key3"))
	     (buffer-string)))))


(ert-deftest ins-key-5 ()
  (should
   (string= "cite:key1,key2 "
	    (org-test-with-temp-text
	     "cite:key1 "
	     (goto-char (point-max))
	     (org-ref-insert-key-at-point '("key2"))
	     (buffer-string)))))

;;* exports
(ert-deftest cite-export-1 ()
  (should
   (string=
    "\\cite{kitchin-2008-alloy}
"
    (org-test-with-temp-text
     "cite:kitchin-2008-alloy"
     (org-latex-export-as-latex nil nil nil t)
     (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest cite-export-2 ()
  (should
   (string=
    "\\cite[page 2]{kitchin-2008-alloy}
"
    (org-test-with-temp-text
     "[[cite:kitchin-2008-alloy][page 2]]"
     (org-latex-export-as-latex nil nil nil t)
     (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest cite-export-3 ()
  (should
   (string=
    "\\cite[page 2][post text]{kitchin-2008-alloy}
"
    (org-test-with-temp-text
     "[[cite:kitchin-2008-alloy][page 2::post text]]"
     (org-latex-export-as-latex nil nil nil t)
     (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest label-export-1 ()
  (should
   (string=
    "\\label{test}
"
    (org-test-with-temp-text
     "label:test"
     (org-latex-export-as-latex nil nil nil t)
     (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest ref-export-1 ()
  (should
   (string=
    "\\ref{test}
"
    (org-test-with-temp-text
     "ref:test"
     (org-latex-export-as-latex nil nil nil t)
     (buffer-substring-no-properties (point-min) (point-max))))))


(ert-deftest bib-export-1 ()
  (should
   (string=
    (format
     "\\bibliography{%s}
" (file-relative-name "test"))
    (org-test-with-temp-text
     "bibliography:test.bib"
     (org-latex-export-as-latex nil nil nil t)
     (buffer-substring-no-properties (point-min) (point-max))))))


(ert-deftest bib-export-1 ()
  (should
   (string=
    (format
     "\\bibliography{%s,%s}
" (file-relative-name "test")
(file-relative-name "titles"))
(org-test-with-temp-text
    "bibliography:test.bib,titles.bib"
  (org-latex-export-as-latex nil nil nil t)
  (buffer-substring-no-properties (point-min) (point-max))))))


;;* org-ref-glossary
(ert-deftest curly-1 ()
  (should
   (= 2
      (org-test-with-temp-text
	  "{}"
	(require 'org-ref-glossary)
	(or-find-closing-curly-bracket)))))


(ert-deftest curly-2 ()
  (should
   (= 4
      (org-test-with-temp-text
	  "{{}}"
	(require 'org-ref-glossary)
	(or-find-closing-curly-bracket)))))

(ert-deftest curly-3 ()
  (should
   (= 3
      (org-test-with-temp-text
	  "{{}}"
	(require 'org-ref-glossary)
	(goto-char 2)
	(or-find-closing-curly-bracket)))))






(ert-deftest bad-citations-1 ()
  (should
   (org-test-with-temp-text
       "
cite:bad

bibliography:tests/test-1.bib
"
     (message "-------------------\n%S" (mapconcat
		    (lambda (x)
		      (file-name-directory (file-truename x)))
		    (org-ref-find-bibliography)		    ":"))
     (org-ref-find-bad-citations)
     (with-current-buffer "*Missing citations*"
       (string-match "^bad \\[\\["
		     (buffer-substring-no-properties (point-min)
						     (point-max)))))))

;;* end
(provide 'org-ref-test)
;;; org-ref-test.el ends here
