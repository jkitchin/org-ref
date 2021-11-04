(ert-deftest or-key-file-p ()
  "Check `org-ref-key-in-file-p'"
  (should
   (not
    (null
     (org-ref-key-in-file-p "kitchin-2015-examp"
			    (expand-file-name
			     "test/test-1.bib"
			     (file-name-directory (locate-library "org-ref"))))))))

(ert-deftest or-key-file-p-nil ()
  "Check `org-ref-key-in-file-p' for non-existent key"
  (should
   (null
    (org-ref-key-in-file-p "bad-key"
			   (expand-file-name
			    "test/test-1.bib"
			    (file-name-directory (locate-library "org-ref")))))))

(ert-deftest swap-1 ()
  "org swap test"
  (should
   (equal
    '(b a)
    (org-ref-swap-list-elements 0 1 '(a b)))))

(ert-deftest swap-2 ()
  "org swap test"
  (should
   (equal
    '(a c b)
    (org-ref-swap-list-elements 1 2 '(a b c)))))

(ert-deftest orfb-3 ()
  "addbibresource form of bibliography."
  (should
   (equal
    (list (expand-file-name
	   "test/test-1.bib"
	   (file-name-directory
	    (locate-library "org-ref"))))
    (mapcar 'file-truename
	    (org-test-with-temp-text
		(format "\\addbibresource{%s}"
			(expand-file-name
			 "test/test-1.bib"
			 (file-name-directory
			  (locate-library "org-ref"))))
	      (org-ref-find-bibliography))))))

(ert-deftest orfb-3a ()
  "multiple bibliographies addbibresource form of bibliography."
  (should
   (equal
    (list (expand-file-name
	   "test/test-1.bib"
	   (file-name-directory
	    (locate-library "org-ref")))
	  (expand-file-name
	   "test/test-2.bib"
	   (file-name-directory
	    (locate-library "org-ref"))))
    (org-test-with-temp-text
	(format "\\addbibresource{%s}
\\addbibresource{%s}"
		(expand-file-name
		 "test/test-1.bib"
		 (file-name-directory
		  (locate-library "org-ref")))
		(expand-file-name
		 "test/test-2.bib"
		 (file-name-directory
		  (locate-library "org-ref"))))
      (org-ref-find-bibliography)))))

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

(ert-deftest short-titles ()
  (org-ref-bibtex-generate-shorttitles)
  (prog1
      (should
       (file-exists-p "shorttitles.bib"))
    (delete-file "shorttitles.bib")))

(ert-deftest long-titles ()
  (org-ref-bibtex-generate-longtitles)

  (prog1
      (should
       (file-exists-p "longtitles.bib"))
    (delete-file "longtitles.bib")))

(ert-deftest title-case-1 ()
  (should
   (string=
    "Examples of Effective Data Sharing"
    (with-temp-buffer
      (insert "@article{kitchin-2015-examp,
author =	 {Kitchin, John R.},
title =	 {Examples of effective data sharing},
journal =	 {ACS Catalysis},
volume =	 {5},
number =	 {6},
pages =	 {3894-3899},
year =	 2015,
doi =		 {10.1021/acscatal.5b00538},
url =		 { http://dx.doi.org/10.1021/acscatal.5b00538 },
keywords =	 {DESC0004031, early-career, orgmode, Data sharing },
eprint =	 { http://dx.doi.org/10.1021/acscatal.5b00538 },
}")
      (bibtex-mode)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (goto-char (point-min))
      (org-ref-title-case-article)
      (bibtex-autokey-get-field "title")))))

(ert-deftest title-case-2 ()
  (should (string=
	   "Examples of Effective Data-Sharing"
	   (with-temp-buffer
	     (bibtex-mode)
	     (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
	     (insert "@article{kitchin-2015-examp,
author =	 {Kitchin, John R.},
title =	 {Examples of effective data-sharing},
journal =	 {ACS Catalysis},
volume =	 {5},
number =	 {6},
pages =	 {3894-3899},
year =	 2015,
doi =		 {10.1021/acscatal.5b00538},
url =		 { http://dx.doi.org/10.1021/acscatal.5b00538 },
keywords =	 {DESC0004031, early-career, orgmode, Data sharing },
eprint =	 { http://dx.doi.org/10.1021/acscatal.5b00538 },
}")
	     (goto-char (point-min))
	     (org-ref-title-case-article)
	     (bibtex-autokey-get-field "title")))))

(ert-deftest title-case-3 ()
  (should (string=
	   "An Example of Effective Data-Sharing"
	   (with-temp-buffer
	     (bibtex-mode)
	     (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
	     (insert "@article{kitchin-2015-examp,
author =	 {Kitchin, John R.},
title =	 {An example of effective data-sharing},
journal =	 {ACS Catalysis},
volume =	 {5},
number =	 {6},
pages =	 {3894-3899},
year =	 2015,
doi =		 {10.1021/acscatal.5b00538},
url =		 { http://dx.doi.org/10.1021/acscatal.5b00538 },
keywords =	 {DESC0004031, early-career, orgmode, Data sharing },
eprint =	 { http://dx.doi.org/10.1021/acscatal.5b00538 },
}")
	     (goto-char (point-min))
	     (org-ref-title-case-article)
	     (bibtex-autokey-get-field "title")))))

(ert-deftest title-case-4 ()
  (should (string=
	   "An Example of Effective Data-Sharing"
	   (with-temp-buffer
	     (bibtex-mode)
	     (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
	     (insert "@book{kitchin-2015-examp,
author =	 {Kitchin, John R.},
title =	 {An example of effective data-sharing},
publisher = {Awesome Publishing},
year =	 2015,
keywords =	 {DESC0004031, early-career, orgmode, Data sharing },
}")
	     (goto-char (point-min))
	     (let ((org-ref-title-case-types '(("book" "title"))))
	       (org-ref-title-case))
	     (bibtex-autokey-get-field "title")))))

(ert-deftest sentence-case-1 ()
  (should (string=
	   "Examples of effective data sharing"
	   (with-temp-buffer
	     (bibtex-mode)
	     (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
	     (insert "@article{kitchin-2015-examp,
author =	 {Kitchin, John R.},
title =	 {Examples of Effective Data Sharing},
journal =	 {ACS Catalysis},
volume =	 {5},
number =	 {6},
pages =	 {3894-3899},
year =	 2015,
doi =		 {10.1021/acscatal.5b00538},
url =		 { http://dx.doi.org/10.1021/acscatal.5b00538 },
keywords =	 {DESC0004031, early-career, orgmode, Data sharing },
eprint =	 { http://dx.doi.org/10.1021/acscatal.5b00538 },
}")
	     (goto-char (point-min))
	     (org-ref-sentence-case-article)
	     (bibtex-autokey-get-field "title")))))

(ert-deftest sentence-case-2 ()
  (should (string=
	   "Effective data sharing: A study"
	   (with-temp-buffer
	     (bibtex-mode)
	     (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
	     (insert "@article{kitchin-2015-examp,
author =	 {Kitchin, John R.},
title =	 {Effective Data Sharing: A study},
journal =	 {ACS Catalysis},
volume =	 {5},
number =	 {6},
pages =	 {3894-3899},
year =	 2015,
doi =		 {10.1021/acscatal.5b00538},
url =		 { http://dx.doi.org/10.1021/acscatal.5b00538 },
keywords =	 {DESC0004031, early-career, orgmode, Data sharing },
eprint =	 { http://dx.doi.org/10.1021/acscatal.5b00538 },
}")
	     (goto-char (point-min))
	     (org-ref-sentence-case-article)
	     (bibtex-autokey-get-field "title")))))

(ert-deftest stringify ()
  (should
   (string=
    "JCP"
    (with-temp-buffer
      (insert "@article{xu-2015-relat,
author =	 {Zhongnan Xu and John R. Kitchin},
title =	 {Relationships Between the Surface Electronic and Chemical
Properties of Doped 4d and 5d Late Transition Metal Dioxides},
keywords =	 {orgmode},
journal =	 {The Journal of Chemical Physics},
volume =	 142,
number =	 10,
pages =	 104703,
year =	 2015,
doi =		 {10.1063/1.4914093},
url =		 {http://dx.doi.org/10.1063/1.4914093},
date_added =	 {Sat Oct 24 10:57:22 2015},
}")
      (bibtex-mode)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (org-ref-stringify-journal-name)
      (bibtex-autokey-get-field "journal")))))

(ert-deftest next-entry-1 ()
  (should
   (string=
    "@article{xu-2015-relat,"
    (with-temp-buffer
      (bibtex-mode)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (insert "@article{kitchin-2015-examp,
author =	 {Kitchin, John R.},
title =	 {Examples of Effective Data Sharing in Scientific Publishing},
journal =	 {ACS Catalysis},
volume =	 {5},
number =	 {6},
pages =	 {3894-3899},
year =	 2015,
doi =		 {10.1021/acscatal.5b00538},
url =		 { http://dx.doi.org/10.1021/acscatal.5b00538 },
keywords =	 {DESC0004031, early-career, orgmode, Data sharing },
eprint =	 { http://dx.doi.org/10.1021/acscatal.5b00538 },
}

@article{xu-2015-relat,
author =	 {Zhongnan Xu and John R. Kitchin},
title =	 {Relationships Between the Surface Electronic and Chemical
Properties of Doped 4d and 5d Late Transition Metal Dioxides},
keywords =	 {orgmode},
journal =	 {The Journal of Chemical Physics},
volume =	 142,
number =	 10,
pages =	 104703,
year =	 2015,
doi =		 {10.1063/1.4914093},
url =		 {http://dx.doi.org/10.1063/1.4914093},
date_added =	 {Sat Oct 24 10:57:22 2015},
}

")
      (goto-char (point-min))
      (org-ref-bibtex-next-entry)
      (buffer-substring (line-beginning-position) (line-end-position))))))

(ert-deftest prev-entry-1 ()
  (should
   (string=
    "@article{kitchin-2015-examp,"
    (with-temp-buffer
      (bibtex-mode)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (insert "@article{kitchin-2015-examp,
author =	 {Kitchin, John R.},
title =	 {Examples of Effective Data Sharing in Scientific Publishing},
journal =	 {ACS Catalysis},
volume =	 {5},
number =	 {6},
pages =	 {3894-3899},
year =	 2015,
doi =		 {10.1021/acscatal.5b00538},
url =		 { http://dx.doi.org/10.1021/acscatal.5b00538 },
keywords =	 {DESC0004031, early-career, orgmode, Data sharing },
eprint =	 { http://dx.doi.org/10.1021/acscatal.5b00538 },
}

@article{xu-2015-relat,
author =	 {Zhongnan Xu and John R. Kitchin},
title =	 {Relationships Between the Surface Electronic and Chemical
Properties of Doped 4d and 5d Late Transition Metal Dioxides},
keywords =	 {orgmode},
journal =	 {The Journal of Chemical Physics},
volume =	 142,
number =	 10,
pages =	 104703,
year =	 2015,
doi =		 {10.1063/1.4914093},
url =		 {http://dx.doi.org/10.1063/1.4914093},
date_added =	 {Sat Oct 24 10:57:22 2015},
}

")
      (re-search-backward "xu-2015")
      (org-ref-bibtex-previous-entry)
      (buffer-substring (line-beginning-position) (line-end-position))))))

(ert-deftest get-bibtex-keys ()
  (should
   (equal
    '("DESC0004031" "early-career" "orgmode" "Data sharing ")
    (with-temp-buffer
      (bibtex-mode)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (insert "@article{kitchin-2015-examp,
author =	 {Kitchin, John R.},
title =	 {Examples of Effective Data Sharing in Scientific Publishing},
journal =	 {ACS Catalysis},
volume =	 {5},
number =	 {6},
pages =	 {3894-3899},
year =	 2015,
doi =		 {10.1021/acscatal.5b00538},
url =		 { http://dx.doi.org/10.1021/acscatal.5b00538 },
keywords =	 {DESC0004031, early-career, orgmode, Data sharing },
eprint =	 { http://dx.doi.org/10.1021/acscatal.5b00538 },
}

@article{xu-2015-relat,
author =	 {Zhongnan Xu and John R. Kitchin},
title =	 {Relationships Between the Surface Electronic and Chemical
Properties of Doped 4d and 5d Late Transition Metal Dioxides},
keywords =	 {orgmode},
journal =	 {The Journal of Chemical Physics},
volume =	 142,
number =	 10,
pages =	 104703,
year =	 2015,
doi =		 {10.1063/1.4914093},
url =		 {http://dx.doi.org/10.1063/1.4914093},
date_added =	 {Sat Oct 24 10:57:22 2015},
}

")
      (org-ref-bibtex-keywords)))))

(ert-deftest set-bibtex-keys ()
  (should
   (equal
    '("key1" "key2" "orgmode")
    (with-temp-buffer
      (insert "@article{xu-2015-relat,
author =	 {Zhongnan Xu and John R. Kitchin},
title =	 {Relationships Between the Surface Electronic and Chemical
Properties of Doped 4d and 5d Late Transition Metal Dioxides},
keywords =	 {orgmode},
journal =	 {The Journal of Chemical Physics},
volume =	 142,
number =	 10,
pages =	 104703,
year =	 2015,
doi =		 {10.1063/1.4914093},
url =		 {http://dx.doi.org/10.1063/1.4914093},
date_added =	 {Sat Oct 24 10:57:22 2015},
}")
      (bibtex-mode)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (goto-char (point-min))
      (org-ref-set-bibtex-keywords '("key1" "key2"))
      (org-ref-bibtex-keywords)))))

(ert-deftest clean-year-1 ()
  (should
   (string=
    "2015"
    (with-temp-buffer
      (insert "@article{kitchin-2015-examp,
author =	 {Kitchin, John R.},
title =	 {Examples of effective data sharing},
journal =	 {ACS Catalysis},
volume =	 {5},
number =	 {6},
pages =	 {3894-3899},
year =	 {0},
doi =		 {10.1021/acscatal.5b00538},
url =		 { http://dx.doi.org/10.1021/acscatal.5b00538 },
keywords =	 {DESC0004031, early-career, orgmode, Data sharing },
eprint =	 { http://dx.doi.org/10.1021/acscatal.5b00538 },
}")
      (bibtex-mode)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (goto-char (point-min))
      (orcb-clean-year "2015")
      (bibtex-autokey-get-field "year")))))

(ert-deftest clean-year-2 ()
  (should
   (string=
    "2015"
    (with-temp-buffer
      (insert "@article{kitchin-2015-examp,
author =	 {Kitchin, John R.},
title =	 {Examples of effective data sharing},
journal =	 {ACS Catalysis},
volume =	 {5},
number =	 {6},
pages =	 {3894-3899},
year =	 {2015},
doi =		 {10.1021/acscatal.5b00538},
url =		 { http://dx.doi.org/10.1021/acscatal.5b00538 },
keywords =	 {DESC0004031, early-career, orgmode, Data sharing },
eprint =	 { http://dx.doi.org/10.1021/acscatal.5b00538 },
}")
      (bibtex-mode)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (goto-char (point-min))
      (orcb-clean-year "2014")
      (bibtex-autokey-get-field "year")))))

(ert-deftest clean-& ()
  (should
   (string=
    "Examples of \\& effective data sharing"
    (with-temp-buffer
      (insert "@article{kitchin-2015-examp,
author =	 {Kitchin, John R.},
title =	 {Examples of & effective data sharing},
journal =	 {ACS Catalysis},
volume =	 {5},
number =	 {6},
pages =	 {3894-3899},
year =	 {2015},
doi =		 {10.1021/acscatal.5b00538},
url =		 { http://dx.doi.org/10.1021/acscatal.5b00538 },
keywords =	 {DESC0004031, early-career, orgmode, Data sharing },
eprint =	 { http://dx.doi.org/10.1021/acscatal.5b00538 },
}")
      (bibtex-mode)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (goto-char (point-min))
      (orcb-&)
      (bibtex-autokey-get-field "title")))))

(ert-deftest clean-comma ()
  (should
   (string=
    "@article{kitchin-2015-examp,"
    (with-temp-buffer
      (insert "@article{kitchin-2015-examp
author =	 {Kitchin, John R.},
title =	 {Examples of & effective data sharing},
journal =	 {ACS Catalysis},
volume =	 {5},
number =	 {6},
pages =	 {3894-3899},
year =	 {2015},
doi =		 {10.1021/acscatal.5b00538},
url =		 { http://dx.doi.org/10.1021/acscatal.5b00538 },
keywords =	 {DESC0004031, early-career, orgmode, Data sharing },
eprint =	 { http://dx.doi.org/10.1021/acscatal.5b00538 },
}")
      (bibtex-mode)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (goto-char (point-min))
      (orcb-key-comma)
      (buffer-substring-no-properties (point-min)
				      (line-end-position))))))

(ert-deftest clean-pages-1 ()
  (should
   (string=
    "123456789"
    (with-temp-buffer
      (insert "@article{kitchin-2015-examp
author =	 {Kitchin, John R.},
title =	 {Examples of & effective data sharing},
journal =	 {ACS Catalysis},
volume =	 {5},
number =	 {6},
pages =	 {},
eid = {123456789},
year =	 {2015},
doi =		 {10.1021/acscatal.5b00538},
url =		 { http://dx.doi.org/10.1021/acscatal.5b00538 },
keywords =	 {DESC0004031, early-career, orgmode, Data sharing },
eprint =	 { http://dx.doi.org/10.1021/acscatal.5b00538 },
}")
      (bibtex-mode)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (goto-char (point-min))
      (orcb-clean-pages)
      (bibtex-autokey-get-field "pages")))))

(ert-deftest clean-doi-1 ()
  (should
   (string=
    "10.1021/acscatal.5b00538"
    (with-temp-buffer
      (insert "@article{kitchin-2015-examp
author =	 {Kitchin, John R.},
title =	 {Examples of & effective data sharing},
journal =	 {ACS Catalysis},
volume =	 {5},
number =	 {6},
pages =	 {},
eid = {123456789},
year =	 {2015},
doi =		 {http://dx.doi.org/10.1021/acscatal.5b00538},
url =		 { http://dx.doi.org/10.1021/acscatal.5b00538 },
keywords =	 {DESC0004031, early-career, orgmode, Data sharing },
eprint =	 { http://dx.doi.org/10.1021/acscatal.5b00538 },
}")
      (bibtex-mode)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (goto-char (point-min))
      (orcb-clean-doi)
      (bibtex-autokey-get-field "doi")))))

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
	   (insert-file-contents (expand-file-name
				  "test/test-1.bib"
				  (file-name-directory
				   (locate-library "org-ref"))))
	   (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
	   (bibtex-search-entry "kitchin-2015-examp"))))))

(ert-deftest bib-2 ()
  "Test for null entry"
  (should
   (null (with-temp-buffer
	   (insert-file-contents (expand-file-name
				  "test/test-1.bib"
				  (file-name-directory
				   (locate-library "org-ref"))))
	   (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
	   (bibtex-search-entry "bad-key")))))

(ert-deftest bad-ref ()
  (should
   (= 5
      (length
       (org-test-with-temp-text
	   "ref:bad1  ref:bad2 eqref:bad3 pageref:bad4 nameref:bad5"
	 (org-ref-bad-ref-candidates))))))

(ert-deftest bad-file-link ()
  (should
   (= 5
      (length
       (org-test-with-temp-text
	   "
file:not.here  [[./or.here]].

We should catch  \\attachfile{latex.style} too.

Why don't we catch [[attachfile:filepath]] or attachfile:some.file?
I think they must be defined in jmax, and are unknown links if it is
not loaded.
"
	 (org-add-link-type "attachfile" nil nil)
	 (org-ref-bad-file-link-candidates))))))

(ert-deftest cite-export-1 ()
  (should
   (string=
    "\\cite{kitchin-2008-alloy}
"
    (org-export-string-as "cite:kitchin-2008-alloy" 'latex t))))

(ert-deftest cite-export-2 ()
  (should
   (string=
    "\\cite[page 2]{kitchin-2008-alloy}
"
    (org-export-string-as "[[cite:page 2;&kitchin-2008-alloy]]" 'latex t))))

(ert-deftest cite-export-3 ()
  (should
   (string=
    "\\cite[page 2][post text]{kitchin-2008-alloy}
"
    (org-export-string-as  "[[cite:page 2;&kitchin-2008-alloy;post text]]" 'latex t))))

(ert-deftest label-export-1 ()
  (should
   (string=
    "\\label{test}
"
    (org-export-string-as "label:test" 'latex t))))

(ert-deftest ref-export-1 ()
  (should
   (string=
    "\\ref{test}
"
    (org-export-string-as "ref:test" 'latex t))))

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

(ert-deftest bib-string ()
  (should
   (equal
    (let ((bibtex-completion-bibliography "test.bib"))
      (org-test-with-temp-text
	  ""
	(org-ref-find-bibliography)))
    '("test.bib"))))

(ert-deftest bib-list ()
  (should
   (equal
    (let ((bibtex-completion-bibliography '("test.bib")))
      (org-test-with-temp-text
	  ""
	(org-ref-find-bibliography)))
    '("test.bib"))))

(ert-deftest bad-cite-1 ()
  (should (= 1 (length (let ((bibtex-completion-bibliography "test-1.bib"))
			 (org-test-with-temp-text
			     "cite:tree"
			   (org-ref-bad-cite-candidates))))))
  )

(ert-deftest bad-cite-1a ()
  (should (= 0 (length (let ((bibtex-completion-bibliography (expand-file-name
							      "test/test-1.bib"
							      (file-name-directory (locate-library "org-ref")))))
			 (org-test-with-temp-text
			     "cite:&xu-2015-tunin-oxide"
			   (org-ref-bad-cite-candidates)))))))

(ert-deftest bad-cite-1b ()
  (should (= 0 (length (let ((bibtex-completion-bibliography (expand-file-name
							      "test/test-1.bib"
							      (file-name-directory (locate-library "org-ref")))))
			 (org-test-with-temp-text
			     "cite:xu-2015-tunin-oxide"
			   (org-ref-bad-cite-candidates))))))
  )

(ert-deftest bad-cite-1c ()
  (should (= 0 (length (let ((bibtex-completion-bibliography (list (expand-file-name
								    "test/test-1.bib"
								    (file-name-directory (locate-library "org-ref"))))))
			 (org-test-with-temp-text
			     "cite:&xu-2015-tunin-oxide"
			   (org-ref-bad-cite-candidates)))))))

(ert-deftest bad-cite-1d ()
  (should (= 0 (length (let ((bibtex-completion-bibliography (expand-file-name
							      "test/test-1.bib"
							      (file-name-directory (locate-library "org-ref")))))
			 (org-test-with-temp-text
			     "cite:xu-2015-tunin-oxide"
			   (org-ref-bad-cite-candidates))))))
  )

(ert-deftest v2-1 ()
  (should (string= "[[cite:&xu-2015-tunin-oxide]]"
		   (car (org-test-with-temp-text
			    "cite:xu-2015-tunin-oxide"
			  (org-ref-v2-cites-to-v3)))))
  )

(ert-deftest v2-2 ()
  (should (string= "[[citeauthor:&xu-2015-tunin-oxide]]"
		   (car (org-test-with-temp-text
			    "citeauthor:xu-2015-tunin-oxide"
			  (org-ref-v2-cites-to-v3)))))
  )

(ert-deftest v2-3 ()
  (should (string= "[[citep:prenote;&xu-2015-tunin-oxide;postnote]]"
		   (car (org-test-with-temp-text
			    "[[citep:xu-2015-tunin-oxide][prenote::postnote]]"
			  (org-ref-v2-cites-to-v3)))))
  )

(ert-deftest v2-4 ()
  (should (string= "[[citep:&key1;&key2]]"
		   (car (org-test-with-temp-text
			    "citep:key1,key2"
			  (org-ref-v2-cites-to-v3)))))
  )

(ert-deftest tooltip-1 ()
  (should
   (string= " Kitchin, J. R. (2021). Simple title. The Journal of Reproducible Science, 1(1), 1."
	    (with-temp-buffer
	      (org-mode)
	      (insert
	       (format
		"[[cite:&jk-2021]]
bibliography:%s"
		(expand-file-name
		 "test/test-1.bib"
		 (file-name-directory
		  (locate-library "org-ref")))))
	      (font-lock-fontify-buffer)
	      (goto-char 8)
	      (display-local-help)))))

(ert-deftest tooltip-2 ()
  (should
   (string= " Kitchin, J. R. (2021). Simple title. The Journal of Reproducible Science, 1(1), 1."
	    (with-temp-buffer
	      (org-mode)
	      (insert
	       (format
		"[[cite:&jk-2021]]
bibliography:%s"
		(expand-file-name
		 "test/test-1.bib"
		 (file-name-directory
		  (locate-library "org-ref")))))
	      (font-lock-fontify-buffer)
	      (goto-char 8)
	      (org-no-properties (org-ref-cite-tooltip (selected-window) (selected-frame) 8))))))

(ert-deftest key-cursor ()
  (should
   (string= "jk-2021"
	    (with-temp-buffer
	      (org-mode)
	      (insert
	       (format
		"[[cite:&jk-2021]]
bibliography:%s"
		(expand-file-name
		 "test/test-1.bib"
		 (file-name-directory
		  (locate-library "org-ref")))))
	      (font-lock-fontify-buffer)
	      (goto-char 8)
	      (org-ref-get-bibtex-key-under-cursor)))))

