(ert-deftest or-split-key-1 ()
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

(ert-deftest or-key-file-p ()
"Check `org-ref-key-in-file-p'"
(should
(not
(null
(org-ref-key-in-file-p "kitchin-2015-examp"
(expand-file-name
"tests/test-1.bib"
(file-name-directory (locate-library "org-ref"))))))))

(ert-deftest or-key-file-p-nil ()
"Check `org-ref-key-in-file-p' for non-existent key"
(should
(null
(org-ref-key-in-file-p "bad-key"
(expand-file-name
"tests/test-1.bib"
(file-name-directory (locate-library "org-ref")))))))

(ert-deftest or-key-file ()
"Check we find a key in a file"
(should
(equal
(cons "kitchin-2015-examp" (expand-file-name
"tests/test-1.bib"
(file-name-directory
(locate-library "org-ref"))))
(let ((org-ref-default-bibliography (list (expand-file-name
"tests/test-1.bib"
(file-name-directory
(locate-library "org-ref"))))))
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

(ert-deftest test-8 ()
(org-test-with-temp-text
(format "cite:kitchin-2015-examp 

bibliography:%s
" (expand-file-name
"tests/test-1.bib"
(file-name-directory
(locate-library "org-ref"))))
(should
(string=
(org-ref-link-message)
"Kitchin, J. R., Examples of effective data sharing in scientific publishing, ACS Catalysis, 5(6), 3894–3899 (2015).  http://dx.doi.org/10.1021/acscatal.5b00538"))))

(ert-deftest test-9 ()
(org-test-with-temp-text
(format "cite:kitchin-2015

bibliography:%s
"
(expand-file-name
"tests/test-1.bib"
(file-name-directory
(locate-library "org-ref"))))
(should 
(string= "!!! No entry found !!!"
(org-ref-link-message)))))

(ert-deftest orlm ()
(org-test-with-temp-text
(format "cite:kitchin-2015-examp

bibliography:%s
" (expand-file-name
"tests/test-1.bib"
(file-name-directory
(locate-library "org-ref"))))
(should
(string= (org-ref-link-message)
"Kitchin, J. R., Examples of effective data sharing in scientific publishing, ACS Catalysis, 5(6), 3894–3899 (2015).  http://dx.doi.org/10.1021/acscatal.5b00538"))))

(ert-deftest orlm-nil ()
(org-test-with-temp-text
(format "cite:kitchin-2015

bibliography:%s
" (expand-file-name
"tests/test-1.bib"
(file-name-directory
(locate-library "org-ref"))))
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
(string= "1 occurrence"
(org-ref-link-message)))))

(ert-deftest orlm-label-2 ()
(org-test-with-temp-text
"label:one

label:one

"
(should
(string= "2 occurrences"
(org-ref-link-message)))))

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
(expand-file-name
"tests/bibtex-pdfs/kitchin-2015.pdf"
(file-name-directory
(locate-library "org-ref"))) 
(org-test-with-temp-text
"cite:kitchin-2015"
(let ((org-ref-pdf-directory (expand-file-name
"tests/bibtex-pdfs/"
(file-name-directory
(locate-library "org-ref")))))
(org-ref-get-pdf-filename (org-ref-get-bibtex-key-under-cursor))))))
)

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
"test a single bibliography link."
(should
(equal
(list (expand-file-name
"tests/test-1.bib"
(file-name-directory
(locate-library "org-ref"))))
(org-test-with-temp-text
(format "bibliography:%s"
(expand-file-name
"tests/test-1.bib"
(file-name-directory
(locate-library "org-ref"))))
(org-ref-find-bibliography)))))

(ert-deftest orfb-1a ()
"Get multiple bib files."
(let ((bibstring ))
(should
(equal
(list (expand-file-name
"tests/test-1.bib"
(file-name-directory
(locate-library "org-ref")))
(expand-file-name
"tests/test-2.bib"
(file-name-directory
(locate-library "org-ref"))))
(org-test-with-temp-text
(format "bibliography:%s,%s"
(expand-file-name
"tests/test-1.bib"
(file-name-directory
(locate-library "org-ref")))
(expand-file-name
"tests/test-2.bib"
(file-name-directory
(locate-library "org-ref"))))
(org-ref-find-bibliography))))))

(ert-deftest orfb-2 ()
"Get bibfile in latex format."
(should
(equal
(list (expand-file-name
"tests/test-1.bib"
(file-name-directory
(locate-library "org-ref"))))
(org-test-with-temp-text
(format "
\\bibliography{%s}"
(file-name-sans-extension (expand-file-name
"tests/test-1.bib"
(file-name-directory
(locate-library "org-ref")))))
(org-ref-find-bibliography)))))

(ert-deftest orfb-2a ()
"Get bibfile in latex format."
(should
(equal
(list (expand-file-name
"tests/test-1.bib"
(file-name-directory
(locate-library "org-ref")))
(expand-file-name
"tests/test-2.bib"
(file-name-directory
(locate-library "org-ref"))))
(org-test-with-temp-text
(format "
\\bibliography{%s,%s}"
(file-name-sans-extension (expand-file-name
"tests/test-1.bib"
(file-name-directory
(locate-library "org-ref"))))
(file-name-sans-extension (expand-file-name
"tests/test-2.bib"
(file-name-directory
(locate-library "org-ref")))))
(org-ref-find-bibliography)))))

(ert-deftest orfb-3 ()
"addbibresource form of bibliography."
(should
(equal
(list (expand-file-name
"tests/test-1.bib"
(file-name-directory
(locate-library "org-ref"))))
(mapcar 'file-truename
(org-test-with-temp-text
(format "\\addbibresource{%s}"
(expand-file-name
"tests/test-1.bib"
(file-name-directory
(locate-library "org-ref"))))	      
(org-ref-find-bibliography))))))

(ert-deftest orfb-3a ()
"multiple bibliographies addbibresource form of bibliography."
(should
(equal
(list (expand-file-name
"tests/test-1.bib"
(file-name-directory
(locate-library "org-ref")))
(expand-file-name
"tests/test-2.bib"
(file-name-directory
(locate-library "org-ref"))))
(org-test-with-temp-text
(format "\\addbibresource{%s}
\\addbibresource{%s}"
(expand-file-name
"tests/test-1.bib"
(file-name-directory
(locate-library "org-ref")))
(expand-file-name
"tests/test-2.bib"
(file-name-directory
(locate-library "org-ref"))))	      
(org-ref-find-bibliography)))))

(ert-deftest orfb-4 ()
"getting default bibfile in file with no bib specification."
(should
(equal
(list (file-truename "test.bib"))
(mapcar 'file-truename
(org-test-with-temp-text
""
(let ((org-ref-default-bibliography '("test.bib")))
(org-ref-find-bibliography)))))))

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
(format
"cite:kitchin-2008-alloy

bibliography:%s
"
(expand-file-name
"tests/test-1.bib"
(file-name-directory
(locate-library "org-ref"))))
(org-ref-get-doi-at-point)))))

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

(ert-deftest get-year ()
(should
(string= "2015"
(org-test-with-temp-text
(format "bibliography:%s"
(expand-file-name
"tests/test-1.bib"
(file-name-directory
(locate-library "org-ref"))))
(org-ref-get-citation-year "kitchin-2015-examp")))))

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
"tests/test-1.bib"
(file-name-directory
(locate-library "org-ref"))))
(bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
(bibtex-search-entry "kitchin-2015-examp"))))))

(ert-deftest bib-2 ()
"Test for null entry"
(should
(null (with-temp-buffer
(insert-file-contents (expand-file-name
"tests/test-1.bib"
(file-name-directory
(locate-library "org-ref"))))
(bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
(bibtex-search-entry "bad-key")))))

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
| 3 |

** subsection \\label{three}
:PROPERTIES:
:CUSTOM_ID: two
:END: 

label:four
"
(org-ref-get-labels))))))

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
(string= (format
"cite:kitchin-2004-role,kitchin-2008-alloy

bibliography:%s
"
(expand-file-name
"tests/test-1.bib"
(file-name-directory (locate-library "org-ref")))) 
(org-test-with-temp-text
(format
"cite:kitchin-2008-alloy,kitchin-2004-role

bibliography:%s
"
(expand-file-name
"tests/test-1.bib"
(file-name-directory (locate-library "org-ref"))))
(org-ref-sort-citation-link)
(buffer-string)))))

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

(ert-deftest bib-export-2 ()
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
(format "
cite:bad

bibliography:%s
"
(expand-file-name
"tests/test-1.bib"
(file-name-directory (locate-library "org-ref"))))
(message "-------------------\n%S" (mapconcat
(lambda (x)
(file-name-directory (file-truename x)))
(org-ref-find-bibliography)		    ":"))
(org-ref-find-bad-citations)
(with-current-buffer "*Missing citations*"
(string-match "^bad \\[\\["
(buffer-substring-no-properties (point-min)
(point-max)))))))

(ert-deftest extract-bibtex ()
(should
(string-match "@article{kitchin-2015-examp,"
(org-test-with-temp-text
(format
"cite:kitchin-2015-examp

bibliography:%s
" (expand-file-name
"tests/test-1.bib"
(file-name-directory (locate-library "org-ref"))))
(org-ref-extract-bibtex-entries)
(buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest mendeley-fname ()
(should
(let ((bibstring (format "bibliography:%s"
(expand-file-name
"tests/test-1.bib"
(file-name-directory
(locate-library
"org-ref"))))))
(string= "/Users/jkitchin/Dropbox/bibliography/bibtex-pdfs/abild-pedersen-2007-scalin-proper.pdf"
(org-test-with-temp-text
bibstring	      
""
(org-ref-get-mendeley-filename "Abild-Pedersen2007"))))))

(ert-deftest fl-next-cite ()
(org-test-with-temp-text
"   cite:kitchin-2015-examp

bibliography:tests/test-1.bib
"
(goto-char (point-min))
(if (fboundp 'org-link-set-parameters)
t
(org-ref-match-next-cite-link nil)
(should
(= 27 (point))))))

(ert-deftest cite-face ()
(org-test-with-temp-text
"cite:kitchin-2015-examp

bibliography:tests/test-1.bib
"
(unless (fboundp 'org-link-set-parameters)
(font-lock-add-keywords
nil
'((org-ref-match-next-cite-link (0  'org-ref-cite-face t))
(org-ref-match-next-label-link (0  'org-ref-label-face t))
(org-ref-match-next-ref-link (0  'org-ref-ref-face t))
(org-ref-match-next-bibliography-link (0  'org-link t))
(org-ref-match-next-bibliographystyle-link (0  'org-link t)))
t)) 
(org-mode)
(font-lock-fontify-region (point-min) (point-max))
(describe-text-properties 1)
;; (should (eq 'org-ref-cite-face (get-char-property 1 'face)))
))

(ert-deftest cite-face ()
(org-test-with-temp-text
"# cite:kitchin-2015-examp

bibliography:tests/test-1.bib
"
(unless (fboundp 'org-link-set-parameters)
(font-lock-add-keywords
nil
'((org-ref-match-next-cite-link (0  'org-ref-cite-face t)))
t)) 
(font-lock-fontify-region (point-min) (point-max))
(should (not (eq 'org-ref-cite-face (get-char-property 5 'face))))))

(ert-deftest cite-in-comment ()
(should
(org-test-with-temp-text
"# cite:kitchin-2015-examp

bibliography:tests/test-1.bib
"
(font-lock-fontify-region (point-min) (point-max))
(eq 'font-lock-comment-face (get-char-property 10 'face)))))

(ert-deftest fl-next-ref ()
(org-test-with-temp-text
"   ref:one
"
(goto-char (point-min))
(if (fboundp 'org-link-set-parameters)
t
(org-ref-match-next-ref-link nil)
(should
(= 11 (point))))))

(ert-deftest ref-face ()
(org-test-with-temp-text
" ref:kitchin-2015-examp

bibliography:tests/test-1.bib
"
(unless (fboundp 'org-link-set-parameters) 
(font-lock-add-keywords
nil
'((org-ref-match-next-ref-link (0  'org-ref-ref-face t)))
t)) 
(font-lock-fontify-region (point-min) (point-max))
(should (eq 'org-ref-ref-face (get-char-property 2 'face)))))

(ert-deftest fl-next-label ()
(org-test-with-temp-text
"   label:one
"
(if (fboundp 'org-link-set-parameters)
t
(goto-char (point-min))
(org-ref-match-next-label-link nil)
(should
(= 13 (point))))))

(ert-deftest label-face ()
(org-test-with-temp-text
"label:kitchin-2015-examp

bibliography:tests/test-1.bib
"
(if (fboundp 'org-link-set-parameters)
t
(font-lock-add-keywords
nil
'((org-ref-match-next-label-link (0  'org-ref-label-face t)))
t)
(font-lock-fontify-region (point-min) (point-max))
(should (eq 'org-ref-label-face (get-char-property 2 'face))))))

(ert-deftest fl-next-bib ()
(org-test-with-temp-text
"   bibliography:one

stuff
"
(if (fboundp 'org-link-set-parameters)
t
(goto-char (point-min))
(org-ref-match-next-bibliography-link nil)
(should
(= 20 (point))))))

(ert-deftest fl-next-bibstyle ()
(org-test-with-temp-text
"   bibliographystyle:one

cite
"
(if (fboundp 'org-link-set-parameters)
t
(goto-char (point-min))
(org-ref-match-next-bibliographystyle-link nil)
(should
(= 25 (point))))))

(ert-deftest store-label-link ()
(org-test-with-temp-text
"label:test"
(goto-char 1)
(org-label-store-link)
(should
(string=
(plist-get org-store-link-plist :type) "ref"))))

(ert-deftest store-label-link-table ()
(org-test-with-temp-text
"#+tblname: test-table
|1 | 2|"
(goto-char 1)
(org-label-store-link)
(should
(string=
(plist-get org-store-link-plist :type) "ref"))
org-store-link-plist))

(ert-deftest store-label-headline ()
(org-test-with-temp-text
"* headline
:PROPERTIES:
:CUSTOM_ID: test
:END:
"
(goto-char 1)
(org-label-store-link)
(should
(string=
(plist-get org-store-link-plist :type) "custom_id"))))

(ert-deftest store-label-label ()
(org-test-with-temp-text
"#+LABEL: test
[[./file.png]]
"
(goto-char 1)
(org-label-store-link)
(should
(string=
(plist-get org-store-link-plist :type) "ref"))))

(ert-deftest store-bibtex-link ()
(should (string= "cite:kitchin-2015-examp"
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
(car (org-ref-store-bibtex-entry-link))))))

