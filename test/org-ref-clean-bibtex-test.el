(require 'ert)
(require 'org-ref)

(ert-deftest get-year ()
  (should
   (string= "2015"
	    (org-test-with-temp-text
		"bibliography:tests/test-1.bib"
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
