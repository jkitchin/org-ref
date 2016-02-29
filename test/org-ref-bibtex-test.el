;; Run all tests (progn (eval-buffer) (ert t))


(ert-deftest short-titles ()
  (org-ref-bibtex-generate-shorttitles)
  (should
   (file-exists-p "shorttitles.bib"))
  (delete-file "shorttitles.bib"))

(ert-deftest long-titles ()
  (org-ref-bibtex-generate-longtitles)
  (should
   (file-exists-p "longtitles.bib"))
  (delete-file "longtitles.bib"))


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
