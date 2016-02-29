(ert-deftest fl-next-cite ()
  (org-test-with-temp-text
      "   cite:kitchin-2015-examp

bibliography:tests/test-1.bib
"
    (goto-char (point-min))
    (org-ref-match-next-cite-link nil)
    (should
     (= 27 (point)))))

(ert-deftest fl-next-ref ()
  (org-test-with-temp-text
      "   ref:one
"
    (goto-char (point-min))
    (org-ref-match-next-ref-link nil)
    (should
     (= 11 (point)))))

(ert-deftest fl-next-label ()
  (org-test-with-temp-text
      "   label:one
"
    (goto-char (point-min))
    (org-ref-match-next-label-link nil)
    (should
     (= 13 (point)))))

(ert-deftest fl-next-bib ()
  (org-test-with-temp-text
      "   bibliography:one
"
    (goto-char (point-min))
    (org-ref-match-next-bibliography-link nil)
    (should
     (= 20 (point)))))

(ert-deftest fl-next-bibstyle ()
  (org-test-with-temp-text
      "   bibliographystyle:one
"
    (goto-char (point-min))
    (org-ref-match-next-bibliographystyle-link nil)
    (should
     (= 25 (point)))))
