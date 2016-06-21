;;; unsrt-footnote.el --- Citation Style Lisp - the other CSL


;;; Commentary:
;; Convert citations to org-mode footnotes

;;; Code:

(setq citation-style
  '((label . orcp-footnote-label)
    (prefix . "")
    (suffix . "")
    ;; sort on increasing citation numbers.
    (sort . (lambda (key1 key2)
	      (let ((i1 (-find-index
			 (lambda (entry)
			   (string= key1 (car entry)))
			 *orcp-unique-entries*))
		    (i2 (-find-index
			 (lambda (entry)
			   (string= key2 (car entry)))
			 *orcp-unique-entries*)))
		(> i2 i1))))
    (delimiter . ", ")
    (vertical-align . baseline)))


(setq bibliography-style
  '((sort . nil)
    (hanging-indent . 3)
    (justification . full)
    (spacing . 1)
    (label . orcp-footnote-label)
    (label-prefix . "")
    (label-suffix . " ")
    (header . ((text . "Bibliography")
	       (font-style . bold)))
    ;; Formatting of fields
    ;; Single author name
    (author . ((initialize . t)		; use initials, not full names
	       (name-order . (lastname firstname))
	       (name-separator . ", ")
	       (et-al . 4)		; after 4 authors use et-al
	       (delimiter . "; ")
	       (last-author-delimiter . " and ")
	       (suffix . "")
	       (field-separator . ", ")
	       ;; ; function to convert (first von last jr) to a string.)
	       (name-format . ''format-author-name)
	       (field-separator ", ")))
    (title . ((font-style . italics)
	      (suffix . "")
	      (field-separator . ", ")))
    (journal . ((suffix . "")
		(field-separator . ", ")))
    ;; here we use some logic to group volume(issue) or volume
    (volume . ((suffix . (when (orcp-get-entry-field "number" entry)
			   (orcp-issue entry)))
	       (field-separator . ", ")))
    (issue . ((font-style . bold)
	      (prefix . "(")
	      (suffix . ")")
	      (field-separator . ", ")))
    (pages . ((prefix . "pp. ")
	      (suffix . "")
	      (field-separator . " ")
	      (collapse-range . nil)))
    (year . ((prefix . "(")
	     (suffix . ")")
	     (field-separator . ".")))
    (doi . ((prefix . " ")
	    (suffix . ".")
	    (field-separator . "")
	    (formatter . orcp-doi-formatter)))
    ;; Formatting of entries
    (entries . ((article . (author title journal volume pages year doi))
		(book . (author title year))
		(misc . (author title url doi))
		(techreport . (author title institution year))
		(mastersthesis . (author title school year))
		(phdthesis . (author title school year))
		(t . (author title year))))))

(provide 'unsrt)

;;; unsrt.el ends here
