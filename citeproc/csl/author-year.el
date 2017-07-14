;;; author-year.el --- Citation Style Lisp - the other CSL


;;; Commentary:
;;

;;; Code:

(setq citation-style
  '((label . orcp-citation-author-year-label)
    (prefix . "(")
    (suffix . ")")
    (delimiter . "; ")
    (citeauthor . ((vertical-align . baseline)
		   (label . orcp-citation-author-label)
		   (prefix . "")
		   (suffix . " ")))
    (citeyear . ((vertical-align . baseline)
		 (label . orcp-citation-year-label)
		 (prefix . "")
		 (suffix . " ")
		 (chomp-leading-space . nil)
		 ))))



(setq bibliography-style
  '((sort . nil)
    (hanging-indent . 3)
    (justification . full)
    (spacing . 1)
    (label . orcp-citation-author-year-label)
    (label-prefix . "(")
    (label-suffix . ") ")
    (header . ((text . "Bibliography")
	       (font-style . bold)))
    ;; Formatting of fields
    ;; Single author name
    (author . ((initialize . t)		; use initials, not full names
	       ;; use firstname and lastname symbols
	       (name-order . (firstname lastname))
	       (name-separator . " ")
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
    (booktitle . ((font-style . italics)
		  (suffix . "")
		  (field-separator . "in ")))
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
		(inproceedings . (author title booktitle year))
		(book . (author title year))
		(manual . (author title url doi))
		(misc . (author title url doi))
		(techreport . (author title institution year))
		(mastersthesis . (author title school year))
		(phdthesis . (author title school year))
		(t . (author title year))))))

(provide 'author-year)

;;; author-year.el ends here
