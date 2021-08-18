
;; * Miscellaneous links

;;** List of figures

;;;###autoload
(defun org-ref-list-of-figures (&optional arg)
  "Generate buffer with list of figures in them.
ARG does nothing.
Ignore figures in COMMENTED sections."
  (interactive)
  (save-excursion
    (widen)
    (let* ((c-b (buffer-name))
	   (counter 0)
	   (list-of-figures
	    (org-element-map (org-ref-parse-buffer) 'link
	      (lambda (link)
		"create a link for to the figure"
		(when
		    (and (string= (org-element-property :type link) "file")
			 (string-match-p
			  "[^.]*\\.\\(png\\|jpg\\|eps\\|pdf\\|svg\\)$"
			  (org-element-property :path link))
			 ;; ignore commented sections
			 (save-excursion
			   (goto-char (org-element-property :begin link))
			   (not (or (org-in-commented-heading-p)
				    (org-at-comment-p)
				    (-intersection (org-get-tags) org-export-exclude-tags)))))
		  (cl-incf counter)

		  (let* ((start (org-element-property :begin link))
			 (linenum (progn (goto-char start) (line-number-at-pos)))
			 (fname (org-element-property :path link))
			 (parent (car (cdr
				       (org-element-property :parent link))))
			 (caption (cl-caaar (plist-get parent :caption)))
			 (name (plist-get parent :name)))

		    (if caption
			(format "[[file:%s::%s][Figure %s:]] %s\n" c-b linenum counter caption)
		      ;; if it has no caption, try the name
		      ;; if it has no name, use the file name
		      (cond (name
			     (format "[[file:%s::%s][Figure %s:]] %s\n" c-b linenum counter name))
			    (fname
			     (format "[[file:%s::%s][Figure %s:]] %s\n"
				     c-b linenum counter fname))))))))))
      (switch-to-buffer "*List of Figures*")
      (setq buffer-read-only nil)
      (org-mode)
      (erase-buffer)
      (insert (mapconcat 'identity list-of-figures ""))
      (goto-char (point-min))
      ;; open links in the same window
      (setq-local org-link-frame-setup
		  '((file . find-file)))
      (setq buffer-read-only t)
      (use-local-map (copy-keymap org-mode-map))
      (local-set-key "q" #'(lambda () (interactive) (kill-buffer))))))


(org-link-set-parameters "list-of-figures"
			 :follow #'org-ref-list-of-figures
			 :export (lambda (keyword desc format)
				   (cond
				    ((eq format 'latex)
				     (format "\\listoffigures")))))

;;** List of tables
;;;###autoload
(defun org-ref-list-of-tables (&optional arg)
  "Generate a buffer with a list of tables.
ARG does nothing."
  (interactive)
  (save-excursion
    (widen)
    (let* ((c-b (buffer-name))
           (counter 0)
           (list-of-tables
            (org-element-map (org-ref-parse-buffer 'element) 'table
              (lambda (table)
                "create a link for to the table"
		(save-excursion
		  (when
		      ;; ignore commented sections
		      (goto-char (org-element-property :begin table))
		    (not (or (org-in-commented-heading-p)
			     (-intersection (org-get-tags) org-export-exclude-tags)))
		    (cl-incf counter)
		    (let* ((start (org-element-property :begin table))
			   (linenum (progn (goto-char start) (line-number-at-pos)))
			   (caption (cl-caaar (org-element-property :caption table)))
			   (name (org-element-property :name table)))
		      (if caption
			  (format "[[file:%s::%s][Table %s:]] %s\n" c-b linenum counter caption)
			;; if it has no caption, try the name
			;; if it has no name, use generic name
			(cond (name
			       (format "[[file:%s::%s][Table %s:]] %s\n"
				       c-b linenum counter name))
			      (t
			       (format "[[file:%s::%s][Table %s:]] No caption\n"
				       c-b linenum counter)))))))))))
      (switch-to-buffer "*List of Tables*")
      (setq buffer-read-only nil)
      (org-mode)
      (erase-buffer)
      (insert (mapconcat 'identity list-of-tables ""))
      (goto-char (point-min))
      ;; open links in the same window
      (setq-local org-link-frame-setup
		  '((file . find-file)))
      (setq buffer-read-only t)
      (use-local-map (copy-keymap org-mode-map))
      (local-set-key "q" #'(lambda () (interactive) (kill-buffer))))))


(org-link-set-parameters "list-of-tables"
			 :follow #'org-ref-list-of-tables
			 :export (lambda (keyword desc format)
				   (cond
				    ((eq format 'latex)
				     (format "\\listoftables")))))
