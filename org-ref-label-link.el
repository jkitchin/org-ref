;;; org-ref-label-link.el ---

;;; Commentary:
;;

;;; Code:


(defface org-ref-label-face
  `((t (:inherit org-link :foreground "dark magenta")))
  "Color for ref links in `org-ref'.")


;;** label link (maybe deprecated)


(org-link-set-parameters
 "label"
 :export (lambda (keyword desc format)
	   (cond
	    ((eq format 'latex)
	     (format "\\label{%s}" keyword))))
 :face 'org-ref-label-face
 :help-echo "A label")

(provide 'org-ref-label-link)

;;; org-ref-label-link.el ends here
