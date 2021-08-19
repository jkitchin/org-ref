;;; org-ref-label-link.el ---

;;; Commentary:
;;

;;; Code:

(defcustom org-ref-label-color
  "dark magenta"
  "Color of label links."
  :type 'string
  :group 'org-ref)


(defface org-ref-label-face
  `((t (:inherit org-link :foreground ,org-ref-label-color)))
  "Color for ref links in `org-ref'.")


;;** label link (maybe deprecated)

(defun org-ref-count-labels (label)
  "Count number of LABELs in the document."
  (+ (count-matches
      (format "label:%s\\( \\|]\\|$\\)" (regexp-quote label))
      (point-min) (point-max))
     (count-matches
      (format "<<%s>>" label)
      (point-min) (point-max))
     ;; for tblname, it is not enough to get word boundary
     ;; tab-little and tab-little-2 match then.
     (count-matches
      (format "^\\( \\)*#\\+tblname:\\s-*%s\\b[^-:]" label)
      (point-min) (point-max))
     (count-matches (format "\\label{%s}" label)
                    (point-min) (point-max))
     ;; this is the org-format #+label:
     (count-matches (format "^\\( \\)*#\\+label:\\s-*%s\\b[^-:]" label)
                    (point-min) (point-max))
     ;; #+name:
     (count-matches (format "^\\( \\)*#\\+name:\\s-*%s\\b[^-:]" label)
		    (point-min) (point-max))
     (let ((custom-id-count 0))
       (org-map-entries
        (lambda ()
          (when (string= label (org-entry-get (point) "CUSTOM_ID"))
            (setq custom-id-count (+ 1 custom-id-count)))))
       custom-id-count)))


(defun org-ref-label-store-link ()
  "Store a link to a label.  The output will be a ref to that label.
This has several conditional ways to store a link to figures and
tables also. Note it does not currently work with latex labels,
only org labels and names."
  ;; First we have to make sure we are on a label link.
  (let* ((object (and (eq major-mode 'org-mode) (org-element-context)))
	 (stored nil)
	 label)
    (cond
     ;; here literally on a label link.
     ((and
       (equal (org-element-type object) 'link)
       (equal (org-element-property :type object) "label"))
      (setq label (org-element-property :path object))
      (org-store-link-props
       :type "ref"
       :link (concat "ref:" label)))

     ;; here on a file link that probably contains an image, although I don't check that
     ((and
       (equal (org-element-type object) 'link)
       (equal (org-element-property :type object) "file")
       (org-file-image-p (org-element-property :path object)))

      (if (org-element-property :name object)
	  (progn
	    (setq label (org-element-property :name object))
	    (org-store-link-props
	     :type "ref"
	     :link (concat "ref:"label)))
	;; maybe we have a caption to get it from.
	(let* ((parent (org-element-property :parent object))
	       (caption))
	  (when (and parent
		     (equal (org-element-type parent) 'paragraph))
	    (if (org-element-property :name parent)
		;; caption paragraph may have a name which we use if it is there
		(setq label (org-element-property :name parent))
	      ;; else search caption
	      (setq caption (s-join
			     ""
			     (mapcar 'org-no-properties
				     (org-export-get-caption parent))))
	      (when (string-match org-ref-label-re caption)
		(setq label (match-string 1 caption))))

	    (org-store-link-props
	     :type "ref"
	     :link (concat "ref:" label))))))

     ;; here on a paragraph (eg in a caption of an image). it is a paragraph with a caption
     ;; in a caption, with no name, but maybe a label
     ((equal (org-element-type object) 'paragraph)
      (if (org-element-property :name object)
	  (org-store-link-props
	   :type "ref"
	   :link (concat "ref:" (org-element-property :name object)))
	;; See if it is in the caption name
	(let ((caption (s-join "" (mapcar 'org-no-properties
					  (org-export-get-caption object)))))
	  (when (string-match org-ref-label-re caption)
	    (setq label (match-string 1 caption))
	    (org-store-link-props
	     :type "ref"
	     :link (concat "ref:" label))))))

     ;; If you are in a table, we need to be at the beginning to make sure we get the name.
     ;; Note when in a caption it appears you are in a table but org-at-table-p is nil there.
     ((or (equal (org-element-type object) 'table) (org-at-table-p))
      (save-excursion
	(goto-char (org-table-begin))
	(let* ((table (org-element-context))
	       (label (org-element-property :name table))
	       (caption (s-join "" (mapcar 'org-no-properties (org-export-get-caption table)))))
	  (when (null label)
	    ;; maybe there is a label in the caption?
	    (when (string-match org-ref-label-re caption)
	      (setq label (match-string 1 caption))))

	  (org-store-link-props
	   :type "ref"
	   :link (concat "ref:" label)))))

     ;; and to #+label: lines
     ((and (equal (org-element-type object) 'paragraph)
           (org-element-property :name object))
      (setq label (org-element-property :name object))
      (org-store-link-props
       :type "ref"
       :link (concat "ref:" label)))

     ;; in a latex environment
     ((equal (org-element-type object) 'latex-environment)
      (let ((value (org-element-property :value object))
	    label)
	(when (string-match "\\\\label{\\(?1:[+a-zA-Z0-9:\\._-]*\\)}" value)
	  (setq label (match-string-no-properties 1 value))
	  (org-store-link-props
	   :type "ref"
	   :link (concat "ref:" label)))))

     (t
      nil))))


(defun org-ref-label-face-fn (label)
  "Return a face for the LABEL link."
  (save-match-data
    (cond
     ((or (not org-ref-show-broken-links)
	  (= 1 (org-ref-count-labels label)))
      'org-ref-label-face)
     (t
      'font-lock-warning-face))))


(org-link-set-parameters "label"
			 :follow (lambda (label)
				   "On clicking count the number of label tags used in the buffer.
A number greater than one means multiple labels!"
				   (let ((count (org-ref-count-labels label)))
				     (message (format "%s occurence%s"
						      count
						      (if (or (= count 0)
							      (> count 1))
							  "s"
							""))
					      (org-ref-count-labels label))))
			 :export (lambda (keyword desc format)
				   (cond
				    ((eq format 'html) (format "<div id=\"%s\"></div>" keyword))
				    ((eq format 'md) (format "<a name=\"%s\"></a>" keyword))
				    ((eq format 'latex)
				     (format "\\label{%s}" keyword))))
			 :store #'org-ref-label-store-link
			 :face 'org-ref-label-face-fn
			 :help-echo "A label")

(provide 'org-ref-label-link)

;;; org-ref-label-link.el ends here
