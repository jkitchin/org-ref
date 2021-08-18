;;; org-ref-citation-links.el --- citation links for org-ref

;;; Commentary:
;;

;;; Code:
(defface org-ref-cite-face
  `((t (:inherit org-link
                 :foreground "forest green")))
  "Color for cite-like links in org-ref.")



(defcustom org-ref-default-citation-link
  "cite"
  "The default type of citation link to use."
  :type 'string
  :group 'org-ref)

(defcustom org-ref-natbib-types
  '("citet" "citet*" "citep" "citep*"
    "citealt" "citealt*" "citealp" "citealp*"
    "citenum" "citetext"
    "citeauthor" "citeauthor*"
    "citeyear" "citeyear*" "citeyearpar"
    "Citet" "Citep" "Citealt" "Citealp" "Citeauthor")
  "natbib cite commands, http://tug.ctan.org/macros/latex/contrib/natbib/natnotes.pdf"
  :type '(repeat :tag "List of citation types" string)
  :group 'org-ref)


(defcustom org-ref-biblatex-types
  '("Cite"
    "parencite" "Parencite"
    "footcite" "footcitetext"
    "textcite" "Textcite"
    "smartcite" "Smartcite"
    "cite*" "parencite*" "supercite"
    "autocite" "Autocite" "autocite*" "Autocite*"
    "Citeauthor*"
    "citetitle" "citetitle*"
    "citedate" "citedate*"
    "citeurl"
    "fullcite" "footfullcite"
    ;; "volcite" "Volcite" cannot support the syntax
    "notecite" "Notecite"
    "pnotecite" "Pnotecite"
    "fnotecite"
    ;; multicites. Very limited support for these.
    "cites" "Cites" "parencites" "Parencites"
    "footcites" "footcitetexts"
    "smartcites" "Smartcites" "textcites" "Textcites"
    "supercites" "autocites" "Autocites")
  "biblatex commands
http://ctan.mirrorcatalogs.com/macros/latex/contrib/biblatex/doc/biblatex.pdf"
  :type '(repeat :tag "List of citation types" string)
  :group 'org-ref)


(defcustom org-ref-cite-types
  (append
   '("cite" "nocite") ;; the default latex cite commands
   org-ref-natbib-types
   org-ref-biblatex-types
   ;; for the bibentry package
   '("bibentry"))
  "List of citation types known in `org-ref'."
  :type '(repeat :tag "List of citation types" string)
  :group 'org-ref)


(defcustom org-ref-cite-keymap
  (let ((map (copy-keymap org-mouse-map)))
    (define-key map (kbd "H-o") 'org-ref-cite-hydra/body)
    (define-key map (kbd "H-b") 'org-ref-open-citation-at-point)
    (define-key map (kbd "H-u") 'org-ref-open-url-at-point)
    (define-key map (kbd "H-p") 'org-ref-open-pdf-at-point)
    (define-key map (kbd "H-n") 'org-ref-open-notes-at-point)
    (define-key map (kbd "H-r") 'org-ref-wos-related-at-point)
    (define-key map (kbd "H-c") 'org-ref-wos-citing-at-point)
    (define-key map (kbd "H-e") (lambda ()
				  "Email entry at point"
				  (interactive)
				  (org-ref-open-citation-at-point)
				  (org-ref-email-bibtex-entry)))
    (define-key map (kbd "H-g") 'org-ref-google-scholar-at-point)
    (define-key map (kbd "H-f") (lambda ()
				  (interactive)
				  (save-excursion
				    (org-ref-open-citation-at-point)
				    (kill-new
				     (org-ref-format-bibtex-entry-at-point)))))
    (define-key map (kbd "H-w") (lambda ()
				  (interactive)
				  (kill-new (car (org-ref-get-bibtex-key-and-file)))))
    (define-key map (kbd "H-W") (lambda ()
				  "Copy all the keys at point."
				  (interactive)
				  (kill-new (org-element-property :path (org-element-context)))))
    (define-key map (kbd "H-y") (lambda ()
				  "Paste key at point. Assumes the first thing in the killring is a key."
				  (interactive)
				  (org-ref-insert-key-at-point (car kill-ring))))

    ;; Navigation keys
    (define-key map (kbd "C-<left>") 'org-ref-previous-key)
    (define-key map (kbd "C-<right>") 'org-ref-next-key)

    ;; rearrangement keys
    (define-key map (kbd "S-<left>") (lambda () (interactive) (org-ref-swap-citation-link -1)))
    (define-key map (kbd "S-<right>") (lambda () (interactive) (org-ref-swap-citation-link 1)))
    (define-key map (kbd "S-<up>") 'org-ref-sort-citation-link)
    (define-key map (kbd "<tab>") (lambda () (interactive)
				    (funcall org-ref-insert-cite-function)))
    map)
  "Keymap for cite links."
  :type 'symbol
  :group 'org-ref)


(defface org-ref-cite-face
  `((t (:inherit org-link :foreground "forest green")))
  "Base face for cite-like links in `org-ref'.")


(defface org-ref-cite-global-prefix/suffix-face
  `((t (:inherit org-ref-cite-face :weight bold)))
  "Face for global prefix/suffix in a cite link.")


(defface org-ref-cite-local-prefix/suffix-face
  `((t (:inherit org-ref-cite-face :slant italic)))
  "FAce for local prefix/suffix in a cite link.")


(defvar org-ref-citation-key-re
  (rx "@" (group-n 1 (one-or-more (any word "-.:?!`'/*@+|(){}<>&_^$#%&~"))))
  "Numbered regular expression for a cite key.
Key is in group 1.")


(defun org-ref-cite-follow (_path)
  "Follow a cite link."
  (let ((key (get-text-property (point) 'cite-key)))
    (bibtex-completion-show-entry (list key))))


(defun org-ref-cite-activate (start end path _bracketp)
  "Activation function for a cite link.
START and END are the bounds of the link.
PATH has the citations in it."
  (let ((valid-keys (cl-loop for entry in (bibtex-completion-candidates)
			     collect (cdr (assoc "=key=" (cdr entry)))))
	(substrings (split-string path ";")))

    (goto-char start)
    (cl-loop for i from 0 for s in substrings
	     do
	     ;; get to the substring
	     (search-forward s end)
	     (let* (key-begin
		    key-end
		    key)

	       (save-match-data
		 (when (string-match org-ref-citation-key-re s)
		   (setq key (match-string-no-properties 1 s)
			 valid-key (member key valid-keys))))

	       ;; these are global prefix/suffixes
	       (when (and (or (= i 0)
			      (= i (- (length substrings) 1)))
			  (null key))
		 (put-text-property (match-beginning 0) (match-end 0) 'face 'org-ref-cite-global-prefix/suffix-face)
		 (put-text-property (match-beginning 0) (match-end 0) 'help-echo "Global prefix/suffix"))

	       ;; we have a key. we have to re-search to get its position
	       (when key
		 (save-excursion
		   (save-match-data
		     (search-backward (concat "@" key))
		     (setq key-begin (match-beginning 0)
			   key-end (match-end 0))))
		 ;; store key on the whole thing
		 (put-text-property (match-beginning 0)
				    (match-end 0)
				    'cite-key
				    key)

		 ;; fontify any prefix /suffix text
		 (put-text-property (match-beginning 0) key-begin
				    'face 'org-ref-cite-local-prefix/suffix-face)

		 (put-text-property key-end (match-end 0)
				    'face 'org-ref-cite-local-prefix/suffix-face)

		 ;; bad key activation
		 (unless valid-key
		   (put-text-property key-begin key-end
				      'face 'font-lock-warning-face)
		   (put-text-property key-begin key-end
				      'help-echo "Key not found")))))))


(defun org-ref-cite-tooltip (_win _obj position)
  "Get a tooltip for the cite at POSITION."
  (let ((key (get-text-property position 'cite-key)))
    (when key
      (bibtex-completion-apa-format-reference key))))


(defun org-ref-parse-cite-path (path)
  (let ((citation-references (split-string path ";"))
	(results ()))
    (when (null (string-match org-ref-citation-key-re (cl-first citation-references)))
      (setq results (plist-put results :prefix (pop citation-references))))
    (when (null (string-match org-ref-citation-key-re (car (last citation-references))))
      (setq results (plist-put results :suffix (car (last citation-references)))
	    citation-references (butlast citation-references)))

    (setq results (plist-put results
			     :references (cl-loop for s in citation-references collect
						  (if (null (string-match org-ref-citation-key-re s))
						      (error "No label found")
						    (let* ((key (match-string-no-properties 1 s))
							   (key-start (match-beginning 0))
							   (key-end (match-end 0))
							   (prefix (substring s 0 key-start))
							   (suffix (substring s key-end)))
						      (list :key key :prefix prefix :suffix suffix))))))))





(defun org-ref-natbib-export (path _desc backend)
  (pcase backend
    (latex
     (let ((cite (org-ref-parse-cite-path path)))
       (format "\\parencites(%s)(%s)%s"
	       (string-trim (or (plist-get cite :prefix) ""))
	       (string-trim (or (plist-get cite :suffix) ""))
	       (cl-loop for ref in (plist-get cite :references) concat
			(format "[%s][%s]{%s}"
				(string-trim (or (plist-get ref :prefix) ""))
				(string-trim (or (plist-get ref :suffix) ""))
				(plist-get ref :key))))))))

(defun org-ref-multicite-export (path _desc backend)
  (pcase backend
    (latex
     (let ((cite (org-ref-parse-cite-path path)))
       (format "\\parencites(%s)(%s)%s"
	       (string-trim (or (plist-get cite :prefix) ""))
	       (string-trim (or (plist-get cite :suffix) ""))
	       (cl-loop for ref in (plist-get cite :references) concat
			(format "[%s][%s]{%s}"
				(string-trim (or (plist-get ref :prefix) ""))
				(string-trim (or (plist-get ref :suffix) ""))
				(plist-get ref :key))))))))

(org-link-set-parameters
 "garencites"
 :follow #'org-ref-cite-follow
 :face 'org-ref-cite-face
 :help-echo #'org-ref-cite-tooltip
 :export #'org-ref-cite-export
 :activate-func #'org-ref-cite-activate)

(provide 'org-ref-citation-links)

;;; org-ref-citation-links.el ends here


;; * code to move

;; TODO delete
(defun org-ref-change-cite-type (new-type)
  "Change the cite type to NEW-TYPE."
  (interactive (list (completing-read "Type: " org-ref-cite-types)))
  (let* ((cite-link (org-element-context))
	 (old-type (org-element-property :type cite-link))
	 (begin (org-element-property :begin cite-link))
	 (end (org-element-property :end cite-link))
	 (bracketp (eq 'bracket (org-element-property :format cite-link)))
	 (path (org-element-property :path cite-link))
	 (deltap (- (point) begin)))
    ;; note this does not respect brackets
    (setf (buffer-substring begin end)
	  (concat
	   (if bracketp "[[" "")
	   new-type ":" path
	   (if bracketp "]]" "")))
    ;; try to preserve the character the point is on.
    (goto-char (+ begin deltap (- (length new-type) (length old-type))))))



(defvar org-ref-cite-re
  (concat "\\(" (mapconcat
                 (lambda (x)
		   (replace-regexp-in-string "\\*" "\\\\*" x))
                 org-ref-cite-types "\\|") "\\):"
                 "\\([a-zA-Z0-9_:\\./-]+,?\\)+")
  "Regexp for cite links.
Group 1 contains the cite type.
Group 2 contains the keys.")


;; This is probably a lot simpler
(defun org-ref-get-bibtex-key-under-cursor ()
  "Return key under the cursor in org-mode.
We search forward from point to get a comma, or the end of the link,
and then backwards to get a comma, or the beginning of the link. that
delimits the keyword we clicked on. We also strip the text
properties."
  (let* ((object (org-element-context))
	 (link-string (if (eq (org-element-type object) 'link)
                          (org-element-property :path object)
                        (org-in-regexp org-link-any-re)
			;; this is clunkier than I prefer, but some keys have
			;; colons in them, and this gets rid of the link type,
			;; then rejoins the rest of the keys
			(s-join ":" (cdr (split-string
					  (match-string-no-properties 0) ":"))))))
    ;; you may click on the part before the citations. here we make
    ;; sure to move to the beginning so you get the first citation.
    (let ((cp (point)))
      (goto-char (org-element-property :begin object))
      (search-forward link-string (org-element-property :end object))
      (goto-char (match-beginning 0))
      ;; check if we clicked before the path and move as needed.
      (unless (< cp (point))
	(goto-char cp)))

    (if (not (org-element-property :contents-begin object))
	;; this means no description in the link
	(progn
	  ;; we need the link path start and end
	  (let (link-string-beginning link-string-end)
	    (save-excursion
	      (goto-char (org-element-property :begin object))
	      (search-forward link-string nil nil 1)
	      (setq link-string-beginning (match-beginning 0))
	      (setq link-string-end (match-end 0)))

	    (let (key-beginning key-end)
	      ;; The key is the text between commas, or the link boundaries
	      (save-excursion
		(if (search-forward "," link-string-end t 1)
		    (setq key-end (- (match-end 0) 1)) ; we found a match
		  (setq key-end link-string-end))) ; no comma found so take the end
	      ;; and backward to previous comma from point which defines the start character
	      (save-excursion
		(if (search-backward "," link-string-beginning 1 1)
		    (setq key-beginning (+ (match-beginning 0) 1)) ; we found a match
		  (setq key-beginning link-string-beginning))) ; no match found
	      ;; save the key we clicked on.
	      (let ((bibtex-key
		     (org-ref-strip-string
		      (buffer-substring key-beginning key-end))))
		(set-text-properties 0 (length bibtex-key) nil bibtex-key)
		bibtex-key))))

      ;; link with description and multiple keys
      (if (and (org-element-property :contents-begin object)
	       (string-match "," link-string)
	       (equal (org-element-type object) 'link))
	  ;; point is not on the link description
	  (if (not (>= (point) (org-element-property :contents-begin object)))
	      (let (link-string-beginning link-string-end)
		(save-excursion
		  (goto-char (org-element-property :begin object))
		  (search-forward link-string nil t 1)
		  (setq link-string-beginning (match-beginning 0))
		  (setq link-string-end (match-end 0)))

		(let (key-beginning key-end)
		  ;; The key is the text between commas, or the link boundaries
		  (save-excursion
		    (if (search-forward "," link-string-end t 1)
			(setq key-end (- (match-end 0) 1)) ; we found a match
		      (setq key-end link-string-end))) ; no comma found so take the end
		  ;; and backward to previous comma from point which defines the start character

		  (save-excursion
		    (if (search-backward "," link-string-beginning 1 1)
			(setq key-beginning (+ (match-beginning 0) 1)) ; we found a match
		      (setq key-beginning link-string-beginning))) ; no match found
		  ;; save the key we clicked on.
		  (let ((bibtex-key
			 (org-ref-strip-string
			  (buffer-substring key-beginning key-end))))
		    (set-text-properties 0 (length bibtex-key) nil bibtex-key)
		    bibtex-key)))
	    ;; point is on the link description, assume we want the
	    ;; last key
	    (let ((last-key (replace-regexp-in-string "[a-zA-Z0-9_-]*," "" link-string)))
	      last-key))
	;; link with description. assume only one key
	link-string))))


;;* Links
;;*** Generation of the cite links
(defmacro org-ref-make-completion-function (type)
  "Macro to make a link completion function for a link of TYPE."
  `(defun ,(intern (format "org-%s-complete-link" type)) (&optional arg)
     (format
      "%s:%s"
      ,type
      (completing-read
       "bibtex key: "
       (let ((bibtex-files (org-ref-find-bibliography)))
	 (bibtex-global-key-alist))))))


(defmacro org-ref-make-format-function (type)
  "Macro to make a format function for a link of TYPE."
  `(defun ,(intern (format "org-ref-format-%s" type)) (keyword desc format)
     ,(format "Formatting function for %s links.\n[[%s:KEYWORD][DESC]]
FORMAT is a symbol for the export backend.
Supported backends: 'html, 'latex, 'ascii, 'org, 'md, 'pandoc" type type)
     (cond
      ((eq format 'org)
       (mapconcat
	(lambda (key)
	  (format "[[#%s][%s]]" key key))
	(org-ref-split-and-strip-string keyword) ","))

      ((eq format 'ascii)
       (concat "["
	       (mapconcat
		(lambda (key)
		  (format "%s" key))
		(org-ref-split-and-strip-string keyword) ",") "]"))

      ((eq format 'html)
       (mapconcat
	(lambda (key)
	  (format org-ref-ref-html key key))
	(org-ref-split-and-strip-string keyword) ","))

      ((eq format 'latex)
       (if (string= (substring ,type -1) "s")
	   ;; biblatex format for multicite commands, which all end in s. These
	   ;; are formated as \cites{key1}{key2}...
	   (concat "\\" ,type
		   (mapconcat (lambda (key) (format "{%s}" key))
			      (org-ref-split-and-strip-string keyword) ""))
	 ;; bibtex format
	 (concat "\\" ,type
		 (when desc (org-ref-format-citation-description desc)) "{"
		 (mapconcat
		  (lambda (key) key)
		  (org-ref-split-and-strip-string keyword) ",")
		 "}"))))))


(defun org-ref-format-citation-description (desc)
  "Return formatted citation description.
If the cite link has a DESC (description), it is optional text
for the citation command.  You can specify pre and post text by
separating these with ::, for example [[cite:key][pre text::post
text]]."
  (cond
   ((string-match "::" desc)
    (let ((results (split-string desc "::")))
      (format "[%s][%s]" (nth 0 results) (nth 1 results))))
   (t (format "[%s]" desc))))



;; This suppresses showing the warning buffer. bibtex-completion seems to make this
;; pop up in an irritating way.
(unless (boundp 'warning-suppress-types)
  (require 'warnings))


(add-to-list 'warning-suppress-types '(:warning))



(defun org-ref-cite-link-face-fn (keys)
  "Return a face for a cite link.
KEYS may be a comma-separated list of keys.
This is not smart enough yet to only highlight the bad key. If any key is bad, the whole cite will be red."

  (save-match-data
    (cond
     ((or (not org-ref-show-broken-links)
	  (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
	    (-every?
	     'identity
	     (mapcar
	      (lambda (key)
		(if (string= key "*")
		    t
		  (assoc "=key="
			 (bibtex-completion-get-entry key))))
	      (split-string keys ",")))))
      'org-ref-cite-face)
     (t
      'font-lock-warning-face))))


;;;###autoload
(defun org-ref-define-citation-link (type &optional key)
  "Add a citation link of TYPE for `org-ref'.
With optional KEY, set the reftex binding.  For example:
\(org-ref-define-citation-link \"citez\" ?z) will create a new
citez link, with reftex key of z, and the completion function."
  (interactive "sCitation Type: \ncKey: ")

  ;; create the formatting function
  (eval `(org-ref-make-format-function ,type))

  (org-link-set-parameters
   type
   :follow (lambda (_) (funcall org-ref-cite-onclick-function nil))
   :export (quote (intern (format "org-ref-format-%s" type)))
   :complete (quote (intern (format "org-%s-complete-link" type)))
   :help-echo (lambda (window object position)
		(when org-ref-show-citation-on-enter
		  (save-excursion
		    (goto-char position)
		    ;; Here we wrap the citation string to a reasonable size.
		    (let ((s (org-ref-format-entry
			      (org-ref-get-bibtex-key-under-cursor))))
		      (with-temp-buffer
			(insert s)
			(fill-paragraph)
			(buffer-string))))))
   :face 'org-ref-cite-link-face-fn
   :display 'full
   :keymap org-ref-cite-keymap)
  g


  ;; create the completion function
  (eval `(org-ref-make-completion-function ,type))

  ;; store new type so it works with adding citations, which checks
  ;; for existence in this list
  (add-to-list 'org-ref-cite-types type)

  (unless (assoc 'org reftex-cite-format-builtin)
    (add-to-list 'reftex-cite-format-builtin '(org "org-ref citations" ())))

  ;; and finally if a key is specified, we modify the reftex menu
  (when key
    (setf (nth 2 (assoc 'org reftex-cite-format-builtin))
          (append (nth 2 (assoc 'org reftex-cite-format-builtin))
                  `((,key  . ,(concat type ":%l")))))))


(defun org-ref-generate-cite-links ()
  "Create all the link types and their completion functions."
  (interactive)
  (dolist (type org-ref-cite-types)
    (org-ref-define-citation-link type))
  (when (fboundp 'org-link-set-parameters)
    (org-link-set-parameters "cite" :store #'org-ref-bibtex-store-link)))


;; This is what actually generated the cite links
(org-ref-generate-cite-links)


;;;###autoload
(defun org-ref-insert-cite-with-completion (type)
  "Insert a cite link of TYPE with completion."
  (interactive (list (completing-read "Type: " org-ref-cite-types)))
  (insert (funcall (intern (format "org-%s-complete-link" type)))))
