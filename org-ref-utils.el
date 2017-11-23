;;; org-ref-utils.el --- Utility functions for org-ref  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;
(require 'org)
(require 'org-ref-pdf)  		; for pdftotext-executable

(defcustom org-ref-bib-html "<h1 class='org-ref-bib-h1'>Bibliography</h1>\n"
  "HTML header to use for bibliography in HTML export."
  :type 'string
  :group 'org-ref)

(defvar org-ref-cite-types)
(defvar org-ref-get-pdf-filename-function)
(defvar org-ref-notes-function)
(defvar org-ref-bibliography-entry-format)

(declare-function 'org-ref-get-bibtex-key-and-file "org-ref-core.el")
(declare-function 'org-ref-key-in-file-p "org-ref-core.el")
(declare-function 'org-ref-find-bibliography "org-ref-core.el")
(declare-function 'org-ref-bib-citation "org-ref-core.el")
(declare-function 'org-ref-get-bibtex-key-under-cursor "org-ref-core.el")

;;; Code:
;;;###autoload
(defun org-ref-version ()
  "Provide a version string for org-ref.
Copies the string to the clipboard."
  (interactive)
  ;; version in the el file.
  (let* ((org-ref-el (concat
		      (file-name-sans-extension
		       (locate-library "org-ref"))
		      ".el"))
	 (org-ref-dir (file-name-directory org-ref-el))
	 org-version
	 git-commit
	 version-string)

    (setq org-version (with-temp-buffer
			(insert-file-contents org-ref-el)
			(goto-char (point-min))
			(re-search-forward ";; Version:")
			(s-trim (buffer-substring (point)
						  (line-end-position)))))

    (setq git-commit
	  ;; If in git, get current commit
	  (let ((default-directory org-ref-dir))
	    (when (= 0 (shell-command "git rev-parse --git-dir"))
	      (s-trim (shell-command-to-string "git rev-parse HEAD")))))

    (setq version-string (format "org-ref: Version %s%s" org-version
				 (if git-commit
				     (format " (git-commit %s)" git-commit)
				   "")))
    (kill-new version-string)
    (message version-string)))


(defun org-ref-report-issue ()
  "Report an issue in org-ref.
Opens https://github.com/jkitchin/org-ref/issues/new."
  (save-window-excursion
    (org-ref-debug)
    (kill-new (buffer-string)))
  (message "org-ref-debug has been run. You can paste the results in the issue website if you like.")
  (browse-url "https://github.com/jkitchin/org-ref/issues/new"))


;;* Debug(require 'org-ref-pdf)
(defmacro ords (&rest body)
  "Evaluate BODY and return a string."
  `(format "%s" (progn ,@body)))


;;;###autoload
(defun org-ref-debug ()
  "Print some debug information to a buffer."
  (interactive)
  (switch-to-buffer "*org-ref-debug*")
  (erase-buffer)
  (org-mode)
  (insert
   (s-format "#+TITLE: org-ref debug

${org-ref-version}

* Variables
1. org-ref-completion-library: ${org-ref-completion-library}
2. org-ref-bibliography-notes: ${org-ref-bibliography-notes} (exists ${orbn-p})
3. org-ref-default-bibliography: ${org-ref-default-bibliography} (exists ${ordb-p}) (listp ${ordb-listp})
4. org-ref-pdf-directory: ${org-ref-pdf-directory} (exists ${orpd-p})

* System
system-type: ${system}
system-configuration: ${system-configuration}
window system: ${window-system}
Emacs: ${emacs-version}
org-version: ${org-version}

* about org-ref
org-ref installed in ${org-ref-location}.

** Dependencies
helm-bibtex ${helm-bibtex-path}

* org-ref-pdf (loaded: ${org-ref-pdf-p})
system pdftotext: ${pdftotext}
You set pdftotext-executable to ${pdftotext-executable} (exists: ${pdftotext-executable-p})

* org-ref-url-utils (loaded: ${org-ref-url-p})

* export variables
org-latex-pdf-process:
${org-latex-pdf-process}
"
	     'aget
	     `(("org-ref-completion-library" . ,(format "%s" org-ref-completion-library))
	       ("org-ref-bibliography-notes" . ,(format "%s"  org-ref-bibliography-notes))
	       ("orbn-p" . ,(format "%s" (file-exists-p org-ref-bibliography-notes)))
	       ("org-ref-version" . ,(org-ref-version))
	       ("org-latex-pdf-process" . ,(format "%S" org-latex-pdf-process))
	       ("org-ref-default-bibliography" . ,(format "%s" org-ref-default-bibliography))
	       ("ordb-p" . ,(format "%s" (mapcar 'file-exists-p org-ref-default-bibliography)))
	       ("ordb-listp" . ,(ords (listp org-ref-default-bibliography)))
	       ("org-ref-pdf-directory" . ,(format "%s" org-ref-pdf-directory))
	       ("orpd-p" . ,(format "%s" (file-exists-p org-ref-pdf-directory)))
	       ("org-ref-location" . ,(format "%s" (locate-library "org-ref")))

	       ("system" . ,(format "System: %s" system-type))
	       ("system-configuration" . ,(ords system-configuration))
	       ("window-system" . ,(format "Window system: %s" window-system))
	       ("emacs-version" . ,(ords (emacs-version)))
	       ("org-version" . ,(org-version))

	       ("helm-bibtex-path" . ,(ords (locate-library "helm-bibtex")))

	       ("org-ref-pdf-p" . ,(ords (featurep 'org-ref-pdf)))
	       ("pdftotext" . ,(ords (if (featurep 'org-ref-pdf)
					 (executable-find "pdftotext")
				       "org-ref-pdf not loaded")))
	       ("pdftotext-executable" . ,(ords (if (featurep 'org-ref-pdf)
						    pdftotext-executable
						  "org-ref-pdf not loaded")))
	       ("pdftotext-executable-p" . ,(ords (if (featurep 'org-ref-pdf)
						      (or
						       (executable-find pdftotext-executable)
						       (file-exists-p pdftotext-executable))
						    "org-ref-pdf not loaded")))
	       ("org-ref-url-p" . ,(ords (featurep 'org-ref-url)))))))



(defun org-ref-reftex-get-bib-field (field entry &optional format)
  "Get FIELD from a bibtex ENTRY in optional FORMAT.
Similar to `reftex-get-bib-field', but removes enclosing braces
and quotes in FIELD in the bibtex ENTRY."
  (let ((result))
    (setq result (reftex-get-bib-field field entry format))
    (when (and (not (string= result "")) (string= "{" (substring result 0 1)))
      (setq result (substring result 1 -1)))
    (when (and (not (string= result "")) (string= "\"" (substring result 0 1)))
      (setq result (substring result 1 -1)))
    result))

(defun org-ref-reftex-format-citation (entry format)
  "Format the bibtex ENTRY according to the FORMAT argument.
ENTRY is from `bibtex-parse-entry'
The FORMAT is a string with these percent escapes.

In the format, the following percent escapes will be expanded.

%l   The BibTeX label of the citation.
%a   List of author names, see also `reftex-cite-punctuation'.
%2a  Like %a, but abbreviate more than 2 authors like Jones et al.
%A   First author name only.
%e   Works like %a, but on list of editor names.  (%2e and %E work as well)

It is also possible to access all other BibTeX database fields:
%b booktitle     %c chapter        %d edition    %h howpublished
%i institution   %j journal        %k key        %m month
%n number        %o organization   %p pages      %P first page
%r address       %s school         %u publisher  %t title
%v volume        %y year
%B booktitle, abbreviated          %T title, abbreviated
%U url
%D doi
%S series        %N note

%f pdf filename
%F absolute pdf filename

Usually, only %l is needed.  The other stuff is mainly for the echo area
display, and for (setq reftex-comment-citations t).

%< as a special operator kills punctuation and space around it after the
string has been formatted.

A pair of square brackets indicates an optional argument, and RefTeX
will prompt for the values of these arguments.

Beware that all this only works with BibTeX database files.  When
citations are made from the \bibitems in an explicit thebibliography
environment, only %l is available."
  ;; Format a citation from the info in the BibTeX ENTRY
  (unless (stringp format) (setq format "\\cite{%l}"))

  (if (and reftex-comment-citations
           (string-match "%l" reftex-cite-comment-format))
      (error "Reftex-cite-comment-format contains invalid %%l"))

  (while (string-match
          "\\(\\`\\|[^%]\\)\\(\\(%\\([0-9]*\\)\\([a-zA-Z]\\)\\)[.,;: ]*\\)"
          format)
    (let ((n (string-to-number (match-string 4 format)))
          (l (string-to-char (match-string 5 format)))
          rpl b e)
      (save-match-data
        (setq rpl
              (cond
               ((= l ?l) (concat
                          (org-ref-reftex-get-bib-field "&key" entry)
                          (if reftex-comment-citations
                              reftex-cite-comment-format
                            "")))
               ((= l ?a) (replace-regexp-in-string
                          "\n\\|\t\\|\s+" " "
                          (reftex-format-names
                           (reftex-get-bib-names "author" entry)
                           (or n 2))))
               ((= l ?A) (replace-regexp-in-string
                          "\n\\|\t\\|\s+" " "
                          (car (reftex-get-bib-names "author" entry))))
               ((= l ?b) (org-ref-reftex-get-bib-field "booktitle" entry "in: %s"))
               ((= l ?B) (reftex-abbreviate-title
                          (org-ref-reftex-get-bib-field "booktitle" entry "in: %s")))
               ((= l ?c) (org-ref-reftex-get-bib-field "chapter" entry))
               ((= l ?d) (org-ref-reftex-get-bib-field "edition" entry))
               ((= l ?D) (org-ref-reftex-get-bib-field "doi" entry))
               ((= l ?e) (reftex-format-names
                          (reftex-get-bib-names "editor" entry)
                          (or n 2)))
               ((= l ?E) (car (reftex-get-bib-names "editor" entry)))
	       ((= l ?f) (concat (org-ref-reftex-get-bib-field "=key=" entry) ".pdf"))
	       ((= l ?F) (concat org-ref-pdf-directory (org-ref-reftex-get-bib-field "=key=" entry) ".pdf"))
               ((= l ?h) (org-ref-reftex-get-bib-field "howpublished" entry))
               ((= l ?i) (org-ref-reftex-get-bib-field "institution" entry))
               ((= l ?j) (let ((jt (reftex-get-bib-field "journal" entry)))
                           (if (string= "" jt)
                               (reftex-get-bib-field "journaltitle" entry)
                             jt)))
               ((= l ?k) (org-ref-reftex-get-bib-field "=key=" entry))
               ((= l ?m) (org-ref-reftex-get-bib-field "month" entry))
               ((= l ?n) (org-ref-reftex-get-bib-field "number" entry))
	       ((= l ?N) (org-ref-reftex-get-bib-field "note" entry))
               ((= l ?o) (org-ref-reftex-get-bib-field "organization" entry))
               ((= l ?p) (org-ref-reftex-get-bib-field "pages" entry))
               ((= l ?P) (car (split-string
                               (org-ref-reftex-get-bib-field "pages" entry)
                               "[- .]+")))
               ((= l ?s) (org-ref-reftex-get-bib-field "school" entry))
               ((= l ?S) (org-ref-reftex-get-bib-field "series" entry))
               ((= l ?u) (org-ref-reftex-get-bib-field "publisher" entry))
               ((= l ?U) (org-ref-reftex-get-bib-field "url" entry))
               ((= l ?r) (org-ref-reftex-get-bib-field "address" entry))
               ;; strip enclosing brackets from title if they are there
               ((= l ?t) (replace-regexp-in-string
                          "\n\\|\t\\|\s+" " "
                          (org-ref-reftex-get-bib-field "title" entry)))
               ((= l ?T) (reftex-abbreviate-title
                          (replace-regexp-in-string
                           "\n\\|\t\\|\s+" " "
                           (org-ref-reftex-get-bib-field "title" entry))))
               ((= l ?v) (org-ref-reftex-get-bib-field "volume" entry))
               ((= l ?y) (org-ref-reftex-get-bib-field "year" entry)))))

      (if (string= rpl "")
          (setq b (match-beginning 2) e (match-end 2))
        (setq b (match-beginning 3) e (match-end 3)))
      (setq format (concat (substring format 0 b) rpl (substring format e)))))
  (while (string-match "%%" format)
    (setq format (replace-match "%" t t format)))
  (while (string-match "[ ,.;:]*%<" format)
    (setq format (replace-match "" t t format)))
  format)


(defun org-ref-get-bibtex-entry-citation (key)
  "Return a string for the bibliography entry corresponding to KEY.
Format according to the type in `org-ref-bibliography-entry-format'."

  (let ((org-ref-bibliography-files (org-ref-find-bibliography))
        (file) (entry) (bibtex-entry) (entry-type) (format))

    (setq file (catch 'result
                 (cl-loop for file in org-ref-bibliography-files do
                          (if (org-ref-key-in-file-p key (file-truename file))
                              (throw 'result file)
                            (message "%s not found in %s"
                                     key (file-truename file))))))

    (with-temp-buffer
      (insert-file-contents file)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (bibtex-search-entry key nil 0)
      (setq bibtex-entry (bibtex-parse-entry))
      ;; downcase field names so they work in the format-citation code
      (dolist (cons-cell bibtex-entry)
        (setf (car cons-cell) (downcase (car cons-cell))))
      (setq entry-type (downcase (cdr (assoc "=type=" bibtex-entry))))
      (setq format (cdr (assoc entry-type org-ref-bibliography-entry-format)))
      (if format
          (setq entry  (org-ref-reftex-format-citation bibtex-entry format))
        ;; if no format, we use the bibtex entry itself as a fallback
        (save-restriction
          (bibtex-narrow-to-entry)
          (setq entry (buffer-string)))))
    entry))


;;*** key at point functions
(defun org-ref-get-pdf-filename (key)
  "Return the pdf filename associated with a bibtex KEY.
If `org-ref-pdf-directory' is non-nil, put filename there."
  (if org-ref-pdf-directory
      (let ((pdf (-first 'f-file?
			 (--map (f-join it (concat key ".pdf"))
				(-flatten (list org-ref-pdf-directory))))))
	(format "%s" pdf))
    (format "%s.pdf" key)))


(defun org-ref-get-mendeley-filename (key)
  "Return the pdf filename indicated by mendeley file field.
Falls back to `org-ref-get-pdf-filename' if file field does not exist.
Contributed by https://github.com/autosquid.
Argument KEY is the bibtex key."
  (let* ((results (org-ref-get-bibtex-key-and-file key))
         (bibfile (cdr results))
         entry)
    (with-temp-buffer
      (insert-file-contents bibfile)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (bibtex-search-entry key nil 0)
      (setq entry (bibtex-parse-entry))
      (let ((e (org-ref-reftex-get-bib-field "file" entry)))
        (if (> (length e) 4)
            (let ((clean-field (replace-regexp-in-string "{\\|}\\|\\\\" "" e)))
              (let ((first-file (car (split-string clean-field ";" t))))
                (format "/%s" (substring first-file 1
					 (- (length first-file) 4)))))
          (format (concat
                   (file-name-as-directory org-ref-pdf-directory)
                   "%s.pdf")
                  key))))))

(defun org-ref-get-pdf-filename-helm-bibtex (key)
  "Use helm-bibtex to retrieve a PDF filename for KEY.
helm-bibtex looks in both the configured directory
`bibtex-completion-library-path' and in the fields of the bibtex
item for a filename. It understands file fields exported by
Jabref, Mendeley and Zotero. See `bibtex-completion-find-pdf'."
  (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
    (or (car (bibtex-completion-find-pdf key)) "")))


;;;###autoload
(defun org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (pdf-file (funcall org-ref-get-pdf-filename-function key)))
    (if (file-exists-p pdf-file)
        (org-open-file pdf-file)
      (message "no pdf found for %s" key))))


;;;###autoload
(defun org-ref-open-url-at-point ()
  "Open the url for bibtex key under point."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (bibfile (cdr results)))
    (save-excursion
      (with-temp-buffer
        (insert-file-contents bibfile)
        (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
        (bibtex-search-entry key)
        ;; I like this better than bibtex-url which does not always find
        ;; the urls
        (catch 'done
          (let ((url (s-trim (bibtex-autokey-get-field "url"))))
            (unless (s-blank? url)
              (browse-url url)
              (throw 'done nil)))

          (let ((doi (s-trim (bibtex-autokey-get-field "doi"))))
            (unless (s-blank? doi)
              (if (string-match "^http" doi)
                  (browse-url doi)
                (browse-url (format "http://dx.doi.org/%s" doi)))
              (throw 'done nil))))))))


;;;###autoload
(defun org-ref-open-notes-at-point (&optional thekey)
  "Open the notes for bibtex key under point in a cite link in a buffer.
Can also be called with THEKEY in a program."
  (interactive)
  (funcall org-ref-notes-function thekey))


;;;###autoload
(defun org-ref-citation-at-point ()
  "Give message of current citation at point."
  (interactive)
  (org-ref-format-entry (org-ref-get-bibtex-key-under-cursor))
  ;; (let* ((results (org-ref-get-bibtex-key-and-file))
  ;;        (key (car results))
  ;;        (bibfile (cdr results)))
  ;;   (message "%s" (progn
  ;;                   (with-temp-buffer
  ;;                     (insert-file-contents bibfile)
  ;;                     (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
  ;;                     (bibtex-search-entry key)
  ;;                     (org-ref-bib-citation)))))
  )


;;;###autoload
(defun org-ref-open-citation-at-point ()
  "Open bibtex file to key at point."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (bibfile (cdr results)))
    (find-file bibfile)
    (bibtex-search-entry key)))

;;*** cite menu
(defvar org-ref-cite-menu-funcs '()
  "Functions to run on cite click menu.
Each entry is a list of (key menu-name function).  The function
must take no arguments and work on the key at point.  Do not
modify this variable, it is set to empty in the menu click
function, and functions are conditionally added to it.")


(defvar org-ref-user-cite-menu-funcs
  '(("C" "rossref" org-ref-crossref-at-point)
    ("y" "Copy entry to file" org-ref-copy-entry-at-point-to-file)
    ("s" "Copy summary" org-ref-copy-entry-as-summary))
  "User-defined functions to run on bibtex key at point.")


;;;###autoload
(defun org-ref-copy-entry-as-summary ()
  "Copy the bibtex entry for the citation at point as a summary."
  (interactive)
  (kill-new (org-ref-bib-citation)))


;;;###autoload
(defun org-ref-copy-entry-at-point-to-file ()
  "Copy the bibtex entry for the citation at point to NEW-FILE.
Prompt for NEW-FILE includes bib files in
`org-ref-default-bibliography', and bib files in current working
directory.  You can also specify a new file."
  (interactive)
  (let ((new-file (completing-read
                   "Copy to bibfile: "
                   (append org-ref-default-bibliography
                           (f-entries "." (lambda (f) (f-ext? f "bib"))))))
        (key (org-ref-get-bibtex-key-under-cursor)))
    (save-window-excursion
      (org-ref-open-citation-at-point)
      (bibtex-copy-entry-as-kill))

    (let ((bibtex-files (list (file-truename new-file))))
      (if (assoc key (bibtex-global-key-alist))
          (message "That key already exists in %s" new-file)
        ;; add to file
        (save-window-excursion
          (find-file new-file)
          (goto-char (point-max))
          ;; make sure we are at the beginning of a line.
          (unless (looking-at "^") (insert "\n\n"))
          (bibtex-yank)
          (save-buffer))))))


(defun org-ref-get-doi-at-point ()
  "Get doi for key at point."
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (bibfile (cdr results))
         doi)
    (save-excursion
      (with-temp-buffer
        (insert-file-contents bibfile)
        (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
        (bibtex-search-entry key)
        (setq doi (bibtex-autokey-get-field "doi"))
        ;; in case doi is a url, remove the url part.
        (replace-regexp-in-string "^http://dx.doi.org/" "" doi)))))


;;**** functions that operate on key at point for click menu
;;;###autoload
(defun org-ref-wos-at-point ()
  "Open the doi in wos for bibtex key under point."
  (interactive)
  (doi-utils-wos (org-ref-get-doi-at-point)))


;;;###autoload
(defun org-ref-wos-citing-at-point ()
  "Open the doi in wos citing articles for bibtex key under point."
  (interactive)
  (doi-utils-wos-citing (org-ref-get-doi-at-point)))


;;;###autoload
(defun org-ref-wos-related-at-point ()
  "Open the doi in wos related articles for bibtex key under point."
  (interactive)
  (doi-utils-wos-related (org-ref-get-doi-at-point)))


;;;###autoload
(defun org-ref-google-scholar-at-point ()
  "Search google scholar for bibtex key under point using the title."
  (interactive)
  (browse-url
   (format
    "http://scholar.google.com/scholar?q=%s"
    (let* ((key-file (org-ref-get-bibtex-key-and-file))
	   (key (car key-file))
	   (file (cdr key-file))
	   entry)
      (with-temp-buffer
	(insert-file-contents file)
	(bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
	(bibtex-search-entry key nil 0)
	(setq entry (bibtex-parse-entry))
	(org-ref-reftex-get-bib-field "title" entry))))))


;;;###autoload
(defun org-ref-pubmed-at-point ()
  "Open the doi in pubmed for bibtex key under point."
  (interactive)
  (doi-utils-pubmed (org-ref-get-doi-at-point)))


;;;###autoload
(defun org-ref-crossref-at-point ()
  "Open the doi in crossref for bibtex key under point."
  (interactive)
  (doi-utils-crossref (org-ref-get-doi-at-point)))


;;* General org-ref utilities
(defun org-ref-strip-string (string)
  "Strip leading and trailing whitespace from the STRING."
  (replace-regexp-in-string
   (concat search-whitespace-regexp "$" ) ""
   (replace-regexp-in-string
    (concat "^" search-whitespace-regexp ) "" string)))


(defun org-ref-split-and-strip-string (string)
  "Split key-string and strip keys in STRING.
Assumes the key-string is comma delimited."
  (mapcar 'org-ref-strip-string (split-string string ",")))


(defun org-ref-get-bibtex-keys (&optional sort)
  "Return a list of unique keys in the buffer.
Use SORT to specify alphabetical order by key."
  (let ((keys '()))
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (let ((plist (nth 1 link)))
          (when (-contains? org-ref-cite-types (plist-get plist ':type))
            (dolist
                (key
                 (org-ref-split-and-strip-string (plist-get plist ':path)))
              (when (not (-contains? keys key))
                (setq keys (append keys (list key))))))))
      ;; set with-affiliated to get keys in captions
      nil nil nil t)
    (when sort
      ;; Sort keys alphabetically
      (setq keys (cl-sort keys 'string-lessp :key 'downcase)))
    keys))


;;;###autoload
(defun org-ref-bibliography (&optional sort)
  "Create a new buffer with a bibliography.
If SORT is non-nil it is alphabetically sorted by key
This is mostly for convenience to see what has been cited.
Entries are formatted according to the bibtex entry type in
`org-ref-bibliography-entry-format', and the actual entries are
generated by `org-ref-reftex-format-citation'."
  (interactive)
  (let ((bib (mapconcat
              'identity
              (cl-loop for i from 1
		       for citation in
		       (mapcar
			(lambda (key)
			  (let* ((results (org-ref-get-bibtex-key-and-file key))
				 (key (car results))
				 (bibfile (cdr results)))
			    (format "cite:%s %s" key
				    (if bibfile
					(save-excursion
					  (with-temp-buffer
					    (insert-file-contents bibfile)
					    (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
					    (bibtex-search-entry key)
					    (org-ref-bib-citation)))
				      "!!! No entry found !!!"))))
			(org-ref-get-bibtex-keys sort))
		       collect (format "%3s. %s" i citation))
              "\n\n")))

    (switch-to-buffer-other-window (format "%s-bibliography" (buffer-file-name)))
    (erase-buffer)
    (insert bib)
    (org-mode)))


(defun org-ref-get-bibtex-entry-html (key)
  "Return an html string for the bibliography entry corresponding to KEY."
  (let ((output))
    (setq output (org-ref-get-bibtex-entry-citation key))
    ;; unescape the &
    (setq output (replace-regexp-in-string "\\\\&" "&" output))
    ;; hack to replace {} around text
    (setq output (replace-regexp-in-string "{" "" output))
    (setq output (replace-regexp-in-string "}" "" output))
    ;; get rid of empty parens
    (setq output (replace-regexp-in-string "()" "" output))
    ;; get rid of empty link and doi
    (setq output (replace-regexp-in-string " <a href=\"\">link</a>\\." "" output))
    ;; change double dash to single dash
    (setq output (replace-regexp-in-string "--" "-" output))
    (setq output (replace-regexp-in-string " <a href=\"http://dx\\.doi\\.org/\">doi</a>\\." "" output))
    (format "<li><a id=\"%s\">[%s]</a> %s</li>"
            key key output)))


(defun org-ref-get-html-bibliography (&optional sort)
  "Create an html bibliography when there are keys.
If SORT is non-nil the bibliography is alphabetically sorted."
  (let ((keys (org-ref-get-bibtex-keys sort)))
    (when keys
      (concat org-ref-bib-html "<ul class='org-ref-bib'>"
              (mapconcat (lambda (x) (org-ref-get-bibtex-entry-html x)) keys "\n")
              "\n</ul>"))))


(defun org-ref-get-bibtex-entry-org (key)
  "Return an org string for the bibliography entry corresponding to KEY."
  (let ((org-ref-bibliography-files (org-ref-find-bibliography))
	file entry)

    (setq file (catch 'result
                 (cl-loop for file in org-ref-bibliography-files do
                          (if (org-ref-key-in-file-p key (file-truename file))
                              (throw 'result file)
                            (message "%s not found in %s" key
				     (file-truename file))))))

    (with-temp-buffer
      (insert-file-contents file)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (bibtex-search-entry key nil 0)
      (setq entry (bibtex-parse-entry))
      (format "** %s - %s
  :PROPERTIES:
  %s
  :END:
" (org-ref-reftex-get-bib-field "author" entry)
(org-ref-reftex-get-bib-field "title" entry)
(concat "   :CUSTOM_ID: " (org-ref-reftex-get-bib-field "=key=" entry) "\n"
        (mapconcat
	 (lambda (element) (format "   :%s: %s"
				   (upcase (car element))
				   (cdr element)))
	 entry
	 "\n"))))))


(defun org-ref-get-org-bibliography (&optional sort)
  "Create an org bibliography when there are keys.
If SORT is non-nil the bibliography is sorted alphabetically by key."
  (let ((keys (org-ref-get-bibtex-keys sort)))
    (when keys
      (concat "* Bibliography
"
              (mapconcat (lambda (x)
			   (org-ref-get-bibtex-entry-org x)) keys "\n")
              "\n"))))


(defun org-ref-get-bibtex-entry-ascii (key)
  "Return an ascii string for the bibliography entry corresponding to KEY."

  (format "[%s] %s" key (org-ref-get-bibtex-entry-citation key)))


(defun org-ref-get-ascii-bibliography (&optional sort)
  "Create an ascii bibliography when there are keys.
if SORT is non-nil the bibliography is sorted alphabetically by key."
  (let ((keys (org-ref-get-bibtex-keys sort)))
    (when keys
      (concat
       "Bibliography
=============
"
       (mapconcat (lambda (x) (org-ref-get-bibtex-entry-ascii x)) keys "\n")
       "\n"))))

(defun org-ref-get-odt-bibliography (&optional sort)
  "Create an ascii bibliography ofr odt export when there are keys.
if SORT is non-nil the bibliography is sorted alphabetically by
key.  This is a variant of `org-ref-get-ascii-bibliography' where
some things are escaped since odt is an xml format."
  (let ((keys (org-ref-get-bibtex-keys sort)))
    (when keys
      (mapconcat (lambda (x)
		   (xml-escape-string (org-ref-get-bibtex-entry-ascii x)))
		 keys "\n"))))

(defun org-ref-pdf-p (filename)
  "Check if FILENAME is PDF file.

From the PDF specification 1.7:

    The first line of a PDF file shall be a header consisting of
    the 5 characters %PDF- followed by a version number of the
    form 1.N, where N is a digit between 0 and 7."
  (let ((header (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert-file-contents-literally filename nil 0 5)
                  (buffer-string))))
    (string-equal (encode-coding-string header 'utf-8) "%PDF-")))

;;;###autoload
(defmacro org-ref-link-set-parameters (type &rest parameters)
  "Set link TYPE properties to PARAMETERS."
  (declare (indent 1))
  (if (fboundp 'org-link-set-parameters)
      `(org-link-set-parameters ,type ,@parameters)
    `(org-add-link-type ,type ,(plist-get parameters :follow) ,(plist-get parameters :export))))

(provide 'org-ref-utils)
;;; org-ref-utils.el ends here
