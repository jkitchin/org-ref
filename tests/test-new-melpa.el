(setq user-emacs-directory "./sandbox")
(require 'package)

(setq package-archives
      '(("org" . "http://orgmode.org/elpa/")
	("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.org/packages/")))

(package-initialize)
(package-refresh-contents)

(package-install 'org-ref)

(setq org-ref-bibliography-notes "./notes.org"
      org-ref-default-bibliography '("./references.bib")
      org-ref-pdf-directory "./bibtex-pdfs/")

(unless (file-exists-p org-ref-pdf-directory)
  (make-directory org-ref-pdf-directory t))

(setq org-src-fontify-natively t
      org-confirm-babel-evaluate nil
      org-src-preserve-indentation t)

(org-babel-do-load-languages
 'org-babel-load-languages '((python . t)))

(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f" 
	"bibtex %b"
	"pdflatex -interaction nonstopmode -output-directory %o %f" 
	"pdflatex -interaction nonstopmode -output-directory %o %f"))

(require 'org-ref)
(require 'org-ref-pdf)
(require 'org-ref-url-utils)
(require 'org-ref-latex)

(find-file "test-1.org")

(message "done")
