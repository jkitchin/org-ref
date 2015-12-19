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

(require 'helm-config)
(require 'org-ref)
(require 'org-ref-pdf)
(require 'org-ref-url-utils)
(require 'org-ref-latex)

(find-file "test-1.org")

(message "done")
