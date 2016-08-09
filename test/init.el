(setq user-emacs-directory "./elpa-for-orgref")
(require 'package)

(setq package-archives
      '(("org" . "http://orgmode.org/elpa/")
	("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.org/packages/")))

(package-initialize)
(package-refresh-contents)

(dolist (package (list 'org-plus-contrib 'dash 'helm 'helm-bibtex
		       'ivy 'hydra 'key-chord 's 'f 'lispy))
  (unless (package-installed-p package)
    (message "installing %s" package)
    (package-install package)))

(setq org-ref-bibliography-notes "./notes.org"
      org-ref-default-bibliography '("./references.bib")
      org-ref-pdf-directory "./bibtex-pdfs/")

(unless (file-exists-p org-ref-pdf-directory)
  (make-directory org-ref-pdf-directory t))

(setq org-src-fontify-natively t
      org-confirm-babel-evaluate nil
      org-confirm-elisp-link-function nil
      org-src-preserve-indentation t)

(org-babel-do-load-languages
 'org-babel-load-languages '((python . t)))


(add-to-list 'load-path
	     (file-name-directory
	      (directory-file-name
	       (file-name-directory
		(or load-file-name (buffer-file-name))))))

(require 'org-ref)


(message "done")
