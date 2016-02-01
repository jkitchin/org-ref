(setq user-emacs-directory "./sandbox")
(require 'package)

(setq package-archives
      '(("org" . "http://orgmode.org/elpa/")
	("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.org/packages/")))

(package-initialize)

(require 'org-ref)
(require 'org-ref-latex)
(require 'org-ref-pdf)
(require 'org-ref-url-utils)

(find-file "test-1.org")

(message "done")
