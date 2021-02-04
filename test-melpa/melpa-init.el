(setq user-emacs-directory "./sandbox")
(require 'package)

(setq package-archives
      '(("org" . "https://orgmode.org/elpa/")
	("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(package-refresh-contents)

(package-install 'org-ref)

(require 'org-ref)
