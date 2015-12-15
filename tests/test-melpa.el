(setq user-emacs-directory "./sandbox")
(require 'package)

(setq package-archives

      '(("org" . "http://orgmode.org/elpa/")
	("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.org/packages/")
	("kitchingroup" . "https://raw.githubusercontent.com/jkitchin/melpa/kitchingroup/packages/")))

(package-initialize)
(package-refresh-contents)

(package-install 'org-ref)

(require 'org-ref)
(find-file "test-1.org")
