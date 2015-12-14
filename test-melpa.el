(setq user-emacs-directory "./sandbox")
(require 'package)

(setq package-archives
      '(("gnu"         . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.org/packages/" )
	("sandbox" . "/Users/jkitchin/Dropbox/kitchingroup/jmax/melpa/packages/")))

(package-initialize)
(package-refresh-contents)

(package-install 'org-ref)

(find-file "tests/test-1.org")
