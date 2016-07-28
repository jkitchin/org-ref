(dolist (package (list 'lispy 'magit))
  (unless (package-installed-p package)
    (message "installing %s" package)
    (package-install package)))

(require 'lispy)
(require 'magit)

(show-paren-mode 1)         ;; highlight parentheses
(setq show-paren-style 'mixed) ;; alternative is 'expression,
			       ;; 'parenthesis or 'mixed

(setq backup-inhibited t)  ;; disable backup file creation

(fset 'yes-or-no-p 'y-or-n-p) ; answer with y/n instead of yes/no
(load-theme 'leuven)

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (lispy-mode)
	    (eldoc-mode)))
