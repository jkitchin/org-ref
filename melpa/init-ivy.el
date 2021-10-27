(setq package-user-dir (expand-file-name "sandbox/"))
(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.org/packages/")))


(package-initialize)
(package-refresh-contents)

(package-install 'ivy-bibtex)
(package-install 'org-ref)

(setq bibtex-completion-bibliography
      (list (expand-file-name "test-1.bib")))


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
(require 'org-ref-ivy)

(setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
      org-ref-insert-cite-function 'org-ref-cite-insert-ivy
      org-ref-insert-label-function 'org-ref-insert-label-link
      org-ref-insert-ref-function 'org-ref-insert-ref-link
      org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body)))

(define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)
(define-key org-mode-map (kbd "s-[") 'org-ref-insert-link-hydra/body)

(find-file "test-1.org")
