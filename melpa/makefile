EMACS ?= emacs
EMACS29 ?= /usr/local/Cellar/emacs-plus\@29/29.0.50/Emacs.app/Contents/MacOS/Emacs
CASK ?= cask
CASK_EXEC ?= ${CASK} exec

clean:
	rm -fr sandbox


ivy:
	${EMACS} -q -Q -l init-ivy.el test-1.org

ivy29:
	${EMACS29} -q -Q -l init-ivy.el test-1.org

helm:
	${EMACS} -q -Q -l init-helm.el test-1.org
