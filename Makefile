emacs ?= emacs
CASK ?= cask
CASK_EXEC ?= ${CASK} exec
EL_SOURCES = org-ref.el arxiv.el jmax-bibtex.el doi-utils.el isbn.el pubmed.el
SOURCES =   ${EL_SOURCES}

all: test

test: clean-elc
	${MAKE} unit

unit:
	${CASK_EXEC} ${emacs} -Q -batch -L "." -l org -l org-ref.el -l org-ref-test.el --eval "(ert t)"

compile:
	${CASK_EXEC} ${emacs} -Q -batch -L "." -f batch-byte-compile *.el

clean-elc:
	rm -f *.elc

release:
	git checkout melpa
	git merge master
	git push origin melpa
	git checkout master


package : ${SOURCES}
	${CASK} package

.PHONY:	all test package clean-elc clean-tangled
