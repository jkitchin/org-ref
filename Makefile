EMACS ?= emacs
CASK ?= cask
CASK_EXEC ?= ${CASK} exec
ORG_SOURCES = doi-utils.org org-ref.org pubmed.org
EL_TANGLED = ${ORG_SOURCES:.org=.el}
EL_SOURCES = arxiv.el jmax-bibtex.el
SOURCES =  ${EL_TANGLED} ${EL_SOURCES}

all: test

test: clean-elc
	${MAKE} unit

unit:
	${CASK_EXEC} ${EMACS} -Q -batch -l org-ref.el -l org-ref-test.el --eval "(ert t)"

compile:
	${CASK_EXEC} ${EMACS} -Q -batch -f batch-byte-compile org-ref-init.el

clean-elc:
	rm -f f.elc

tangled:
	${MAKE} ${EL_TANGLED}

clean-tangled:
	rm ${EL_TANGLED}

package : ${SOURCES}
	${CASK} package

%.el: %.org
	${EMACS} -Q -batch $< -f org-babel-tangle

.PHONY:	all test package clean-elc clean-tangled
