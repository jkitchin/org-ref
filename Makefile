EMACS ?= emacs
CASK_EXEC ?= cask exec

all: test

test: clean-elc
	${MAKE} unit

unit:
	${CASK_EXEC} ${EMACS} -Q -batch  -l org-ref-init.el -l org-ref-test.el --eval "(ert t)"

compile:
	${CASK_EXEC} ${EMACS} -Q -batch -f batch-byte-compile org-ref-init.el

clean-elc:
	rm -f f.elc

.PHONY:	all test
