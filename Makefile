emacs ?= emacs
CASK ?= cask
CASK_EXEC ?= ${CASK} exec
EL_SOURCES = *.el
SOURCES =   ${EL_SOURCES}

all: test

test: clean
	${MAKE} unit

unit:
	${CASK_EXEC} ${emacs} -Q -batch -L "." -l org -l org-ref.el -l org-ref-test.el --eval "(ert t)"

mytest:
	${CASK_EXEC} ${emacs} -Q -batch  -l ../init.el  -l tests/org-test.el -l org-ref-test.el -f ert-run-tests-batch-and-exit

compile:
	${CASK_EXEC} ${emacs} -Q -batch -l ../init.el -L "." -f batch-byte-compile *.el

clean:
	rm -f *.elc

github:
	open http://github.com/jkitchin/org-ref

travis:
	open https://travis-ci.org/jkitchin/org-ref

melpa:
	open https://melpa.org/#/org-ref


package : ${SOURCES}
	${CASK} package

.PHONY:	all test package clean-elc test-melpa
