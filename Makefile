emacs ?= emacs
CASK ?= cask
CASK_EXEC ?= ${CASK} exec
EL_SOURCES = *.el
SOURCES =   ${EL_SOURCES}

all: test

test: clean-elc
	${MAKE} unit

unit:
	${CASK_EXEC} ${emacs} -Q -batch -L "." -l org -l org-ref.el -l org-ref-test.el --eval "(ert t)"

mytest:
	${CASK_EXEC} ${emacs} -Q -batch  -l ../init.el -L "." -l org -l org-ref.el -l org-ref-test.el --eval "(ert t)"
compile:
	${CASK_EXEC} ${emacs} -Q -batch -l ../init.el -L "." -f batch-byte-compile *.el

clean-elc:
	rm -f *.elc

release:
	git checkout melpa
	git merge master
	git checkout master
	git push origin melpa

# this makes the package so I can test it.
melpa: release
	${MAKE} -C /Users/jkitchin/Dropbox/kitchingroup/jmax/melpa clean
	${MAKE} -C /Users/jkitchin/Dropbox/kitchingroup/jmax/melpa recipes/org-ref
	${MAKE} -C /Users/jkitchin/Dropbox/kitchingroup/jmax/melpa packages/archive-contents

test-melpa: melpa
	rm -fr sandbox
	${emacs} -Q -l tests/test-melpa.el


package : ${SOURCES}
	${CASK} package

.PHONY:	all test package clean-elc test-melpa
