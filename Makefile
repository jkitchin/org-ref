emacs ?= emacs
CASK ?= cask
CASK_EXEC ?= ${CASK} exec
EL_SOURCES = *.el
SOURCES =   ${EL_SOURCES}

all: test

test: clean
	${CASK_EXEC} ert-runner

unit:
	${CASK_EXEC} ${emacs} -Q -batch -L "." -l org -l org-ref.el -l test/org-ref-test.el --eval "(ert t)"



ert-runner:
	${CASK_EXEC} exec ert-runner

mytest:
	${CASK_EXEC} ${emacs} -Q -batch  -l ../init.el  -l test/org-test-setup.el -l test/org-ref-test.el -f ert-run-tests-batch-and-exit

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

no-user:
	${CASK_EXEC} ${emacs} -Q --eval="(setq jmax-load-user-dir nil)" -l ../init.el

revtex:
	${CASK_EXEC} ${emacs} -Q --eval="(setq jmax-load-user-dir nil)"  --eval="(setq org-ref-completion-library 'org-ref-helm-reftex)" -l ../init.el tests/test-1.org

helm-bibtex:
	${CASK_EXEC} ${emacs} -Q --eval="(setq jmax-load-user-dir nil)" --eval="(setq org-ref-completion-library 'org-ref-helm-bibtex)" -l ../init.el tests/test-1.org

helm-cite:
	${CASK_EXEC} ${emacs} -Q --eval="(setq jmax-load-user-dir nil)"  --eval="(setq org-ref-completion-library 'org-ref-helm-cite)" -l ../init.el tests/test-1.org

ivy:
	${CASK_EXEC} ${emacs} -Q --eval="(setq jmax-load-user-dir nil)"  --eval="(setq org-ref-completion-library 'org-ref-ivy-cite)" -l ../init.el tests/test-1.org

vanilla:
	${CASK_EXEC} ${emacs} -Q  -l ../init.el tests/test-1.org

.PHONY:	all test package clean-elc test-melpa
