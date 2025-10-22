emacs ?= emacs
CASK ?= cask
CASK_EXEC ?= ${CASK} exec
EL_SOURCES = *.el
SOURCES =   ${EL_SOURCES}

INIT = test/init.el
INIT-DEVEL = test/init-devel.el

all: test

github-actions:
	git clone https://github.com/cask/cask ~/.cask
	PATH=${HOME}/.cask/bin:${PATH} cask install
	PATH=${HOME}/.cask/bin:${PATH} ${CASK_EXEC} ert-runner


test: clean
	${CASK_EXEC} ert-runner

# Run all test-*.el files directly (without cask/ert-runner dependencies)
# This is useful for CI environments or local testing without Cask
test-direct: clean
	${emacs} -batch -L . -L test -l test/run-tests.el

unit:
	${CASK_EXEC} ${emacs} -Q -batch -L "." -l ${INIT} -l org -l org-ref.el -l test/org-ref-test.el --eval "(ert t)"


TEST_FILES = $(wildcard test/*-test.el)

mytest:
	${CASK_EXEC} ${emacs} -Q -batch -l ${INIT}  -l test/org-test-setup.el $(patsubst %,-l %,${TEST_FILES}) -f ert-run-tests-batch-and-exit

compile:
	${CASK} build

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
	${CASK_EXEC} ${emacs} -Q --eval="(setq scimax-load-user-dir nil)" -l ${INIT}


ivy:
	${CASK_EXEC} ${emacs} -Q  \
	--eval="(setq scimax-load-user-dir nil)" \
	-l ${INIT} \
	tests/test-1.org

devel:
	${CASK_EXEC} ${emacs} -Q  \
	--eval="(setq scimax-load-user-dir nil)" \
	-l ${INIT} \
	-l ${INIT-DEVEL}

vanilla:
	${CASK_EXEC} ${emacs} -Q  -l ${INIT} tests/test-1.org

# Run relint to check for regexp issues
relint:
	@echo "Running relint on all .el files..."
	@${emacs} --batch --eval "\
	(progn \
	  (require 'package) \
	  (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\")) \
	  (package-initialize) \
	  (unless (package-installed-p 'relint) \
	    (message \"Installing relint...\") \
	    (package-refresh-contents) \
	    (package-install 'relint)) \
	  (require 'relint) \
	  (message \"Checking *.el files...\") \
	  (relint-directory \".\"))" 2>&1 | grep -v "^Loading"

# Run relint on a specific file
# Usage: make relint-file FILE=org-ref-bibtex.el
relint-file:
	@${emacs} --batch --eval "\
	(progn \
	  (require 'package) \
	  (package-initialize) \
	  (unless (package-installed-p 'relint) \
	    (package-refresh-contents) \
	    (package-install 'relint)) \
	  (require 'relint) \
	  (relint-file \"$(FILE)\"))" 2>&1

.PHONY:	all test test-direct package clean-elc test-melpa unit mytest relint relint-file
