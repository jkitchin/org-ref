#!/bin/bash
# Run relint on org-ref codebase
# Usage: ./scripts/run-relint.sh [file.el]

set -e

EMACS=${EMACS:-emacs}

if [ -n "$1" ]; then
    # Run on specific file
    echo "Running relint on $1..."
    $EMACS --batch --eval "
    (progn
      (require 'package)
      (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\"))
      (package-initialize)
      (unless (package-installed-p 'relint)
        (message \"Installing relint...\")
        (package-refresh-contents)
        (package-install 'relint))
      (require 'relint)
      (relint-file \"$1\"))"
else
    # Run on all .el files
    echo "Running relint on all .el files in current directory..."
    $EMACS --batch --eval "
    (progn
      (require 'package)
      (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\"))
      (package-initialize)
      (unless (package-installed-p 'relint)
        (message \"Installing relint...\")
        (package-refresh-contents)
        (package-install 'relint))
      (require 'relint)
      (relint-directory \".\"))"
fi
