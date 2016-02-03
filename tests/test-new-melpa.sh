#!/bin/bash

rm -fr sandbox

exec emacs -q -l test-new-melpa.el test-1.org

#end
