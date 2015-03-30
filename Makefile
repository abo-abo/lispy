EMACS ?= emacs

CASK = ~/.cask/bin/cask
BEMACS = $(EMACS) -batch -l elpa.el
LOAD = -l lispy-inline.el -l lispy.el
QEMACS = $(EMACS) -Q -l elpa.el -l targets/base-init.el

all: test

cask:
	$(shell EMACS=$(EMACS) $(CASK))

compile:
	$(BEMACS) $(LOAD) -l targets/compile.el

check-declare:
	$(BEMACS) $(LOAD) -l targets/check-declare.el

test:
	$(BEMACS) -l lispy-test.el $(LOAD) -f ert-run-tests-batch-and-exit

elisp:
	$(QEMACS) lispy.el

clojure:
	$(QEMACS) ~/git/incanter/modules/incanter-core/src/incanter/bayes.clj

scheme:
	$(QEMACS) ~/Dropbox/source/scheme/script3.scm -f lispy-mode

lisp:
	$(QEMACS) ~/Dropbox/source/site-lisp/git/slime/metering.lisp

clean:
	rm -f *.elc

.PHONY: all clean cask elisp clojure lisp scheme check-declare
