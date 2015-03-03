EMACS = emacs
# EMACS = emacs-24.3

CASK = ~/.cask/bin/cask
CASKEMACS = $(CASK) exec $(EMACS)
LOAD = -l lispy-inline.el -l lispy.el

.PHONY: all clean cask elisp clojure lisp scheme check-declare

all: test

cask:
	$(shell EMACS=$(EMACS) $(CASK))

compile:
	$(CASKEMACS) -batch $(LOAD) -l lispy-test.el -l targets/compile.el

check-declare:
	$(CASKEMACS) -batch $(LOAD) -l targets/check-declare.el

test:
	${MAKE} unit

simpletest:
	$(EMACS) -batch -l elpa.el -l lispy-test.el $(LOAD) -f ert-run-tests-batch-and-exit

unit:
	$(EMACS) -batch -l elpa.el -l lispy-test.el $(LOAD) -f ert-run-tests-batch-and-exit

elisp:
	$(CASKEMACS) -q $(LOAD) lispy.el

clojure:
	$(CASKEMACS) -q $(LOAD) \
	~/git/incanter/modules/incanter-core/src/incanter/bayes.clj \
	--eval "(require 'clojure-mode)" -f clojure-mode

scheme:
	$(CASKEMACS) -q $(LOAD) \
	~/Dropbox/source/scheme/script3.scm

lisp:
	$(CASKEMACS) -q $(LOAD) \
	~/git/slime/metering.lisp

clean:
	rm -f *.elc
