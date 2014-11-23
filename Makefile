EMACS = emacs
# EMACS = emacs-24.3
# EMACS = emacs24

CASKEMACS = cask exec $(EMACS)
LOAD = -l init.el -l lispy-inline.el -l lispy.el

.PHONY: emacsq clean

all: emacsq

cask:
	cask

emacsq:
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

compile:
	$(CASKEMACS) -q  $(LOAD) -l lispy-test.el lispy.el --eval "(progn (mapc #'byte-compile-file '(\"lispy.el\" \"lispy-inline.el\" \"le-clojure.el\" \"le-scheme.el\" \"le-lisp.el\")) (switch-to-buffer \"*Compile-Log*\") (ert t))"

ert:
	$(CASKEMACS) -q $(LOAD) -l lispy-test.el  --eval "(ert t)"

test:
	$(CASKEMACS) -batch $(LOAD) -l lispy-simple-test.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc
