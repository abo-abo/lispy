emacs ?= emacs
CASK = ~/.cask/bin/cask
BEMACS = $(emacs) -batch -l elpa.el
LOAD = -l lispy-inline.el -l lispy.el
QEMACS = $(emacs) -Q -l elpa.el -l targets/interactive-init.el

all: compile test

cask:
	$(shell EMACS=$(emacs) $(CASK) --verbose --debug)

update:
	$(emacs) -batch -l targets/install-deps.el

compile:
	$(BEMACS) $(LOAD) -l targets/compile.el

checkdoc:
	emacs-snapshot -batch -l elpa.el $(LOAD) -l targets/checkdoc.el

check-declare:
	$(BEMACS) $(LOAD) -l targets/check-declare.el

test:
	@echo "Using $(shell which $(emacs))..."
	$(BEMACS) -l lispy-test.el $(LOAD) -f ert-run-tests-batch-and-exit

elisp:
	$(QEMACS) lispy.el

clojure:
	clojure -e '(load-file "lispy-clojure.clj")'

clean:
	rm -f *.elc

.PHONY: all clean cask elisp check-declare
