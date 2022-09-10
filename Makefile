emacs ?= emacs
BEMACS = $(emacs) -batch -l elpa.el
LOAD = -l lispy-inline.el -l lispy.el
QEMACS = $(emacs) -Q -l elpa.el -l targets/interactive-init.el

all: compile test

update:
	$(emacs) -batch -l targets/install-deps.el

compile:
	$(BEMACS) $(LOAD) -l targets/compile.el

checkdoc:
	$(emacs) -batch -l elpa.el $(LOAD) -l targets/checkdoc.el

check-declare:
	$(BEMACS) $(LOAD) -l targets/check-declare.el

test:
	@echo "Using $(shell which $(emacs))..."
	$(BEMACS) -l lispy-test.el $(LOAD) -f ert-run-tests-batch-and-exit

plain:
	$(QEMACS) -l elpa.el lispy.el

clojure:
	clojure -M -e '(load-file "targets/tlc.clj")'

clean:
	rm -f *.elc

.PHONY: all clean elisp check-declare test
