EMACS ?= emacs
EASK ?= eask --strict

.PHONY: clean package install compile test checkdoc lint ci

ci:
	$(MAKE) clean
	$(MAKE) package
	$(MAKE) install
	$(MAKE) compile
	$(MAKE) checkdoc
	$(MAKE) lint
	$(MAKE) test

package:
	$(EASK) package

install:
	$(EASK) install
	$(EASK) run command install-treesit-grammars

compile:
	$(EASK) compile

test:
	$(EASK) test ert ./test/*-tests.el

test-fast:
	$(EMACS) --batch  -L . -l test/compile-plus-rust-ts-tests.el --eval '(ert-run-tests-batch-and-exit t)'

checkdoc:
	$(EASK) lint checkdoc --strict

lint:
	$(EASK) lint package; true

autoloads:
	$(EASK) autoloads

pkg-file:
	$(EASK) pkg-file

clean:
	$(EASK) clean all
