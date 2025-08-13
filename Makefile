EASK ?= eask --strict

.PHONY: clean package install compile test test-fast checkdoc lint ci

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

ERT_SELECTOR ?= t
ifeq ($(ERT_SELECTOR),t)
	ERT_SELECTOR_ARG := t
else
	ERT_SELECTOR_ARG := "$(ERT_SELECTOR)"
endif

test-fast:
	$(EASK) emacs --batch  -L . \
		$(addprefix -l , $(wildcard test/*-tests.el)) \
		--eval '(ert-run-tests-batch-and-exit $(ERT_SELECTOR_ARG))'

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
