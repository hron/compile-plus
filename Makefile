EMACS ?= emacs
EASK ?= eask

.PHONY: clean package install compile test checkdoc lint ci

ci: clean package install compile checkdoc lint test

package:
	$(EASK) package

install:
	$(EASK) install

compile:
	$(EASK) compile

test:
	$(EASK) test ert ./test/*.el

checkdoc:
	$(EASK) lint checkdoc --strict

lint:
	$(EASK) lint package

autoloads:
	$(EASK) autoloads

pkg-file:
	$(EASK) pkg-file

clean:
	$(EASK) clean all
