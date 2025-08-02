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
	$(EASK) emacs --batch \
							-l treesit \
							--eval "(progn \
												(push '(python \"https://github.com/tree-sitter/tree-sitter-python\") \
															treesit-language-source-alist) \
												(push '(rust \"https://github.com/tree-sitter/tree-sitter-rust\") \
															treesit-language-source-alist) \
												(setq treesit-auto-install-grammar 'always) \
												(treesit-install-language-grammar 'python) \
												(treesit-install-language-grammar 'rust))"

compile:
	$(EASK) compile

test:
	$(EASK) test ert ./test/*.el

test-fast:
	$(EMACS) --batch  -L . -l test/*.el --eval '(ert-run-tests-batch-and-exit t)'

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
