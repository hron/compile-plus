;; -*- mode: eask; lexical-binding: t -*-

(package "compile-plus"
         "0.0.1"
         "Run M-x `compile' based on buffer content")

(website-url "https://github.com/hron/compile-plus.el")
(keywords "convenience" "compile" "test")

(package-file "compile-plus.el")
(files "compile-plus-helpers.el"
       "compile-plus-python-ts.el"
       "compile-plus-rust-ts.el")

(depends-on "emacs" "29.1")

(eask-defcommand install-treesit-grammars
  "Install treesit grammars needed for the tests."
  (require 'treesit)

  (setq treesit-language-source-alist
        '((python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.23.6"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.23.3"))))
  (treesit-install-language-grammar 'python)
  (treesit-install-language-grammar 'rust))
