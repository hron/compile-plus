;;; compile-plus-rust-ts-tests.el --- compile-plus-rust-ts tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Aleksei Gusev

;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Keywords: tools

;;; Code:

(require 'compile-plus-rust-ts)
(require 'ert)

(ert-deftest rust-ts-test-at-point ()
  (with-temp-buffer
    (let ((buffer-file-name "/tmp/foobar_mod.rs"))
      (insert "
      #[test]
      mod tests {
        #[test]
        fn foobar_test (){
        }

        #[test]
        fn another_test (){
        }
      }")
      (search-backward "fn foobar_test")
      (rust-ts-mode)
      (compile-plus-mode +1)
      (should (equal (compile-plus--build-future-history)
                     '("cargo test foobar_test"
                       "cargo test -- foobar_mod"
                       "cargo test"))))))

;; cargo test -p rune-macros --doc -- defun
(ert-deftest rust-ts-doctest-at-point ()
  (with-temp-buffer
    (let ((buffer-file-name "/tmp/foobar_mod.rs"))
      (insert "
        /// ```
        /// assert_eq!(4, 2 + 2)
        /// ```
        fn add(a: usize, b: usize) -> usize {
          a + b
        }
      ")
      (search-backward "fn add")
      (rust-ts-mode)
      (compile-plus-mode +1)
      (should (equal (compile-plus--build-future-history)
                     '("cargo test --doc -- add"
                       "cargo test -- foobar_mod"
                       "cargo test"))))))
