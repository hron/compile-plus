;;; compile-plus-rust-ts-tests.el --- compile-plus-rust-ts tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Aleksei Gusev

;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Keywords: tools

;;; Code:

(require 'compile-plus-rust-ts)
(require 'ert)

(ert-deftest test-under-the-cursor ()
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
