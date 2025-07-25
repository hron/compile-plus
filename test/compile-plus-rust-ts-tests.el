;;; compile-plus-rust-ts-tests.el --- compile-plus-rust-ts tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Aleksei Gusev

;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Keywords: tools

;;; Code:

(require 'compile-plus)
(require 'ert)

(ert-deftest rust-ts-test-at-point ()
  (with-sample-file "rust-ts/src/sub.rs" #'rust-ts-mode
    (search-forward "fn test_sub_foo")
    (should (equal (compile-plus--build-future-history)
                   '("cargo test test_sub_foo"
                     "cargo test -- sub"
                     "cargo test")))))

(ert-deftest rust-ts-doctest-at-point ()
  (with-sample-file "rust-ts/src/add.rs" #'rust-ts-mode
    (search-forward "fn add")
    (should (equal (compile-plus--build-future-history)
                   '("cargo test --doc -- add"
                     "cargo test")))))

(ert-deftest rust-ts-main ()
  (with-sample-file "rust-ts/src/main.rs" #'rust-ts-mode
    (search-forward "fn main")
    (should (equal (compile-plus--build-future-history)
                   '("cargo run --bin"
                     "cargo test")))))

(defmacro with-sample-file (file-path mode &rest body)
  "Execute BODY in context of FILE-PATH from test fixtures directory.
Use MODE as major mode."
  (declare (indent 2))
  `(let ((buffer (find-file-noselect (expand-file-name (concat "test/fixtures/" ,file-path)))))
     (with-current-buffer buffer
       (unwind-protect
           (funcall ,mode)
         (progn ,@body)
         (kill-buffer buffer)))))
