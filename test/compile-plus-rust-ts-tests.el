;;; compile-plus-rust-ts-tests.el --- compile-plus-rust-ts tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Aleksei Gusev

;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Keywords: tools

;;; Code:

(require 'compile-plus)
(require 'compile-plus-test-helpers)
(require 'ert)

(ert-deftest rust-ts-test-at-point ()
  (with-sample-file "rust-ts/src/sub.rs" #'rust-ts-mode
    (search-forward "fn test_sub_foo")
    (should (equal (compile-plus--build-future-history)
                   '("cargo test -p rust-ts -- --no-capture --include-ignored test_sub_foo"
                     "cargo test -p rust-ts -- --no-capture --include-ignored sub"
                     "cargo test")))))

(ert-deftest rust-ts-package-argument ()
  (with-sample-file "rust-ts/crates/multi/src/lib.rs" #'rust-ts-mode
    (search-forward "fn test_multi")
    (should (equal (compile-plus--build-future-history)
                   '("cargo test -p multi -- --no-capture --include-ignored test_multi"
                     "cargo test -p multi -- --no-capture --include-ignored lib"
                     "cargo test")))))

(ert-deftest rust-ts-doctest-at-point ()
  (with-sample-file "rust-ts/src/add.rs" #'rust-ts-mode
    (search-forward "fn add")
    (should (equal (compile-plus--build-future-history)
                   '("cargo test -p rust-ts --doc -- --no-capture --include-ignored add"
                     "cargo test")))))

(ert-deftest rust-ts-main ()
  (with-sample-file "rust-ts/src/main.rs" #'rust-ts-mode
    (search-forward "fn main")
    (should (equal (compile-plus--build-future-history)
                   '("cargo run -p rust-ts --bin"
                     "cargo test")))))

(ert-deftest rust-ts-bin-target ()
  (with-sample-file "rust-ts/src/bin/another_bin.rs" #'rust-ts-mode
    (search-forward "fn main")
    (should (equal (compile-plus--build-future-history)
                   '("cargo run -p rust-ts --bin another_bin"
                     "cargo test")))))

(ert-deftest rust-ts-package-name ()
  (should (equal
           (compile-plus-rust-ts-package-name-from-pkgid "path+file:///absolute/path/rust-ts#0.1.0")
           "rust-ts"))
  (should (equal
           (compile-plus-rust-ts-package-name-from-pkgid "path+file:///absolute/path/rust-ts#custom-rust-ts@0.1.0")
           "custom-rust-ts")))

(ert-deftest rust-ts-slow-doctest ()
  (with-sample-file "rust-ts/src/slow-doctest.rs" #'rust-ts-mode
    (search-forward "```")
    (let* ((benchmark
            (benchmark-run
                (should (equal (compile-plus--build-future-history)
                               '("cargo test -p rust-ts --doc -- --no-capture --include-ignored add"
                                 "cargo test")))))
           (elapsted-time (car benchmark)))
      (should (> 0.2 elapsted-time)))))

(ert-deftest rust-ts-example ()
  (with-sample-file "rust-ts/examples/hello_world.rs" #'rust-ts-mode
    (search-forward "fn main")
    (should (equal (compile-plus--build-future-history)
                   '("cargo run -p rust-ts --example hello_world"
                     "cargo test")))))
