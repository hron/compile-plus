;;; compile-plus-rust-ts-tests.el --- compile-plus-rust-ts tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Aleksei Gusev

;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Keywords: tools

;;; Code:

(require 'compile-plus-rust-ts)
(require 'ert)

(unless (macrop 'with-sample-file)
  (load (expand-file-name "./compile-plus-test-helpers"
                          (file-name-directory load-file-name))))

(ert-deftest rust-ts-test-at-point ()
  (with-sample-file "rust-ts/src/sub.rs" #'rust-ts-mode
    (search-forward "fn test_sub_foo")
    (should (equal (compile-plus-rust-ts-test-at-point)
                   "cargo test -p rust-ts -- --no-capture --include-ignored test_sub_foo"))))

(ert-deftest rust-ts-package-argument ()
  (with-sample-file "rust-ts/crates/multi/src/lib.rs" #'rust-ts-mode
    (search-forward "fn test_multi")
    (should (equal (compile-plus-rust-ts-test-at-point)
                   "cargo test -p multi -- --no-capture --include-ignored test_multi"))))

(ert-deftest rust-ts-doctest-at-point ()
  (with-sample-file "rust-ts/src/add.rs" #'rust-ts-mode
    (search-forward "fn add")
    (should (equal (compile-plus-rust-ts-doctest-at-point)
                   "cargo test -p rust-ts --doc -- --no-capture --include-ignored add"))))

(ert-deftest rust-ts-mod ()
  (with-sample-file "rust-ts/src/sub.rs" #'rust-ts-mode
    (search-forward "#[cfg(test)]")
    (should (equal (compile-plus-rust-ts-test-mod)
                   "cargo test -p rust-ts -- --no-capture --include-ignored sub"))))

(ert-deftest rust-ts-main ()
  (with-sample-file "rust-ts/src/main.rs" #'rust-ts-mode
    (search-forward "fn main")
    (should (equal (compile-plus-rust-ts-run)
                   "cargo run -p rust-ts --bin"))))

(ert-deftest rust-ts-bin-target ()
  (with-sample-file "rust-ts/src/bin/another_bin.rs" #'rust-ts-mode
    (search-forward "fn main")
    (should (equal (compile-plus-rust-ts-run)
                   "cargo run -p rust-ts --bin another_bin"))))

(ert-deftest rust-ts-bin-target-with-feature ()
  (with-sample-file "rust-ts/src/bin/feature_bin.rs" #'rust-ts-mode
    (search-forward "fn main")
    (should (equal (compile-plus-rust-ts-run)
                   "cargo run -p rust-ts --features foo_feature,bar_feature --bin feature_bin"))))

(ert-deftest rust-ts-package-name ()
  (should (equal
           (compile-plus-rust-ts--package-name-from-pkgid "path+file:///absolute/path/rust-ts#0.1.0")
           "rust-ts"))
  (should (equal
           (compile-plus-rust-ts--package-name-from-pkgid "path+file:///absolute/path/rust-ts#custom-rust-ts@0.1.0")
           "custom-rust-ts")))

(ert-deftest rust-ts-slow-doctest ()
  (with-sample-file "rust-ts/src/slow-doctest.rs" #'rust-ts-mode
    (search-forward "```")
    (let* ((benchmark
            (benchmark-run
                (should (equal (compile-plus-rust-ts-doctest-at-point)
                               "cargo test -p rust-ts --doc -- --no-capture --include-ignored add"))))
           (elapsted-time (car benchmark)))
      (should (> 0.5 elapsted-time)))))

(ert-deftest rust-ts-example ()
  (with-sample-file "rust-ts/examples/hello_world.rs" #'rust-ts-mode
    (search-forward "fn main")
    (should (equal (compile-plus-rust-ts-run)
                   "cargo run -p rust-ts --example hello_world"))))

(ert-deftest rust-ts-dape-config-program ()
  (with-sample-file "rust-ts/src/sub.rs" #'rust-ts-mode
    (search-forward "fn test_sub_foo")
    (let* ((cmd "cargo test --no-run -p rust-ts -- --no-capture --include-ignored test_sub_foo")
           (config `(compile ,cmd)))
      ;; First call should add the flag because `dape' calls fn two
      ;; times: before and after compilation
      (setq config (compile-plus-rust-ts-dape-config-program (copy-tree config)))
      (should (plist-get config 'compile-plus-rust-ts-compile-finished))
      ;; Second call should add all the missing attributes to make
      ;; dape-config be able to run the test
      (setq config (compile-plus-rust-ts-dape-config-program (copy-tree config)))
      (should (string-match "target/debug/deps/rust_ts-"
                            (plist-get config :program)))
      (should (equal ["--no-capture" "--include-ignored" "test_sub_foo"]
                     (plist-get config :args))))))
