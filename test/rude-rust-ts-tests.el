;;; rude-rust-ts-tests.el --- rude-rust-ts tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Aleksei Gusev

;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Keywords: tools

;;; Code:

(require 'rude-rust-ts)
(require 'ert)

(unless (macrop 'with-sample-file)
  (load (expand-file-name "./rude-test-helpers"
                          (file-name-directory load-file-name))))

(ert-deftest rust-ts-augmented-dape-configs ()
  (dolist (config-key '(rude-rust-codelldb rude-rust-lldb))
    (let ((config (alist-get config-key dape-configs)))
      (unless config
        (error "`dape-configs' doesn't have %S key" config-key))
      (should (plist-member config :type))
      (should (not (plist-member config :program))))))

(defun should-equal-with-cd-prefix (left right)
  "Check that LEFT is equal `cd <cargo-root> && RIGHT'."
  (let* ((project-root (abbreviate-file-name rude-project-dir))
         (cargo-dir (format "%s/test/fixtures/rust-ts/" project-root)))
    (should (equal left (format "cd %s && %s" cargo-dir right)))))

(ert-deftest rust-ts-test-at-point ()
  (with-sample-file "rust-ts/src/sub.rs" #'rust-ts-mode
    (search-forward "fn test_sub_foo")
    (should-equal-with-cd-prefix
     (rude-rust-ts-test-at-point)
     "cargo test -p rust-ts -- --no-capture --include-ignored test_sub_foo")
    (pcase-let* ((`(,debug-adapter . ,dape-config) (rude-rust-ts-test-at-point t)))
      (should (equal debug-adapter 'rude-rust-codelldb))
      (should-equal-with-cd-prefix
       (plist-get dape-config 'compile)
       "cargo test -p rust-ts --no-run")
      (should (equal (plist-get dape-config :args)
                     ["--no-capture" "--include-ignored" "test_sub_foo"])))))

(ert-deftest rust-ts-test-at-point-package-argument ()
  (with-sample-file "rust-ts/crates/multi/src/lib.rs" #'rust-ts-mode
    (let* ((project-root (abbreviate-file-name rude-project-dir))
           (cargo-dir (format "%s/test/fixtures/rust-ts/crates/multi/" project-root)))
      (search-forward "fn test_multi")
      (should (equal
               (rude-rust-ts-test-at-point)
               (format "cd %s && %s"
                       cargo-dir
                       "cargo test -p multi -- --no-capture --include-ignored test_multi")))
      (pcase-let* ((`(,debug-adapter . ,dape-config) (rude-rust-ts-test-at-point t)))
        (should (equal debug-adapter 'rude-rust-codelldb))
        (should (equal (plist-get dape-config 'compile)
                       (format "cd %s && %s" cargo-dir "cargo test -p multi --no-run")))
        (should (equal (plist-get dape-config :args)
                       ["--no-capture" "--include-ignored" "test_multi"]))))))

(ert-deftest rust-ts-doctest-at-point ()
  (with-sample-file "rust-ts/src/add.rs" #'rust-ts-mode
    (search-forward "fn add")
    (should-equal-with-cd-prefix
     (rude-rust-ts-doctest-at-point)
     "cargo test -p rust-ts --doc -- --no-capture --include-ignored add")
    (pcase-let* ((`(,debug-adapter . ,dape-config) (rude-rust-ts-doctest-at-point t)))
      (should (equal debug-adapter 'rude-rust-codelldb))
      (should-equal-with-cd-prefix (plist-get dape-config 'compile)
                                   "cargo test -p rust-ts --doc --no-run")
      (should (equal (plist-get dape-config :args)
                     ["--no-capture" "--include-ignored" "add"])))))

(ert-deftest rust-ts-test-mod ()
  (with-sample-file "rust-ts/src/sub.rs" #'rust-ts-mode
    (search-forward "#[cfg(test)]")
    (should-equal-with-cd-prefix
     (rude-rust-ts-test-mod)
     "cargo test -p rust-ts -- --no-capture --include-ignored sub")
    (pcase-let* ((`(,debug-adapter . ,dape-config) (rude-rust-ts-test-mod t)))
      (should (equal debug-adapter 'rude-rust-codelldb))
      (should-equal-with-cd-prefix
       (plist-get dape-config 'compile)
       "cargo test -p rust-ts --no-run")
      (should (equal (plist-get dape-config :args)
                     ["--no-capture" "--include-ignored" "sub"])))))

(ert-deftest rust-ts-main ()
  (with-sample-file "rust-ts/src/main.rs" #'rust-ts-mode
    (search-forward "fn main")
    (should-equal-with-cd-prefix
     (rude-rust-ts-run)
     "cargo run -p rust-ts --bin rust-ts")
    (pcase-let* ((`(,debug-adapter . ,dape-config) (rude-rust-ts-run t)))
      (should (equal debug-adapter 'codelldb-rust))
      (should-equal-with-cd-prefix
       (plist-get dape-config 'compile)
       "cargo build -p rust-ts --bin rust-ts"))))

(ert-deftest rust-ts-bin-target ()
  (with-sample-file "rust-ts/src/bin/another_bin.rs" #'rust-ts-mode
    (search-forward "fn main")
    (should-equal-with-cd-prefix (rude-rust-ts-run)
                                 "cargo run -p rust-ts --bin another_bin")))

(ert-deftest rust-ts-bin-target-with-feature ()
  (with-sample-file "rust-ts/src/bin/feature_bin.rs" #'rust-ts-mode
    (search-forward "fn main")
    (should-equal-with-cd-prefix
     (rude-rust-ts-run)
     "cargo run -p rust-ts --features foo_feature,bar_feature --bin feature_bin")))

(ert-deftest rust-ts-example ()
  (with-sample-file "rust-ts/examples/hello_world.rs" #'rust-ts-mode
    (search-forward "fn main")
    (should-equal-with-cd-prefix
     (rude-rust-ts-run)
     "cargo run -p rust-ts --example hello_world")
    (pcase-let* ((`(,debug-adapter . ,dape-config) (rude-rust-ts-run t)))
      (should (equal debug-adapter 'codelldb-rust))
      (should-equal-with-cd-prefix
       (plist-get dape-config 'compile)
       "cargo build -p rust-ts --example hello_world")
      (should (string-match "hello_world" (plist-get dape-config :program))))))

(ert-deftest rust-ts-package-name ()
  (should (equal
           (rude-rust-ts--package-name-from-pkgid "path+file:///absolute/path/rust-ts#0.1.0")
           "rust-ts"))
  (should (equal
           (rude-rust-ts--package-name-from-pkgid "path+file:///absolute/path/rust-ts#custom-rust-ts@0.1.0")
           "custom-rust-ts")))

(ert-deftest rust-ts-test-all ()
  (should (equal (rude-rust-ts-test-all) "cargo test"))
  (pcase-let* ((`(,debug-adapter . ,dape-config) (rude-rust-ts-test-all t)))
    (should (equal debug-adapter 'rude-rust-codelldb))
    (should (equal (plist-get dape-config 'compile)
                   "cargo test --no-run"))))

(ert-deftest rust-ts-slow-doctest ()
  (with-sample-file "rust-ts/src/slow-doctest.rs" #'rust-ts-mode
    (search-forward "```")
    (let* ((benchmark
            (benchmark-run
                (should-equal-with-cd-prefix
                 (rude-rust-ts-doctest-at-point)
                 "cargo test -p rust-ts --doc -- --no-capture --include-ignored add")))
           (elapsted-time (car benchmark)))
      (should (> 0.5 elapsted-time)))))

(ert-deftest rust-ts-choose-debug-adapter ()
  (with-sample-file "rust-ts/src/sub.rs" #'rust-ts-mode
    (search-forward "fn test_sub_foo")
    (pcase-let* ((rude-rust-debug-adapter 'codelldb)
                 (`(,debug-adapter) (rude-rust-ts-test-at-point t)))
      (should (equal 'rude-rust-codelldb debug-adapter)))
    (pcase-let* ((rude-rust-debug-adapter 'lldb)
                 (`(,debug-adapter) (rude-rust-ts-test-at-point t)))
      (should (equal 'rude-rust-lldb debug-adapter)))))

(ert-deftest rust-ts-dape-config-program ()
  (with-sample-file "rust-ts/src/sub.rs" #'rust-ts-mode
    (search-forward "fn test_sub_foo")
    (dolist (cmd '("cd ~/src/rude/test/fixtures/rust-ts/ && cargo test --no-run -p rust-ts"
                   "cargo test --no-run -p rust-ts"))
      (let ((config `(compile ,cmd)))
        ;; First call should add the flag because `dape' calls fn two
        ;; times: before and after compilation
        (setq config (rude-rust-ts-dape-config-program (copy-tree config)))
        (should (plist-get config 'rude-rust-ts-compile-finished))
        ;; Second call should add all the missing attributes to make
        ;; dape-config be able to run the test
        (setq config (rude-rust-ts-dape-config-program (copy-tree config)))
        (let ((program (plist-get config :program)))
          (should (and (string-match "target" program)
                       (string-match "rust_ts-" program))))))))
