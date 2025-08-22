;;; rude-rust-ts.el --- Runnables for rust-ts-mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025  Aleksei Gusev
;;
;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; SPDX-License-Identifier: GPL
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Defines runnables for `rust-ts-mode'
;;
;;; Code:

(require 'rude-helpers)
(require 'dape)

(defgroup rude-rust nil
  "Rust settings of `rude' package."
  :tag "Rust"
  :link '(url-link :tag "Website" "https://github.com/hron/rude.el")
  :link '(emacs-library-link :tag "Library Source" "rude-rust-ts.el")
  :group 'rude
  :prefix "rude-rust-")

(defcustom rude-rust-test-binary-args "--no-capture --include-ignored"
  "Arguments for the test binary: cargo test -- ARGS."
  :tag "Rust test binary arguments"
  :type 'string
  :group 'rude-rust)

(defcustom rude-rust-debug-adapter 'codelldb
  "Debug adapter to use to debug Rust."
  :type 'symbol
  :options '(codelldb lldb)
  :group 'rude-rust)

(defvar rude-rust-ts--test-query
  (treesit-query-compile
   'rust
   '(((attribute_item
       (attribute
        [((identifier) @_attribute)
         (scoped_identifier (identifier) @_attribute)])
       (:match "test"  @_attribute)) @start
       :anchor
       (attribute_item) :*
       :anchor
       [(line_comment) (block_comment)] :*
       :anchor
       (function_item
        name: (_)  @test_name
        body: _) @end
       (:pred rude-helpers--point-between-nodes-p @start @end)))))

(pcase-dolist (`(,key ,dape-key) '((lldb lldb-dap) (codelldb codelldb-rust)))
  (push
   (let* ((key (intern (concat "rude-rust-" (symbol-name key))))
          (base-config (alist-get dape-key dape-configs))
          (base-config (map-delete base-config :program)))
     `(,key fn rude-rust-ts-dape-config-program ,@base-config))
   dape-configs))

(defun rude-rust-ts-dape-config-program (config)
  "Replace :program in CONFIG with the test executable."
  ;; `dape' runs this function twice: before and after compile. it
  ;; should act only after compile.
  (if (plist-get config 'rude-rust-ts-compile-finished)
      (let ((executable (rude-rust-ts--dape-test-cmd (plist-get config 'compile))))
        (plist-put config :program executable))
    (plist-put config 'rude-rust-ts-compile-finished t))
  config)

(defun rude-rust-ts--extact-arg-value (command arg)
  "Extract the value of ARG in COMMAND.  Return nil if not found."
  (and (string-match (concat "-" arg " \\([^ ]+\\)") command)
       (match-string 1 command)))

(defun rude-rust-ts--dape-test-cmd (command)
  "Find test executable for the given COMMAND which is cargo test ..."
  (let* ((args (replace-regexp-in-string "^[[:space:]]*cd .* && " "" command))
         (args (concat args " --message-format=json"))
         (args (string-remove-prefix "cargo " args))
         (args (string-split args " "))
         (cargo-package (rude-rust-ts--extact-arg-value command "p")))
    (with-temp-buffer
      (apply 'call-process "cargo" nil '(t nil) nil args)
      (let ((json-objs (seq-map (lambda (string)
                                  (json-parse-string string :array-type 'list))
                                (string-split (string-trim (buffer-string)) "\n"))))
        (seq-reduce
         (lambda (test-cmd json)
           (or (when-let* ((target (gethash "target" json))
                           (target-name (gethash "name" target)))
                 (when (equal target-name cargo-package)
                   (gethash "executable" json)))
               test-cmd))
         json-objs
         '())))))

(defun rude-rust-ts--dape-debug-adapter ()
  "Return a symbol to be used as a key for `dape-configs'."
  (let* ((debug-adapter (symbol-name rude-rust-debug-adapter))
         (debug-adapter (concat "rude-rust-" debug-adapter))
         (debug-adapter (intern debug-adapter)))
    debug-adapter))

(defun rude-rust-ts--build-dape-config (command)
  "Build `dape-config' for COMMAND."
  (pcase-let* ((debug-adapter (rude-rust-ts--dape-debug-adapter))
               (`(,command ,args) (string-split command " -- "))
               (command (concat command " --no-run"))
               (args (apply #'vector (string-split (or args "") " "))))
    `(,debug-adapter
      compile ,command
      command-cwd rude-rust-ts-default-directory
      :args ,args)))

(defun rude-rust-ts--add-cd-prefix (command)
  "Add `cd <cargo-root> && ' to COMMAND if needed."
  (let ((cargo-root (rude-rust-ts-default-directory)))
    (if (equal default-directory cargo-root)
        command
      (format "cd %s && %s" cargo-root command))))

;;;###autoload
(defun rude-rust-ts-test-at-point (&optional debug)
  "Build a command line to run the test at point using cargo test.
If DEBUG is non-nil, then return a `dape' configuration instead."
  (when-let*
      ((captures (treesit-query-capture 'rust rude-rust-ts--test-query))
       (test-name (treesit-node-text (alist-get 'test_name captures) t))
       (cargo-package (rude-rust-ts--package-name))
       (command (format "cargo test -p %s -- %s %s"
                        cargo-package
                        rude-rust-test-binary-args
                        test-name))
       (command (rude-rust-ts--add-cd-prefix command)))
    (if debug (rude-rust-ts--build-dape-config command) command)))

(defun rude-rust-ts--package-name ()
  "Return cargo package name for current buffer by running `cargo pkgid`.
Return nil if detection fails or cargo is not available."
  (let* ((default-directory (file-name-directory buffer-file-name))
         (pkgid (string-trim (shell-command-to-string "cargo pkgid"))))
    (rude-rust-ts--package-name-from-pkgid pkgid)))

(defun rude-rust-ts--package-name-from-pkgid (pkgid)
  "Extracts package name from PKGID.
path+file:///absolute/path/package_name#0.1.0
path+file:///absolute/path/package_name#custom-package@0.1.0."
  (if (string-match-p "@" pkgid)
      (let* ((name (car (string-split pkgid "@")))
             (name (car (last (string-split name "#")))))
        name)
    (let* ((name (car (string-split pkgid "#" )))
           (name (car (last (string-split name "/")))))
      name)))

(defvar rude-rust-ts--doctest-query
  (treesit-query-compile
   'rust
   '(((line_comment) :*
      (line_comment doc: (_) @_comment_content) @start
      (:match "```" @_comment_content)
      ;; This is an optimization. Without it the query is super, when
      ;; multiple ``` blocks are present in the comments
      ;; :anchor
      ;; (line_comment) :* :anchor (line_comment doc: (_)
      ;; @_end_comment_content) @_end_code_block (:match "```"
      ;; @_end_comment_content)
      :anchor
      (line_comment) :*
      (attribute_item) :*
      :anchor
      [(function_item
        name: (_)  @doc_test_name
        body: _)
       (function_signature_item
        name: (_) @doc_test_name)
       (struct_item
        name: (_) @doc_test_name)
       (enum_item
        name: (_) @doc_test_name
        body: _)
       ((attribute_item) :?
        (macro_definition
         name: (_) @doc_test_name))
       (mod_item
        name: (_) @doc_test_name)] @end
      (:pred rude-helpers--point-between-nodes-p @start @end)))))

;;;###autoload
(defun rude-rust-ts-doctest-at-point (&optional debug)
  "Find the doctest at point in `rust-ts-mode'.
If DEBUG is non-nil, then return a `dape' configuration instead."
  (when-let*
      ((captures (treesit-query-capture 'rust rude-rust-ts--doctest-query))
       (test-name (treesit-node-text (alist-get 'doc_test_name captures) t))
       (command (format "cargo test -p %s --doc -- %s %s"
                        (rude-rust-ts--package-name)
                        rude-rust-test-binary-args
                        test-name))
       (command (rude-rust-ts--add-cd-prefix command)))
    (if debug (rude-rust-ts--build-dape-config command) command)))

(defvar rude-rust-ts--test-mod-query
  (treesit-query-compile
   'rust
   '(((attribute_item
       (attribute
        ((identifier) @_attribute)
        arguments: (
                    (token_tree (identifier) @_test)
                    (:equal "test" @_test)))
       (:equal "cfg" @_attribute))
      :anchor
      (mod_item
       name: (_))))))

;;;###autoload
(defun rude-rust-ts-test-mod (&optional debug)
  "Build a command to test the current mod.
If DEBUG is non-nil, then return a `dape' configuration instead."
  (when (treesit-query-capture 'rust rude-rust-ts--test-mod-query)
    (let* ((command (format "cargo test -p %s -- %s %s"
                            (rude-rust-ts--package-name)
                            rude-rust-test-binary-args
                            (file-name-base buffer-file-name)))
           (command (rude-rust-ts--add-cd-prefix command)))
      (if debug (rude-rust-ts--build-dape-config command) command))))

(defvar rude-rust-ts--run-query
  (treesit-query-compile
   'rust
   '(((function_item
       name: (_) @_func_name
       body: _) @start @end
       (:equal "main" @_func_name)
       (:pred rude-helpers--point-between-nodes-p @start @end)))))

(defun rude-rust-ts--run-kind ()
  "Return bin/example for current cargo package."
  (car (gethash "kind" (rude-rust-ts--cargo-target))))

(defun rude-rust-ts--run-name ()
  "Return name for current bin/example of the cargo package."
  (if (string-suffix-p "src/main.rs" buffer-file-name)
      (rude-rust-ts--package-name)
    (gethash "name" (rude-rust-ts--cargo-target))))

(defun rude-rust-ts--cargo-target ()
  "Return target entry from cargo metadata for current buffer."
  (let (result)
    (dolist (package (gethash "packages" (rude-rust-ts--cargo-metadata)))
      (dolist (target (gethash "targets" package))
        (when (equal buffer-file-name (expand-file-name (gethash "src_path" target)))
          (setq result target))))
    result))

(defvar-local rude-rust-ts--cargo-metadata nil)

(defun rude-rust-ts--cargo-metadata ()
  "Return a hash table with cargo metadata for current buffer."
  (when (not rude-rust-ts--cargo-metadata)
    (setq rude-rust-ts--cargo-metadata
          (json-parse-string
           (let ((default-directory (file-name-directory buffer-file-name)))
             (shell-command-to-string
              "cargo metadata --no-deps --format-version 1"))
           :array-type 'list)))
  rude-rust-ts--cargo-metadata)

(defun rude-rust-ts--run-features-flag ()
  "Return --features foo,bar needed for current buffer to run, if any."
  (if-let* ((target (rude-rust-ts--cargo-target))
            (features (gethash "required-features" target)))
      (concat "--features " (string-join features ",") " ")
    ""))

(defun rude-rust-ts--build-dape-config-for-run (command)
  "Build `dape-config' for COMMAND."
  (pcase-let*
      ((debug-adapter
        (if (equal 'codelldb rude-rust-debug-adapter)
            'codelldb-rust
          'lldb-dap))
       (`(,build-command ,args) (string-split command " -- "))
       (build-command (string-trim (string-replace "cargo run" "cargo build" build-command)))
       (args (if (stringp args) (vector (string-split args)) []))
       (example-arg (rude-rust-ts--extact-arg-value command "example"))
       (binary-name (if example-arg
                        (file-name-concat "examples" example-arg)
                      (rude-rust-ts--package-name)))
       (program (file-name-concat (rude-rust-ts-default-directory)
                                  "target"
                                  "debug"
                                  binary-name)))
    `(,debug-adapter
      compile ,build-command
      command-cwd rude-rust-ts-default-directory
      :program ,program
      :args ,args)))

;;;###autoload
(defun rude-rust-ts-run (&optional debug)
  "Return command to run main function at point.
If DEBUG is non-nil, then return a `dape' configuration instead."
  (when (treesit-query-capture 'rust rude-rust-ts--run-query)
    (let* ((command
            (string-trim (format "cargo run -p %s %s--%s %s"
                                 (rude-rust-ts--package-name)
                                 (rude-rust-ts--run-features-flag)
                                 (rude-rust-ts--run-kind)
                                 (rude-rust-ts--run-name))))
           (command (rude-rust-ts--add-cd-prefix command)))
      (if debug (rude-rust-ts--build-dape-config-for-run command) command))))

;;;###autoload
(defun rude-rust-ts-test-all (&optional debug)
  "Build the command to run the whole project.
If DEBUG is non-nil, then return a `dape' configuration instead."
  (let* ((command "cargo test")
         (command (rude-rust-ts--add-cd-prefix command)))
    (if debug (rude-rust-ts--build-dape-config command) command)))

;;;###autoload
(defun rude-rust-ts-default-directory ()
  "Find the project root -- dominating directory with Cargo.toml."
  (cond
   (buffer-file-name
    (if-let* ((cargo-dir (or (locate-dominating-file buffer-file-name "Cargo.toml")
                             (locate-dominating-file default-directory "Cargo.toml"))))
        cargo-dir
      default-directory))
   (t default-directory)))

(provide 'rude-rust-ts)
;;; rude-rust-ts.el ends here
