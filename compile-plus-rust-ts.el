;;; compile-plus-rust-ts.el --- Runnables for rust-ts-mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025  Aleksei Gusev
;;
;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Maintainer: Aleksei Gusev <aleksei.gusev@gmail.com>
;; URL: https://github.com/hron/compile-plus.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, processes, compile, test, rust
;; SPDX-License-Identifier: GPL
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Defines runnables for `rust-ts-mode'
;;
;;; Code:

(defvar compile-plus-rust-ts-test-query
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
        body: _) @end))))

;;;###autoload
(defun compile-plus-rust-ts-test-at-point ()
  "Find a test under point in `rust-ts-mode'."
  (let ((matches (treesit-query-capture
                  'rust
                  compile-plus-rust-ts-test-query
                  (point-min) (point)
                  nil t))
        (result '()))

    (when (< 0 (length matches))
      (let ((mod (file-name-base buffer-file-name)))
        (push (format "cargo test -- %s" mod) result)))

    (dolist (captures matches)
      (let ((test-name (treesit-node-text (alist-get 'test_name captures)))
            (beg (treesit-node-start (alist-get 'start captures)))
            (end (treesit-node-end (alist-get 'end captures))))
        (when (and (<= beg (point)) (>= end (point)))
          (push (format "cargo test %s" test-name) result))))

    result))

(defvar compile-plus-rust-ts-doctest-query
  (treesit-query-compile
   'rust
   '(((line_comment) :*
      (line_comment doc: (_) @_comment_content) @start
      (:match "```" @_comment_content)
      :anchor
      (line_comment) :*
      :anchor
      (line_comment doc: (_) @_end_comment_content) @_end_code_block
      (:match "```" @_end_comment_content)
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
        name: (_) @doc_test_name)] @end))))

;;;###autoload
(defun compile-plus-rust-ts-doctest-at-point ()
  "Find the doctest at point in `rust-ts-mode'."
  (let ((matches (treesit-query-capture
                  'rust
                  compile-plus-rust-ts-doctest-query
                  (point-min) (point)
                  nil t))
        (result '()))

    (when (< 0 (length matches))
      (let ((mod (file-name-base buffer-file-name)))
        (push (format "cargo test -- %s" mod) result)))

    (dolist (captures matches)
      (let ((test-name (treesit-node-text (alist-get 'doc_test_name captures)))
            (beg (treesit-node-start (alist-get 'start captures)))
            (end (treesit-node-end (alist-get 'end captures))))
        (when (and (<= beg (point)) (>= end (point)))
          (push (format "cargo test --doc -- %s" test-name) result))))

    result))

;;;###autoload
(defun compile-plus-rust-ts-all ()
  "Return command to run all tests as string."
  "cargo test")

;; (defun rust-buffer-crate ()
;;   "Try to locate Cargo.toml using `locate-dominating-file'."
;;   (let ((dir (locate-dominating-file default-directory "Cargo.toml")))
;;     (if dir dir default-directory)))

;; cargo pkgid
;; path+file:///home/algus/src/rune#0.1.0


;; // For providing local `cargo check -p $pkgid` task, we do not need most of the information we have returned.
;; // Output example in the root of Zed project:
;; // ```sh
;; // ❯ cargo pkgid zed
;; // path+file:///absolute/path/to/project/zed/crates/zed#0.131.0
;; // ```
;; // Another variant, if a project has a custom package name or hyphen in the name:
;; // ```
;; // path+file:///absolute/path/to/project/custom-package#my-custom-package@0.1.0
;; // ```
;; //
;; // Extracts the package name from the output according to the spec:
;; // https://doc.rust-lang.org/cargo/reference/pkgid-spec.html#specification-grammar
;; fn package_name_from_pkgid(pkgid: &str) -> Option<&str> {
;;     fn split_off_suffix(input: &str, suffix_start: char) -> &str {
;;         match input.rsplit_once(suffix_start) {
;;             Some((without_suffix, _)) => without_suffix,
;;             None => input,
;;         }
;;     }
;;
;;     let (version_prefix, version_suffix) = pkgid.trim().rsplit_once('#')?;
;;     let package_name = match version_suffix.rsplit_once('@') {
;;         Some((custom_package_name, _version)) => custom_package_name,
;;         None => {
;;             let host_and_path = split_off_suffix(version_prefix, '?');
;;             let (_, package_name) = host_and_path.rsplit_once('/')?;
;;             package_name
;;         }
;;     };
;;     Some(package_name)
;; }

(provide 'compile-plus-rust-ts)
;;; compile-plus-rust-ts.el ends here
