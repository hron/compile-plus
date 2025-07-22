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

(require 'compile-plus)

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

(defun compile-plus-rust-ts-test-under-cursor ()
  "Find a test under point in `rust-ts-mode'."
  (let ((matches (treesit-query-capture
                  'rust
                  compile-plus-rust-ts-test-query
                  (point-min) (point)
                  nil t))
        (result '()))
    (dolist (captures matches)
      (let ((test-name (treesit-node-text (alist-get 'test_name captures)))
            (beg (treesit-node-start (alist-get 'start captures)))
            (end (treesit-node-end (alist-get 'end captures))))
        (when (and (<= beg (point)) (>= end (point)))
          (push (format "cargo test %s" test-name) result))))
    result))

;; "cargo test -p rune -- fileio" to run module tests

(add-to-list 'compile-plus-providers-alist '(rust-ts-mode . (compile-plus-rust-ts-test-under-cursor)))

(provide 'compile-plus-rust-ts)
;;; compile-plus-rust-ts.el ends here
