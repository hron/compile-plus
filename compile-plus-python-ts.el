;;; compile-plus-python-ts.el --- Runnables for python-ts-mode  -*- lexical-binding: t; -*-;;
;;
;; Copyright (C) 2025  Aleksei Gusev
;;
;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
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
;; Defines runnables for `python-ts-mode'
;;
;;; Code:
(require 'treesit)

(defcustom compile-plus-python-ts-bin "python3"
  "Name of python binary used to build commands."
  :type 'string
  :group 'compile-plus)

(defvar compile-plus-python-ts--main-query
  (treesit-query-compile
   'python
   '((module
      (if_statement
       condition: (comparison_operator
                   (identifier) @_lhs
                   operators: "=="
                   (string) @_rhs)
       (:equal "__name__" @_lhs)
       (:match "^[\"']__main__[\"']$" @_rhs))) @start @end)))

(defun compile-plus-python-ts-main ()
  "Return command line to run the Python main module.
This function checks if the current buffer is a Python source file with
defined __main__ and returns a string with the command line that
can be used for `compile' to run the file."
  (when-let* ((matches (treesit-query-capture
                        'python
                        compile-plus-python-ts--main-query
                        (point-min) (point-max)
                        nil t))
              (captures (seq-find #'compile-plus-helpers--has-point-p matches))
              (relative-buffer-path (file-relative-name buffer-file-name default-directory)))
    (format "%s %s" compile-plus-python-ts-bin relative-buffer-path)))

(defvar compile-plus-python-ts--unittest-class-query
  (treesit-query-compile
   'python
   '(((class_definition
       name: (identifier) @class-name
       superclasses: (argument_list
                      [(identifier) @_superclass
                       (attribute (identifier) @_superclass)])
       (:equal "TestCase" @_superclass ))) @start @end)))

(defun compile-plus-python-ts-unittest-class ()
  "Return command line to run a subclass of unittest.TestCase."
  (when-let* ((matches (treesit-query-capture
                        'python
                        compile-plus-python-ts--unittest-class-query
                        (point-min) (point-max)
                        nil t))
              (captures (seq-find #'compile-plus-helpers--has-point-p matches))
              (class-name (treesit-node-text (alist-get 'class-name captures)))
              (module (file-name-base buffer-file-name)))
    (format "%s -m unittest %s.%s" compile-plus-python-ts-bin module class-name)))

(defvar compile-plus-python-ts--unittest-method-query
  (treesit-query-compile
   'python
   '((((class_definition
        name: (identifier) @class-name
        superclasses: (argument_list
                       [(identifier) @_superclass
                        (attribute (identifier) @_superclass)])
        (:equal "TestCase" @_superclass )
        body: (block
               (function_definition
                name: (identifier) @method-name
                (:match "^test.*" @method-name ))) @start @end))))))

(defun compile-plus-python-ts-unittest-method ()
  "Return command line to run a method of subclass of unittest.TestCase."
  (when-let* ((matches (treesit-query-capture
                        'python
                        compile-plus-python-ts--unittest-method-query
                        (point-min) (point-max)
                        nil t))
              (captures (seq-find #'compile-plus-helpers--has-point-p matches))
              (method-name (treesit-node-text (alist-get 'method-name captures)))
              (class-name (treesit-node-text (alist-get 'class-name captures)))
              (module (file-name-base buffer-file-name)))
    (format "%s -m unittest %s.%s.%s"
            compile-plus-python-ts-bin
            module class-name method-name)))

(provide 'compile-plus-python-ts)
;;; compile-plus-python-ts.el ends here
