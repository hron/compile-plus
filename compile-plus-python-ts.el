;;; compile-plus-python-ts.el --- Runnables for python-ts-mode  -*- lexical-binding: t; -*-;;
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
;; Defines runnables for `python-ts-mode'
;;
;;; Code:

(require 'treesit)
(require 'compile-plus-helpers)

(defgroup compile-plus-python nil
  "Python settings of `compile-plus' package."
  :tag "Python"
  :link '(url-link :tag "Website" "https://github.com/hron/compile-plus")
  :link '(emacs-library-link :tag "Library Source" "compile-plus-python-ts.el")
  :group 'compile-plus
  :prefix "compile-plus-python-")

(defcustom compile-plus-python-ts-bin "python3"
  "The command to use to run Python."
  :tag "Python Command"
  :type 'string
  :local t
  :group 'compile-plus-python)

(defcustom compile-plus-python-ts-test-runner "unittest"
  "Test runner to use to run Python tests."
  :tag "Test Runner"
  :type '(choice (const "unittest")
                 (const "pytest"))
  :local t
  :group 'compile-plus-python
  :link '(emacs-library-link :tag "Library Source" "compile-plus-python-ts.el"))

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
       (:match "^[\"']__main__[\"']$" @_rhs)) @start @end
      (:pred compile-plus-helpers--point-between-nodes-p @start @end)))))

(defun compile-plus-python-ts-main ()
  "Return command line to run the Python main module.
This function checks if the current buffer is a Python source file with
defined __main__ and returns a string with the command line that
can be used for `compile' to run the file."
  (when-let* ((captures (treesit-query-capture 'python compile-plus-python-ts--main-query))
              (relative-buffer-path (file-relative-name buffer-file-name default-directory)))
    (compile-plus--format-no-prop "%s %s" compile-plus-python-ts-bin relative-buffer-path)))

(defvar compile-plus-python-ts--unittest-class-query
  (treesit-query-compile
   'python
   '(((class_definition
       name: (identifier) @class-name
       superclasses: (argument_list
                      [(identifier) @_superclass
                       (attribute (identifier) @_superclass)])
       (:equal "TestCase" @_superclass)) @start @end
       (:pred compile-plus-helpers--point-between-nodes-p @start @end)))))

(defvar compile-plus-python-ts--pytest-class-query
  (treesit-query-compile
   'python
   '((((module
        (class_definition
         name: (identifier) @class-name
         (:match "^Test" @class-name)) @start @end
        (:pred compile-plus-helpers--point-between-nodes-p @start @end)))))))

(defun compile-plus-python-ts-test-class ()
  "Return command line to run a test class."
  (when-let* ((test-runner compile-plus-python-ts-test-runner)
              (queries (if (equal "pytest" test-runner)
                           '(compile-plus-python-ts--pytest-class-query
                             compile-plus-python-ts--unittest-class-query)
                         '(compile-plus-python-ts--unittest-class-query)))
              (captures  (compile-plus-treesit-query-capture
                          'python
                          (seq-map #'symbol-value queries)))
              (class-name (treesit-node-text (alist-get 'class-name captures)))
              (test-file (file-relative-name buffer-file-name default-directory)))
    (compile-plus--format-no-prop "%s -m %s %s -k %s"
                                  compile-plus-python-ts-bin
                                  test-runner
                                  test-file
                                  class-name)))

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
                (:match "^test.*" @method-name ))) @start @end
        (:pred compile-plus-helpers--point-between-nodes-p @start @end)))))))

(defvar compile-plus-python-ts--pytest-method-query
  (treesit-query-compile
   'python
   '((((class_definition
        name: (identifier) @class-name
        body: (block
               (function_definition
                name: (identifier) @method-name
                (:match "^test.*" @method-name ))) @start @end
        (:pred compile-plus-helpers--point-between-nodes-p @start @end)))))))

(defun compile-plus-python-ts-test-method ()
  "Return command line to run a method of subclass of unittest.TestCase."
  (when-let* ((test-runner compile-plus-python-ts-test-runner)
              (queries (if (equal "pytest" test-runner)
                           '(compile-plus-python-ts--pytest-method-query
                             compile-plus-python-ts--unittest-method-query)
                         '(compile-plus-python-ts--unittest-method-query)))
              (captures (compile-plus-treesit-query-capture
                         'python
                         (seq-map #'symbol-value queries)))
              (method-name (treesit-node-text (alist-get 'method-name captures)))
              (class-name (treesit-node-text (alist-get 'class-name captures)))
              (test-file (file-relative-name buffer-file-name default-directory))
              (delimiter (if (equal "pytest" test-runner)
                             " and "
                           ".")))
    (compile-plus--format-no-prop "%s -m %s %s -k '%s%s%s'"
                                  compile-plus-python-ts-bin
                                  test-runner
                                  test-file
                                  class-name
                                  delimiter
                                  method-name)))

(defvar compile-plus-python-ts--test-file-query
  (treesit-query-compile
   'python
   '((function_definition
      name: (identifier) @method-name
      (:match "^test.*" @method-name )))))

(defun compile-plus-python-ts-test-file ()
  "Return command line to run the current buffer as a test module."
  (when (treesit-query-capture 'python compile-plus-python-ts--test-file-query)
    (compile-plus--format-no-prop "%s -m %s %s"
                                  compile-plus-python-ts-bin
                                  compile-plus-python-ts-test-runner
                                  (file-relative-name buffer-file-name))))

(provide 'compile-plus-python-ts)
;;; compile-plus-python-ts.el ends here
