;;; rude-python-ts.el --- Runnables for python-ts-mode  -*- lexical-binding: t; -*-;;
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
(require 'rude-helpers)

(defgroup rude-python nil
  "Python settings of `rude' package."
  :tag "Python"
  :link '(url-link :tag "Website" "https://github.com/hron/rude")
  :link '(emacs-library-link :tag "Library Source" "rude-python-ts.el")
  :group 'rude
  :prefix "rude-python-")

(defcustom rude-python-ts-bin "python3"
  "The command to use to run Python."
  :tag "Python Command"
  :type 'string
  :local t
  :group 'rude-python)

(defcustom rude-python-ts-test-runner "unittest"
  "Test runner to use to run Python tests."
  :tag "Test Runner"
  :type '(choice (const "unittest")
                 (const "pytest"))
  :local t
  :group 'rude-python
  :link '(emacs-library-link :tag "Library Source" "rude-python-ts.el"))

(defvar rude-python-ts--main-query
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
      (:pred rude-helpers--point-between-nodes-p @start @end)))))

(defun rude-python-ts-main (&optional debug)
  "Return command line to run the Python main module.
This function checks if the current buffer is a Python source file with
defined __main__ and returns a string with the command line that
can be used for `compile' to run the file.

If DEBUG is t then return `dape' configuration instead."
  (when-let* ((captures
               (treesit-query-capture 'python rude-python-ts--main-query))
              (relative-buffer-path
               (file-relative-name buffer-file-name default-directory)))
    (if debug
        `(debugpy :program ,relative-buffer-path)
      (format "%s %s" rude-python-ts-bin relative-buffer-path))))

(defvar rude-python-ts--unittest-class-query
  (treesit-query-compile
   'python
   '(((class_definition
       name: (identifier) @class-name
       superclasses: (argument_list
                      [(identifier) @_superclass
                       (attribute (identifier) @_superclass)])
       (:equal "TestCase" @_superclass)) @start @end
       (:pred rude-helpers--point-between-nodes-p @start @end)))))

(defvar rude-python-ts--pytest-class-query
  (treesit-query-compile
   'python
   '((((module
        (class_definition
         name: (identifier) @class-name
         (:match "^Test" @class-name)) @start @end
        (:pred rude-helpers--point-between-nodes-p @start @end)))))))

(defun rude-python-ts-test-class (&optional debug)
  "Return command line to run a test class.
If DEBUG is t then return `dape' configuration instead."
  (when-let* ((test-runner rude-python-ts-test-runner)
              (queries (if (equal "pytest" test-runner)
                           '(rude-python-ts--pytest-class-query
                             rude-python-ts--unittest-class-query)
                         '(rude-python-ts--unittest-class-query)))
              (captures  (rude-treesit-query-capture
                          'python
                          (seq-map #'symbol-value queries)))
              (class-name (treesit-node-text (alist-get 'class-name captures) t))
              (test-file (file-relative-name buffer-file-name default-directory))
              (test-runner-args (format "%s -k %s" test-file class-name)))
    (cond
     (debug
      `(debugpy-module command ,rude-python-ts-bin
                       :module ,test-runner
                       :args ,test-runner-args))
     (t
      (format "%s -m %s %s"
              rude-python-ts-bin
              test-runner
              test-runner-args)))))

(defvar rude-python-ts--unittest-method-query
  (treesit-query-compile
   'python
   '((((class_definition
        name: (identifier) @class-name
        superclasses: (argument_list
                       [(identifier) @_superclass
                        (attribute (identifier) @_superclass)])
        (:equal "TestCase" @_superclass)
        body: (block
               (function_definition
                name: (identifier) @method-name
                (:match "^test.*" @method-name))  @start @end)
        (:pred rude-helpers--point-between-nodes-p @start @end)))))))

(defvar rude-python-ts--pytest-method-query
  (treesit-query-compile
   'python
   '((((class_definition
        name: (identifier) @class-name
        body: (block
               (function_definition
                name: (identifier) @method-name
                (:match "^test.*" @method-name)) @start @end)
        (:pred rude-helpers--point-between-nodes-p @start @end)))))))

(defun rude-python-ts-test-method (&optional debug)
  "Return command line to run a method of subclass of unittest.TestCase.
If DEBUG is set to t return a `dape' config instead."
  (when-let* ((test-runner rude-python-ts-test-runner)
              (queries (if (equal "pytest" test-runner)
                           '(rude-python-ts--pytest-method-query
                             rude-python-ts--unittest-method-query)
                         '(rude-python-ts--unittest-method-query)))
              (captures (rude-treesit-query-capture
                         'python
                         (seq-map #'symbol-value queries)))
              (method-name (treesit-node-text (alist-get 'method-name captures) t))
              (class-name (treesit-node-text (alist-get 'class-name captures) t))
              (test-file (file-relative-name buffer-file-name default-directory))
              (delimiter (if (equal "pytest" test-runner) " and " "."))
              (runner-args (format "%s -k '%s%s%s'" test-file
                                   class-name
                                   delimiter
                                   method-name)))
    (if debug
        `(debugpy-module
          command ,rude-python-ts-bin
          :module ,test-runner
          :args ,runner-args)
      (format "%s -m %s %s" rude-python-ts-bin test-runner runner-args))))

(defvar rude-python-ts--test-file-query
  (treesit-query-compile
   'python
   '((function_definition
      name: (identifier) @method-name
      (:match "^test.*" @method-name )))))

(defun rude-python-ts-test-file (&optional debug)
  "Return command line to run the current buffer as a test module.
If DEBUG is t then return `dape' configuration instead."
  (when (treesit-query-capture 'python rude-python-ts--test-file-query)
    (let ((test-runner rude-python-ts-test-runner)
          (test-file (file-relative-name buffer-file-name)))
      (cond
       (debug
        `(debugpy-module command ,rude-python-ts-bin
                         :module ,test-runner
                         :args ,test-file))
       (t
        (format "%s -m %s %s"
                rude-python-ts-bin
                test-runner
                test-file))))))

(defvar rude-python-ts--pytest-function-query
  (treesit-query-compile
   'python
   '(((function_definition
       name: (identifier) @method-name
       (:match "^test.*" @method-name)) @start @end
       (:pred rude-helpers--point-between-nodes-p @start @end)))))

(defun rude-python-ts-pytest-function (&optional debug)
  "Return command line to run the current function as pytest test.
If DEBUG is t then return `dape' configuration instead."
  (when-let* ((captures (treesit-query-capture
                         'python rude-python-ts--pytest-function-query))
              (method-name (treesit-node-text (alist-get 'method-name captures) t))
              (test-file (file-relative-name buffer-file-name))
              (test-runner-args (format "%s -k %s" test-file method-name)))
    (cond
     (debug
      `(debugpy-module command ,rude-python-ts-bin
                       :module "pytest"
                       :args ,test-runner-args) )
     (t
      (format "%s -m pytest %s" rude-python-ts-bin test-runner-args)))))

(provide 'rude-python-ts)
;;; rude-python-ts.el ends here
