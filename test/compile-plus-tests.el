;;; compile-plus-tests.el --- compile-plus tests     -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Aleksei Gusev

;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Keywords: tools

;;; Code:

(require 'compile-plus)
(require 'ert)

(defun sample-provider-1 (&optional debug)
  "Sample provider that always return the same list.
DEBUG"
  (if debug
      '(launch command "sample-command-1")
    "Sample #1"))

(defun sample-provider-2 (&optional debug)
  "Sample provider that always return the same list.
DEBUG"
  (if debug
      '(launch command "sample-command-2")
    "Sample #2"))

(defun dont-match-provider (&optional debug)
  "Sample provider that always return the same list.
If DEBUG is t then return dape-config."
  (if debug
      '(launch command "Don't match provider!")
    "Don't match"))

(defun error-provider (&optional debug)
  "Sample provider function that always produces an error.
DEBUG has no meaning."
  (error "Needed external file doesn't exist"))

(ert-deftest compile-command-for-major-mode ()
  (with-temp-buffer
    (let ((compile-plus-providers-alist
           '((c-mode . (sample-provider-1 sample-provider-2))
             (text-mode . (dont-match-provider)))))
      (c-mode)
      (should (equal (compile-plus-compile-command) "Sample #1")))))

(ert-deftest compile-command-rescue-errors ()
  (with-temp-buffer
    (let ((compile-command nil)
          (debug-on-error nil)
          (compile-plus-providers-alist
           '((text-mode . (error-provider)))))
      (text-mode)
      (should (equal (compile-plus-compile-command) '())))))

(ert-deftest dape-command-for-major-mode ()
  (with-temp-buffer
    (let ((compile-plus-providers-alist
           '((c-mode . (sample-provider-1 sample-provider-2))
             (text-mode . (dont-match-provider)))))
      (c-mode)
      (should (equal (compile-plus-dape-command)
                     '(launch command "sample-command-1"))))))

(ert-deftest dape-command-rescue-errors ()
  (with-temp-buffer
    (let ((compile-command nil)
          (debug-on-error nil)
          (compile-plus-providers-alist
           '((text-mode . (error-provider)))))
      (text-mode)
      (should (equal (compile-plus-dape-command) '())))))
