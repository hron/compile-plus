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
    '("Sample #1")))

(defun sample-provider-2 (&optional debug)
  "Sample provider that always return the same list.
DEBUG"
  (if debug
      '(launch command "sample-command-2")
    '("Sample #2")))

(defun dont-match-provider (&optional debug)
  "Sample provider that always return the same list.
If DEBUG is t then return dape-config."
  (if debug
      '(launch command "Don't match provider!")
    '("Don't match")))

(ert-deftest compile-future-history-for-major-mode ()
  (with-temp-buffer
    (let ((compile-plus-providers-alist
           '((c-mode . (sample-provider-1 sample-provider-2))
             (text-mode . (dont-match-provider)))))
      (c-mode)
      (compile-plus-mode +1)
      (should (equal (compile-plus--build-future-history)
                     '("Sample #1" "Sample #2"))))))

(ert-deftest compile-future-history-override-providers ()
  (with-temp-buffer
    (let ((compile-plus-providers-alist
           '((text-mode . (dont-match-provider))))
          (compile-plus-override-providers '(sample-provider-1 sample-provider-2)))
      (text-mode)
      (compile-plus-mode +1)
      (should (equal (compile-plus--build-future-history)
                     '("Sample #1" "Sample #2"))))))

(ert-deftest compile-future-history-rescue-errors ()
  (with-temp-buffer
    (let ((debug-on-error nil)
          (compile-plus-providers-alist
           '((text-mode . ((lambda () (error "Needed external file doesn't exist")))))))
      (text-mode)
      (compile-plus-mode +1)
      (should (equal (compile-plus--build-future-history)
                     '())))))

(ert-deftest dape-command-for-major-mode ()
  (with-temp-buffer
    (let ((compile-plus-providers-alist
           '((c-mode . (sample-provider-1 sample-provider-2))
             (text-mode . (dont-match-provider)))))
      (c-mode)
      (should (equal (compile-plus-dape-command)
                     '(launch command "sample-command-1"))))))
