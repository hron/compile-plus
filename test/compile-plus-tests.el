;;; compile-plus-tests.el --- compile-plus tests     -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Aleksei Gusev

;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Keywords: tools

;;; Code:

(require 'compile-plus)
(require 'ert)

(defun sample-provider ()
  "Sample provider that always return the same list."
  '("Sample #1" "Sample #2"))

(defun dont-match-provider ()
  "Sample provider that always return the same list."
  '("Don't match"))

(ert-deftest find-providers-for-major-mode ()
  (with-temp-buffer
    (let ((compile-plus-providers-alist
           '((c-mode . (sample-provider))
             (text-mode . (dont-match-provider)))))
      (c-mode)
      (compile-plus-mode +1)
      (should (equal (compile-plus--build-future-history)
                     '("Sample #1" "Sample #2"))))))

(ert-deftest override-providers ()
  (with-temp-buffer
    (let ((compile-plus-providers-alist
           '((text-mode . (dont-match-provider))))
          (compile-plus-override-providers '(sample-provider)))
      (text-mode)
      (compile-plus-mode +1)
      (should (equal (compile-plus--build-future-history)
                     '("Sample #1" "Sample #2"))))))
