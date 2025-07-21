;;; compile-plus-tests.el --- compile-plus tests     -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Aleksei Gusev

;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Keywords: tools

;;; Code:

(require 'compile-plus)
(require 'ert)

(defun compile-plus-tests--sample-provider ()
  "Sample provider that always return the same list."
  '("Sample #1" "Sample #2"))

(defun compile-plus-tests--dont-match-provider ()
  "Sample provider that always return the same list."
  '("Don't match"))

(ert-deftest compile-plus-tests--find-providers-for-major-mode ()
  (with-temp-buffer
    (let ((compile-plus-providers-alist
           '((c-mode . (compile-plus-tests--sample-provider))
             (text-mode . (compile-plus-tests--dont-match-provider)))))
      (c-mode)
      (compile-plus-mode +1)
      (should (equal (compile-plus--build-future-history)
                     '("Sample #1" "Sample #2"))))))

(ert-deftest compile-plus-tests--override-providers ()
  (with-temp-buffer
    (let ((compile-plus-providers-alist
           '((text-mode . (compile-plus-tests--dont-match-provider))))
          (compile-plus-override-providers '(compile-plus-tests--sample-provider)))
      (text-mode)
      (compile-plus-mode +1)
      (should (equal (compile-plus--build-future-history)
                     '("Sample #1" "Sample #2"))))))
