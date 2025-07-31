;;; compile-plus-python-ts-tests.el --- compile-plus-python-ts tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Aleksei Gusev

;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Keywords: tools

;;; Code:

(require 'compile-plus)
(require 'compile-plus-test-helpers)
(require 'ert)

(ert-deftest python-ts-main ()
  (with-sample-file "python-ts/test_unittest.py" #'python-ts-mode
    (search-forward "if __name__ == '__main__'")
    (should (equal (compile-plus--build-future-history)
                   '("python3 test_unittest.py")))))

(ert-deftest python-ts-unittest-class ()
  (with-sample-file "python-ts/test_unittest.py" #'python-ts-mode
    (search-forward "class TestStringMethods")
    (should (equal (compile-plus--build-future-history)
                   '("python3 -m unittest test_unittest.TestStringMethods"
                     "python3 test_unittest.py")))))

(ert-deftest python-ts-unittest-method ()
  (with-sample-file "python-ts/test_unittest.py" #'python-ts-mode
    (search-forward "def test_upper")
    (should (equal (compile-plus--build-future-history)
                   '("python3 -m unittest test_unittest.TestStringMethods.test_upper"
                     "python3 -m unittest test_unittest.TestStringMethods"
                     "python3 test_unittest.py")))))
