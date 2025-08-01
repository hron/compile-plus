;;; compile-plus-python-ts-tests.el --- compile-plus-python-ts tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Aleksei Gusev

;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Keywords: tools

;;; Code:

(require 'compile-plus)
(require 'compile-plus-test-helpers)
(require 'ert)

(ert-deftest python-ts-main ()
  (with-sample-file "python-ts/main.py" #'python-ts-mode
    (search-forward "if __name__ == '__main__'")
    (should (equal (compile-plus--build-future-history)
                   '("python3 main.py")))))

(ert-deftest python-ts-unittest-file ()
  (let ((compile-plus-python-ts-test-runner "unittest"))
    (with-sample-file "python-ts/test_unittest.py" #'python-ts-mode
      (search-forward "import")
      (should (equal (compile-plus--build-future-history)
                     '("python3 -m unittest test_unittest.py"))))))

(ert-deftest python-ts-unittest-class ()
  (let ((compile-plus-python-ts-test-runner "unittest"))
    (with-sample-file "python-ts/test_unittest.py" #'python-ts-mode
      (search-forward "class TestStringMethods")
      (should (equal (compile-plus--build-future-history)
                     '("python3 -m unittest test_unittest.TestStringMethods"
                       "python3 -m unittest test_unittest.py"))))))

(ert-deftest python-ts-unittest-method ()
  (let ((compile-plus-python-ts-test-runner "unittest"))
    (with-sample-file "python-ts/test_unittest.py" #'python-ts-mode
      (search-forward "def test_upper")
      (should (equal (compile-plus--build-future-history)
                     '("python3 -m unittest test_unittest.TestStringMethods.test_upper"
                       "python3 -m unittest test_unittest.TestStringMethods"
                       "python3 -m unittest test_unittest.py"))))))

(ert-deftest python-ts-unittest-file-with-pytest ()
  (let ((compile-plus-python-ts-test-runner "pytest"))
    (with-sample-file "python-ts/test_unittest.py" #'python-ts-mode
      (search-forward "import")
      (should (equal (compile-plus--build-future-history)
                     '("python3 -m pytest test_unittest.py"))))))

(ert-deftest python-ts-unittest-class-with-pytest ()
  (let ((compile-plus-python-ts-test-runner "pytest"))
    (with-sample-file "python-ts/test_unittest.py" #'python-ts-mode
      (search-forward "class TestStringMethods")
      (should (equal (compile-plus--build-future-history)
                     '("python3 -m pytest test_unittest.TestStringMethods"
                       "python3 -m pytest test_unittest.py"))))))

(ert-deftest python-ts-unittest-class-without-prefix-with-pytest ()
  (let ((compile-plus-python-ts-test-runner "pytest"))
    (with-sample-file "python-ts/test_unittest.py" #'python-ts-mode
      (search-forward "class SomeClassWithoutPrefix")
      (should (equal (compile-plus--build-future-history)
                     '("python3 -m pytest test_unittest.SomeClassWithoutPrefix"
                       "python3 -m pytest test_unittest.py"))))))

(ert-deftest python-ts-unittest-method-with-pytest ()
  (let ((compile-plus-python-ts-test-runner "pytest"))
    (with-sample-file "python-ts/test_unittest.py" #'python-ts-mode
      (search-forward "def test_upper")
      (should (equal (compile-plus--build-future-history)
                     '("python3 -m pytest test_unittest.TestStringMethods.test_upper"
                       "python3 -m pytest test_unittest.TestStringMethods"
                       "python3 -m pytest test_unittest.py"))))))

(ert-deftest python-ts-pytest-file ()
  (let ((compile-plus-python-ts-test-runner "pytest"))
    (with-sample-file "python-ts/test_pytest.py" #'python-ts-mode
      (goto-char (point-max))
      (should (equal (compile-plus--build-future-history)
                     '("python3 -m pytest test_pytest.py"))))))

(ert-deftest python-ts-pytest-class ()
  (let ((compile-plus-python-ts-test-runner "pytest"))
    (with-sample-file "python-ts/test_pytest.py" #'python-ts-mode
      (search-forward "class TestPytestClass")
      (should (equal (compile-plus--build-future-history)
                     '("python3 -m pytest test_pytest.TestPytestClass"
                       "python3 -m pytest test_pytest.py"))))))
