;;; rude-python-ts-tests.el --- rude-python-ts tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Aleksei Gusev

;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Keywords: tools

;;; Code:

(require 'rude)
(require 'ert)

(unless (macrop 'with-sample-file)
  (load (expand-file-name "./rude-test-helpers"
                          (file-name-directory load-file-name))))

(ert-deftest python-ts-main ()
  (with-sample-file "python-ts/main.py" #'python-ts-mode
    (search-forward "if __name__ == '__main__'")
    (should (equal (rude-python-ts-main) "python3 main.py"))
    (should (equal (rude-python-ts-main t)
                   '(debugpy :program "main.py")))))

(ert-deftest python-ts-unittest-file ()
  (let ((rude-python-ts-test-runner "unittest"))
    (with-sample-file "python-ts/test_unittest.py" #'python-ts-mode
      (search-forward "import")
      (should (equal (rude-python-ts-test-file)
                     "python3 -m unittest test_unittest.py"))
      (should (equal (rude-python-ts-test-file t)
                     '(debugpy-module
                       command "python3"
                       :module "unittest"
                       :args "test_unittest.py"))))))

(ert-deftest python-ts-unittest-class ()
  (let ((rude-python-ts-test-runner "unittest"))
    (with-sample-file "python-ts/test_unittest.py" #'python-ts-mode
      (search-forward "class TestStringMethods")
      (should
       (equal (rude-python-ts-test-class)
              "python3 -m unittest test_unittest.py -k TestStringMethods"))
      (should
       (equal (rude-python-ts-test-class t)
              '(debugpy-module
                command "python3"
                :module "unittest"
                :args "test_unittest.py -k TestStringMethods"))))))

(ert-deftest python-ts-unittest-method ()
  (let ((rude-python-ts-test-runner "unittest"))
    (with-sample-file "python-ts/test_unittest.py" #'python-ts-mode
      (search-forward "def test_upper")
      (should
       (equal (rude-python-ts-test-method)
              "python3 -m unittest test_unittest.py -k 'TestStringMethods.test_upper'"))
      (should
       (equal (rude-python-ts-test-method t)
              '(debugpy-module
                command "python3"
                :module "unittest"
                :args "test_unittest.py -k 'TestStringMethods.test_upper'"))))))

(ert-deftest python-ts-unittest-method-with-point-at-beginning-of-the-line ()
  (let ((rude-python-ts-test-runner "unittest"))
    (with-sample-file "python-ts/test_unittest.py" #'python-ts-mode
      (search-forward "def test_upper")
      (beginning-of-line)
      (should
       (equal (rude-python-ts-test-method)
              "python3 -m unittest test_unittest.py -k 'TestStringMethods.test_upper'")))))

(ert-deftest python-ts-unittest-file-with-pytest ()
  (let ((rude-python-ts-test-runner "pytest"))
    (with-sample-file "python-ts/test_unittest.py" #'python-ts-mode
      (search-forward "import")
      (should (equal (rude-python-ts-test-file) "python3 -m pytest test_unittest.py"))
      (should (equal (rude-python-ts-test-file t)
                     '(debugpy-module command "python3" :module "pytest" :args "test_unittest.py"))))))

(ert-deftest python-ts-unittest-class-with-pytest ()
  (let ((rude-python-ts-test-runner "pytest"))
    (with-sample-file "python-ts/test_unittest.py" #'python-ts-mode
      (search-forward "class TestStringMethods")
      (should (equal (rude-python-ts-test-class)
                     "python3 -m pytest test_unittest.py -k TestStringMethods"))
      (should (equal (rude-python-ts-test-class t)
                     '(debugpy-module command "python3"
                                      :module "pytest"
                                      :args "test_unittest.py -k TestStringMethods"))))))

(ert-deftest python-ts-unittest-class-without-prefix-with-pytest ()
  (let ((rude-python-ts-test-runner "pytest"))
    (with-sample-file "python-ts/test_unittest.py" #'python-ts-mode
      (search-forward "class SomeClassWithoutPrefix")
      (should (equal (rude-python-ts-test-class)
                     "python3 -m pytest test_unittest.py -k SomeClassWithoutPrefix"))
      (should (equal (rude-python-ts-test-class t)
                     '(debugpy-module command "python3"
                                      :module "pytest"
                                      :args "test_unittest.py -k SomeClassWithoutPrefix"))))))

(ert-deftest python-ts-unittest-method-with-pytest ()
  (let ((rude-python-ts-test-runner "pytest"))
    (with-sample-file "python-ts/test_unittest.py" #'python-ts-mode
      (search-forward "def test_upper")
      (should (equal (rude-python-ts-test-method)
                     "python3 -m pytest test_unittest.py -k 'TestStringMethods and test_upper'"))
      (should (equal (rude-python-ts-test-method t)
                     '(debugpy-module command "python3"
                                      :module "pytest"
                                      :args "test_unittest.py -k 'TestStringMethods and test_upper'"))))))

(ert-deftest python-ts-pytest-file ()
  (let ((rude-python-ts-test-runner "pytest"))
    (with-sample-file "python-ts/test_pytest.py" #'python-ts-mode
      (goto-char (point-max))
      (should (equal (rude-python-ts-test-file)
                     "python3 -m pytest test_pytest.py"))
      (should (equal (rude-python-ts-test-file t)
                     '(debugpy-module command "python3"
                                      :module "pytest"
                                      :args "test_pytest.py"))))))

(ert-deftest python-ts-pytest-class ()
  (let ((rude-python-ts-test-runner "pytest"))
    (with-sample-file "python-ts/test_pytest.py" #'python-ts-mode
      (search-forward "class TestPytestClass")
      (should (equal (rude-python-ts-test-class)
                     "python3 -m pytest test_pytest.py -k TestPytestClass"))
      (should (equal (rude-python-ts-test-class t)
                     '(debugpy-module command "python3"
                                      :module "pytest"
                                      :args "test_pytest.py -k TestPytestClass"))))))

(ert-deftest python-ts-pytest-function ()
  (let ((rude-python-ts-test-runner "pytest"))
    (with-sample-file "python-ts/test_pytest.py" #'python-ts-mode
      (search-forward "def test_function")
      (should (equal (rude-python-ts-pytest-function)
                     "python3 -m pytest test_pytest.py -k test_function"))
      (should (equal (rude-python-ts-pytest-function t)
                     '(debugpy-module command "python3"
                                      :module "pytest"
                                      :args "test_pytest.py -k test_function"))))))
