;;; compile-plus.el --- Run M-x `compile' based on buffer content  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025  Aleksei Gusev
;;
;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Maintainer: Aleksei Gusev <aleksei.gusev@gmail.com>
;; URL: https://github.com/hron/compile-plus.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (dape "0.24.1"))
;; Keywords: convenience, compile, test
;; SPDX-License-Identifier: GPL
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Run M-x `compile' based on buffer content
;;
;;; Code:

(require 'compile)
(require 'dape)

(require 'compile-plus-rust-ts)
(require 'compile-plus-python-ts)

(defvar compile-plus-providers-alist
  '((rust-mode . (compile-plus-rust-ts-doctest-at-point
                  compile-plus-rust-ts-test-at-point
                  compile-plus-rust-ts-test-mod
                  compile-plus-rust-ts-run
                  compile-plus-rust-ts-test-all))
    (rust-ts-mode . (compile-plus-rust-ts-doctest-at-point
                     compile-plus-rust-ts-test-at-point
                     compile-plus-rust-ts-test-mod
                     compile-plus-rust-ts-run
                     compile-plus-rust-ts-test-all))
    (python-ts-mode . (compile-plus-python-ts-test-method
                       compile-plus-python-ts-test-class
                       compile-plus-python-ts-test-file
                       compile-plus-python-ts-main)))
  "Contains functions to provide candidates per mode.")

(defvar compile-plus-default-directory-alist
  '((rust-mode . compile-plus-rust-ts-default-directory)
    (rust-ts-mode . compile-plus-rust-ts-default-directory)
    (rustic-mode . compile-plus-rust-ts-default-directory)))

(defgroup compile-plus nil
  "Run \\[compile] based on the buffer content."
  :link '(url-link :tag "Website" "https://github.com/hron/compile-plus")
  :link '(emacs-library-link :tag "Library Source" "compile-plus.el")
  :group 'compilation
  :prefix "compile-plus-")

(defun compile-plus--providers-for-current-buffer ()
  "Return list of providers for the current buffer."
  (alist-get major-mode compile-plus-providers-alist))

(defun compile-plus-compile-command (&optional debug)
  "Build `compile-command' for the thing at point.
if DEBUG is set to t return `dape-command' instead."
  (or (catch 'found
        (dolist (func (compile-plus--providers-for-current-buffer))
          (with-demoted-errors "Error in a provider func: %S"
            (when-let* ((candidate-command (funcall func debug)))
              (throw 'found candidate-command)))))
      compile-command))

(defun compile-plus-dape-command ()
  "Build `dape-command' for the thing at point."
  (compile-plus-compile-command t))

(defun compile-plus-default-directory ()
  "Return the directory suitable for compile-plus command."
  (let ((fn (alist-get major-mode compile-plus-default-directory-alist)))
    (cond
     ((fboundp fn)
      (funcall fn))
     ((project-current nil)
      (project-root (project-current nil)))
     (t
      default-directory))))

;;;###autoload
(defun compile-plus-compile-thing-at-point ()
  "Call `compile' to run thing at point (test, main function etc)."
  (interactive)
  (let ((default-directory (compile-plus-default-directory)))
    (setq compile-command (compile-plus-compile-command))
    (call-interactively #'compile)))

;;;###autoload
(defun compile-plus-dape-thing-at-point ()
  "Call `dape' to run thing at point (test, main function etc)."
  (interactive)
  (let* ((default-directory (compile-plus-default-directory))
         (dape-command (compile-plus-compile-command t)))
    (ignore dape-command)
    (call-interactively #'dape)))

(provide 'compile-plus)
;;; compile-plus.el ends here
