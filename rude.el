;;; rude.el --- Run M-x `compile' based on buffer content  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025  Aleksei Gusev
;;
;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Maintainer: Aleksei Gusev <aleksei.gusev@gmail.com>
;; URL: https://github.com/hron/rude.el
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

(require 'rude-rust-ts)
(require 'rude-python-ts)

(defvar rude-providers-alist
  '((rust-mode . (rude-rust-ts-doctest-at-point
                  rude-rust-ts-test-at-point
                  rude-rust-ts-test-mod
                  rude-rust-ts-run
                  rude-rust-ts-test-all))
    (rust-ts-mode . (rude-rust-ts-doctest-at-point
                     rude-rust-ts-test-at-point
                     rude-rust-ts-test-mod
                     rude-rust-ts-run
                     rude-rust-ts-test-all))
    (python-ts-mode . (rude-python-ts-test-method
                       rude-python-ts-test-class
                       rude-python-ts-test-file
                       rude-python-ts-main)))
  "Contains functions to provide candidates per mode.")

(defvar rude-default-directory-alist
  '((rust-mode . rude-rust-ts-default-directory)
    (rust-ts-mode . rude-rust-ts-default-directory)
    (rustic-mode . rude-rust-ts-default-directory)))

(defgroup rude nil
  "Run \\[compile] based on the buffer content."
  :link '(url-link :tag "Website" "https://github.com/hron/rude.el")
  :link '(emacs-library-link :tag "Library Source" "rude.el")
  :group 'compilation
  :prefix "rude-")

(defun rude--providers-for-current-buffer ()
  "Return list of providers for the current buffer."
  (alist-get major-mode rude-providers-alist))

(defun rude-compile-command (&optional debug)
  "Build `compile-command' for the thing at point.
if DEBUG is set to t return `dape-command' instead."
  (or (catch 'found
        (dolist (func (rude--providers-for-current-buffer))
          (with-demoted-errors "Error in a provider func: %S"
            (when-let* ((candidate-command (funcall func debug)))
              (throw 'found candidate-command)))))
      compile-command))

(defun rude-dape-command ()
  "Build `dape-command' for the thing at point."
  (rude-compile-command t))

(defun rude-default-directory ()
  "Return the directory suitable for rude command."
  (let ((fn (alist-get major-mode rude-default-directory-alist)))
    (cond
     ((fboundp fn)
      (funcall fn))
     ((project-current nil)
      (project-root (project-current nil)))
     (t
      default-directory))))

;;;###autoload
(defun rude-compile-thing-at-point ()
  "Call `compile' to run thing at point (test, main function etc)."
  (interactive)
  (let ((default-directory (rude-default-directory)))
    (setq compile-command (rude-compile-command))
    (call-interactively #'compile)))

;;;###autoload
(defun rude-dape-thing-at-point ()
  "Call `dape' to run thing at point (test, main function etc)."
  (interactive)
  (let* ((default-directory (rude-default-directory))
         (dape-command (rude-compile-command t)))
    (ignore dape-command)
    (call-interactively #'dape)))

(provide 'rude)
;;; rude.el ends here
