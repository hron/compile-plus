;;; compile-plus.el --- Run M-x `compile' based on buffer content  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025  Aleksei Gusev
;;
;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Maintainer: Aleksei Gusev <aleksei.gusev@gmail.com>
;; URL: https://github.com/hron/compile-plus.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
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

(defgroup compile-plus nil
  "Run \\[compile] based on the buffer content."
  :link '(url-link :tag "Website" "https://github.com/hron/compile-plus")
  :link '(emacs-library-link :tag "Library Source" "compile-plus.el")
  :group 'compilation
  :prefix "compile-plus-")

(defcustom compile-plus-override-providers nil
  "Override compile-plus providers for the current buffer."
  :tag "Override Providers"
  :type '(function)
  :group 'compile-plus
  :local t
  :risky t)

(defun compile-plus--build-future-history ()
  "Returns a list of commands that is used as future history for `compile'."
  (let ((provider-funcs (reverse
                         (or compile-plus-override-providers
                             (alist-get major-mode compile-plus-providers-alist))))
        (result '()))
    (dolist (func provider-funcs)
      (push (funcall func) result))
    (seq-map #'substring-no-properties (flatten-list result))))

(defcustom compile-plus-replace-compile-command nil
  "Replace `compile-command' with first entry of future history."
  :type 'boolean
  :group 'compile-plus
  :tag "Replace compile-command")

(defun compile-plus--read-command (command)
  "Copy of `compile-read-command', except provides future history.
Also it uses the first future history entry as default if the passed
COMMAND is nil."
  (let* ((future-history (compile-plus--build-future-history))
         (initial-content (if compile-plus-replace-compile-command
                              (car future-history)
                            command))
         (future-history (if compile-plus-replace-compile-command
                             (cdr future-history)
                           future-history)))
    (read-shell-command "Compile command: "
                        initial-content
                        'compile-history
                        future-history)))

;;;###autoload
(define-minor-mode compile-plus-mode
  "Extends `compile' command with future history based on the context."
  :global t :lighter nil
  (advice-remove 'compilation-read-command #'compile-plus--read-command)
  (when compile-plus-mode
    (advice-add 'compilation-read-command :override #'compile-plus--read-command)))

(provide 'compile-plus)
;;; compile-plus.el ends here
