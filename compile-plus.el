;;; compile-plus.el --- Run M-x `compile' based on buffer content  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025  Aleksei Gusev
;;
;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Maintainer: Aleksei Gusev <aleksei.gusev@gmail.com>
;; URL: https://github.com/hron/compile-plus.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, processes, compile, test
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

(defvar compile-plus-providers-alist
  '((rust-ts-mode . (compile-plus-rust-ts-all compile-plus-rust-ts-test-under-cursor)))
  "Contains functions to provide candidates per mode.")

(defcustom compile-plus-override-providers nil
  "Overrides providers for current buffer."
  :local t
  :risky t)

(defun compile-plus--build-future-history ()
  "Returns a list of commands that is used as future history for `compile'."
  (let ((provider-funcs (or compile-plus-override-providers
                            (alist-get major-mode compile-plus-providers-alist)))
        (result '()))
    (dolist (func provider-funcs)
      (push (funcall func) result))
    (flatten-list result)))

(defun compile-plus--read-command (command)
  "Copy of `compile-read-command', except provides future history.
See `compile-read-command' documentation for COMMAND meaning."
  (read-shell-command "Compile command: " command
                      (if (equal (car compile-history) command)
                          '(compile-history . 1)
                        'compile-history)
                      (compile-plus--build-future-history)))

;;;###autoload
(define-minor-mode compile-plus-mode
  "Extends `compile' command with future history based on the context."
  :global t :lighter nil
  (advice-remove 'compilation-read-command #'compile-plus--read-command)
  (when compile-plus-mode
    (advice-add 'compilation-read-command :override #'compile-plus--read-command)))

(provide 'compile-plus)
;;; compile-plus.el ends here
