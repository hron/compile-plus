;;; compile-plus-helpers.el --- Run M-x `compile' based on buffer content  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025  Aleksei Gusev
;;
;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; SPDX-License-Identifier: GPL
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Run M-x `compile' based on buffer content
;;
;;; Code:

(defun compile-plus-helpers--has-point-p (match)
  "Detect if MATCH includes the point."
  (let ((beg (treesit-node-start (alist-get 'start match)))
        (end (treesit-node-end (alist-get 'end match))))
    (and (<= beg (point)) (>= end (point)))))

(provide 'compile-plus-helpers)
;;; compile-plus-helpers.el ends here
