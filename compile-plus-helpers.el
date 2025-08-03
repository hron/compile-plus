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

(require 'treesit)

(defun compile-plus-helpers--has-point-p (match)
  "Detect if MATCH includes the point."
  (let ((beg (treesit-node-start (alist-get 'start match)))
        (end (treesit-node-end (alist-get 'end match))))
    (and (<= beg (point)) (>= end (point)))))

(defun compile-plus-treesit-query-capture
    (node queries &optional beg end node-only grouped)
  "Run `treesit-query-capture' for each of QUERIES and combine the results.
NODE BEG END NODE-ONLY GROUPED are passed through."
  (seq-reduce
   (lambda (acc query)
     (let ((captures (treesit-query-capture node
                                            (symbol-value query)
                                            beg end
                                            node-only
                                            grouped)))
       (append acc captures)))
   queries
   '()))

(provide 'compile-plus-helpers)
;;; compile-plus-helpers.el ends here
