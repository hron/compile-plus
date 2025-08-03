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

(defun compile-plus-helpers--point-between-nodes-p (beg end)
  "Detect if the point is between BEG and END nodes."
  (and (<= (treesit-node-start beg) (point))
       (>= (treesit-node-end end) (point))))

(defun compile-plus-treesit-query-capture
    (node queries &optional beg end node-only)
  "Run `treesit-query-capture' for each of QUERIES and combine the results.
NODE BEG END NODE-ONLY GROUPED are passed through."
  (seq-reduce
   (lambda (acc query)
     (let ((captures (treesit-query-capture node
                                            query
                                            beg end
                                            node-only)))
       (append acc captures)))
   queries
   '()))

(defun compile-plus--format-no-prop (&rest format-args)
  "The same as `format', but without text properties.
FORMAT-ARGS are passed through."
  (substring-no-properties (apply #'format format-args)))

(provide 'compile-plus-helpers)
;;; compile-plus-helpers.el ends here
