;;; rude-helpers.el --- Run M-x `compile' based on buffer content  -*- lexical-binding: t; -*-
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

(defun rude-helpers--point-between-nodes-p (beg end)
  "Detect if the point is between BEG and END nodes.
Use end of the line instead of point if the point is at the beginning of
the line the following char is a space."
  (let ((p (if (and (equal (point) (line-beginning-position))
                    (eq (following-char) ?\s))
               (line-end-position)
             (point))))
    (and (<= (treesit-node-start beg) p)
         (>= (treesit-node-end end) p))))

(defun rude-helpers--treesit-query-capture
    (node queries &optional beg end node-only)
  "Run `treesit-query-capture' for each of QUERIES and combine the results.
NODE BEG END NODE-ONLY GROUPED are passed through."
  (seq-reduce
   (lambda (acc query)
     (append acc (treesit-query-capture node query beg end node-only)))
   queries
   '()))

(provide 'rude-helpers)
;;; rude-helpers.el ends here
