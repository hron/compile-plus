;;; compile-plus-test-helpers.el --- Test helpers    -*- lexical-binding: t; -*-;;
;;
;; Copyright (C) 2025  Aleksei Gusev
;;
;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Keywords: lisp
;; SPDX-License-Identifier: GPL
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'find-func)

(defconst compile-plus-project-dir
  (expand-file-name ".." (find-library-name "compile-plus"))
  "Stores project root.")

(defmacro with-sample-file (file-path mode &rest body)
  "Execute BODY in context of FILE-PATH from test fixtures directory.
Use MODE as major mode."
  (declare (indent 2))
  `(let* ((fixture-relpath (concat "test/fixtures/" ,file-path))
          (fixture-path (expand-file-name fixture-relpath compile-plus-project-dir))
          (buffer (find-file-noselect fixture-path)))
     (with-current-buffer buffer
       (unwind-protect
           (goto-char (point-min))
         (funcall ,mode)
         (progn ,@body)
         (kill-buffer buffer)))))

(provide 'compile-plus-test-helpers)
;;; compile-plus-test-helpers.el ends here
