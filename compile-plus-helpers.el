(defun compile-plus-helpers--has-point-p (match)
  "Detect if MATCH includes the point."
  (let ((beg (treesit-node-start (alist-get 'start match)))
        (end (treesit-node-end (alist-get 'end match))))
    (and (<= beg (point)) (>= end (point)))))

(provide 'compile-plus-helpers)
;;; compile-plus-helpers.el ends here
