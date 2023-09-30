(defun key-log()
  (let ((b (get-buffer-create "*keylog*")))
    (display-buffer-in-side-window b '((side . right)))
    (with-current-buffer b
      (goto-char (point-max))
      (let ((kd (key-description (this-command-keys-vector))))
        (when (not (string-empty-p kd))
          (progn
            (insert kd)
            (newline)
            (set-window-point (get-buffer-window b) (point-max))))))))

(add-hook 'pre-command-hook 'key-log nil 'local)
