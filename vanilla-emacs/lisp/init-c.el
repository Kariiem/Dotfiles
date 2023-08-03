(add-hook 'c-mode-hook (lambda () (c-set-style "linux")))
(setq-default c-indentation-style "linux"
              c-syntactic-indentation t
              c-basic-offset 4)
(defun set-c-offsets ()
  (c-set-offset 'defun-block-intro *))
(add-hook 'c-mode-hook 'set-c-offsets)
(provide 'init-c)
