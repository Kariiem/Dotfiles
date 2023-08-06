(setq-default c-syntactic-indentation t
              c-basic-offset 4)
(defun set-c-styles ()
  (c-set-style "k&r"))

(add-hook 'c-mode-hook 'set-c-styles)
(provide 'init-c)
