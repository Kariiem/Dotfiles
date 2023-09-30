;; -*- lexical-binding: t -*-

(setq c-default-style "stroustrup"
      c-syntactic-indentation t)
(add-hook 'c-mode-hook 'electric-pair-local-mode)

(provide 'init-c)
