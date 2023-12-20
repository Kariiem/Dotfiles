;; -*- lexical-binding: t -*-
(setq c-default-style "stroustrup"
      c-syntactic-indentation t)

(with-eval-after-load 'c-mode
  (defun c-help()
  (interactive)
  (other-window-prefix)
  (man (concat (word-at-point t) "(2)")))
  (define-key c-mode-map (kbd "C-c C-i") 'c-help))
(provide 'init-c)
