;; -*- lexical-binding: t -*-
(setq c-default-style "stroustrup"
      c-syntactic-indentation t)
(add-hook 'c-mode-hook 'electric-pair-local-mode)
(defun c-help()
  (interactive)
  (other-window-prefix)
  (man (word-at-point t)))
(define-key c-mode-map (kbd "C-c C-i") 'c-help)
(provide 'init-c)
