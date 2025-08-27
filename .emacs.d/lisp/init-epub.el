;;; -*- lexical-binding: t -*-

(install-pkgs nov)

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(defun setup-nov()
  (display-line-numbers-mode -1)
  (olivetti-mode)
  (olivetti-set-width 90))

(add-hook 'nov-mode-hook 'setup-nov)
(provide 'init-epub)
