;;; -*- lexical-binding: t -*-

(install-pkgs nov)

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(setq nov-variable-pitch nil)
(setq nov-text-width 100)
(setq nov-header-line-format nil)

(defun setup-nov()
  (display-line-numbers-mode -1)
  (olivetti-mode)
  (olivetti-set-width 110))

(add-hook 'nov-mode-hook 'setup-nov)
(provide 'init-epub)
