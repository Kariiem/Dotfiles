;;; -*- lexical-binding: t -*-

(install-pkgs nov)

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(provide 'init-epub)
