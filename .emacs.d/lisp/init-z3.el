;;; -*- lexical-binding: t -*-
(install-pkgs z3-mode)
(add-to-list 'auto-mode-alist '("\\.smt\\'" . z3-mode))
(provide 'init-z3)
