;; -*- lexical-binding: t; -*-

(install-pkgs kotlin-mode)
(require 'kotlin-imenu)
(add-hook 'kotlin-mode-hook 'kotlin-decl-scan-mode)

(provide 'init-kotlin)
