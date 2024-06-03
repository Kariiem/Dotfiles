;; -*- lexical-binding: t -*-

(install-pkgs go-mode)
(add-hook 'go-mode-hook 'subword-mode)
(provide 'init-go)
