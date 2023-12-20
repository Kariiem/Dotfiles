;; -*- lexical-binding: t; -*-

(require-package 'ruby-end)
(require-package 'inf-ruby)
(require-package 'robe)

(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-ts-mode-hook 'robe-mode)

(provide 'init-ruby)
