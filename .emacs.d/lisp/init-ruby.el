;; -*- lexical-binding: t; -*-

(install-pkgs inf-ruby
              robe
              ruby-electric)

(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)

(add-to-list 'auto-mode-alist
             '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

(provide 'init-ruby)
