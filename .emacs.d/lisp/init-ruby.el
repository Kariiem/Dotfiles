;; -*- lexical-binding: t; -*-

(install-pkgs ruby-end
              inf-ruby
              robe)

(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-ts-mode-hook 'robe-mode)

(provide 'init-ruby)
