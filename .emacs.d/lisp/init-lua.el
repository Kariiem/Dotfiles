;; -*- lexical-binding: t; -*-

(require-package 'lua-mode)

(with-eval-after-load 'lua-mode
  (setq lua-indent-level 4))

(provide 'init-lua)
