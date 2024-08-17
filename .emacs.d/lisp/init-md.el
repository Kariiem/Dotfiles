;; -*- lexical-binding: t -*-

(install-pkgs markdown-mode)
(with-eval-after-load 'markdown-mode
  (setq browse-url-browser-function #'eww-browse-url))

(provide 'init-md)
