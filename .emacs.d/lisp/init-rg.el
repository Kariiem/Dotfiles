;; -*- lexical-binding: t -*-

(install-pkgs rg)
(rg-enable-default-bindings)

(with-eval-after-load 'rg
  (setq transient-default-level 7))

(provide 'init-rg)
