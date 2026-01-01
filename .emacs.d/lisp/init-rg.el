;; -*- lexical-binding: t -*-

(install-pkgs rg)


(add-hook 'after-init-hook #'rg-enable-default-bindings)

(with-eval-after-load 'rg
  (setq transient-default-level 7))

(add-to-list 'display-buffer-alist
             '((major-mode . rg-mode)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side . top)
               (window-height . 0.25)))
(provide 'init-rg)
