;; -*- lexical-binding: t -*-

(install-pkgs rg)


(rg-enable-default-bindings)

(with-eval-after-load 'rg
  (setq rg-hide-command nil
        rg-command-line-flags (list "--sort=path"))
  (setq transient-default-level 7))

;; (add-to-list 'display-buffer-alist
;;              '((major-mode . rg-mode)
;;                (display-buffer-reuse-window
;;                 display-buffer-in-side-window)
;;                (reusable-frames . visible)
;;                (side . top)
;;                (window-height . 0.25)))
(provide 'init-rg)
