;; -*- lexical-binding: t -*-

(install-pkgs rfc-mode)
(with-eval-after-load 'rfc-mode
    (setq rfc-mode-directory (expand-file-name "~/rfc/"))
    (define-key rfc-mode-map "[" 'backward-page)
    (define-key rfc-mode-map "]" 'forward-page))
(provide 'init-rfc)
