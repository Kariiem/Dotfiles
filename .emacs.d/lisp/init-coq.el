;; -*- lexical-binding: t -*-

(install-pkgs proof-general
              company-coq)
(add-hook 'coq-mode-hook #'company-coq-mode)

(with-eval-after-load 'proof-general
  (set-fontset-font t 'unicode (font-spec :name "Symbola") nil 'prepend)
  (set-fontset-font t 'greek (font-spec :name "DejaVu Sans Mono") nil 'prepend))

(provide 'init-coq)
