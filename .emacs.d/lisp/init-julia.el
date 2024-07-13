;; -*- lexical-binding: t -*-
(install-pkgs vterm
              julia-snail)
(with-eval-after-load 'julia-snail
  (setq julia-snail-repl-display-eval-results t
        julia-snail-popup-display-eval-results :command))
(add-hook 'julia-mode-hook #'julia-snail-mode)
(provide 'init-julia)
