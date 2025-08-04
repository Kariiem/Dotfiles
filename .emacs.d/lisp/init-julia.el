;; -*- lexical-binding: t -*-
(install-pkgs julia-mode
              julia-repl
              julia-snail
              vterm)

(add-hook 'julia-mode-hook #'julia-snail-mode)
(with-eval-after-load 'julia-snail
  (setq julia-snail-repl-display-eval-results t
        julia-snail-popup-display-eval-results :command))

;; (add-hook 'julia-mode-hook 'julia-repl-mode)
(with-eval-after-load 'julia-repl-mode
  (julia-repl-set-terminal-backend 'vterm))
(provide 'init-julia)
