;; -*- lexical-binding: t -*-
(install-pkgs geiser
              geiser-chez
              geiser-guile
              geiser-gambit
              geiser-mit
              geiser-racket
              macrostep-geiser)

(with-eval-after-load 'geiser
  (prettify-symbols-mode)
  (paredit-mode)
  (setq geiser-mode-start-repl-p t
        geiser-chez-binary "chez"))

(provide 'init-scheme)
