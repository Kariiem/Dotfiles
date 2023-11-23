;; -*- lexical-binding: t -*-
(require-package 'geiser)
(require-package 'geiser-chez)
(require-package 'geiser-guile)
(require-package 'geiser-gambit)
(require-package 'geiser-mit)
(require-package 'geiser-racket)
(require-package 'macrostep-geiser)

(with-eval-after-load 'geiser
  (setq geiser-mode-start-repl-p t
        geiser-chez-binary "chez"))
(paredit-mode)
(provide 'init-scheme)
