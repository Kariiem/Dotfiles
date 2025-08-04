;; -*- lexical-binding: t -*-
(install-pkgs geiser
              geiser-chez
              geiser-guile
              geiser-gambit
              geiser-mit
              geiser-racket
              macrostep-geiser)

(defun geiser-mechanics ()
  (interactive)
  (require 'geiser-mit)
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "MITSCHEME_HEAP_SIZE" "100000")
    (setenv "MITSCHEME_BAND" "mechanics.com")
    (geiser-mit)))

(with-eval-after-load 'geiser
  (prettify-symbols-mode)
  (paredit-mode)
  (setq geiser-mode-start-repl-p t
        geiser-chez-binary "chez"))

(provide 'init-scheme)
