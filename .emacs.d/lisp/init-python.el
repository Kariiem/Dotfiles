;; -*- lexical-binding: t -*-

(install-pkgs elpy
              ein
              polymode
              poly-markdown)
(advice-add 'python-mode :before 'elpy-enable)
(with-eval-after-load 'ein
  (custom-set-faces
   '(ein:basecell-input-area-face ((t (:background "#000000"))))))
(with-eval-after-load 'elpy
  (highlight-indentation-mode -1)
  (indent-bars-mode)
  (setq
   indent-bars-color '(highlight :face-bg t :blend 0.2)
   indent-bars-pattern "."
   indent-bars-width-frac 0.1
   indent-bars-pad-frac 0.1
   indent-bars-zigzag nil
   indent-bars-color-by-depth nil
   indent-bars-highlight-current-depth nil
   indent-bars-display-on-blank-lines nil
   indent-bars-prefer-character t))
(provide 'init-python)
