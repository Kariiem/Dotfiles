;; -*- lexical-binding: t; -*-

(setq compilation-ask-about-save nil
      compilation-always-kill t
      compilation-scroll-output 'first-error)

(defun recompile-with-edit ()
  "Re-compile when in the Compilation mode buffer with command editing."
  (interactive)
  (recompile t))

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(with-eval-after-load 'compile
  (define-key compilation-mode-map "r" 'recompile-with-edit))
(provide 'init-compile)
