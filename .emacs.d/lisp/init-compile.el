;; -*- lexical-binding: t; -*-

(defun recompile-with-edit ()
  "Re-compile when in the Compilation mode buffer with command editing."
  (interactive)
  (recompile t))

(with-eval-after-load 'compile
  (define-key compilation-mode-map "r" 'recompile-with-edit))
(provide 'init-compile)
