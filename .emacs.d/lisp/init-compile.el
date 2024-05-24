;; -*- lexical-binding: t; -*-

(setq compilation-ask-about-save nil
      compilation-always-kill t
      compilation-scroll-output 'first-error)

(defun recompile-with-edit ()
  "Re-compile when in the Compilation mode buffer with command editing."
  (interactive)
  (recompile t))

(defun compilation-cd ()
  (interactive)
  (when (eq major-mode 'compilation-mode)
    (setq-local compilation-directory
                (read-directory-name "Compilation directory: "
                                     compilation-directory))
    (recompile-with-edit)))

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(with-eval-after-load 'compile
  (define-key compilation-mode-map "d" 'compilation-cd)
  (define-key compilation-mode-map "r" 'recompile-with-edit))
(provide 'init-compile)
