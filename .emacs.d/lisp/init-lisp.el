;; -*- lexical-binding: t -*-

;; (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
;; (add-hook 'scheme-mode-hook (lambda () (lispy-mode 1)))
;; (defun conditionally-enable-lispy ()
;;   (when (eq this-command 'eval-expression)
;;     (lispy-mode 1)))
;; (add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy)
(install-pkgs paredit
              slime)
(paredit-mode)
(setq inferior-lisp-program "sbcl")
(provide 'init-lisp)
