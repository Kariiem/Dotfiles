;; -*- lexical-binding: t -*-

;; (require-package 'lispy)

;; (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
;; (add-hook 'scheme-mode-hook (lambda () (lispy-mode 1)))
;; (defun conditionally-enable-lispy ()
;;   (when (eq this-command 'eval-expression)
;;     (lispy-mode 1)))
;; (add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy)
(require-package 'paredit)
(provide 'init-lisp)
