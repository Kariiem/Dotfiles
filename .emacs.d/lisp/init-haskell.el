;; -*- lexical-binding: t -*-

(require-package 'haskell-mode)
(require 'speedbar)
(require 'ghcid)
(require 'hindent)

(defun haskell-company-setup ()
  (set (make-local-variable 'company-backends)
       (append '((company-capf company-dabbrev-code)) company-backends)))

(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t))

(speedbar-add-supported-extension ".hs")
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'haskell-company-setup)
;;(add-hook 'haskell-mode-hook 'haskell-unicode-input-method-enable)
(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
(add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
(add-hook 'haskell-mode-hook 'hindent-mode)

(with-eval-after-load 'which-func
  (and (listp 'which-func-modes) (add-to-list 'which-func-modes 'haskell-mode)))

(with-eval-after-load 'haskell
  (define-key interactive-haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
  (setq-local prettify-symbols-alist '(("alpha"     . ?α)
                                       ("beta"      . ?β)
                                       ("gamma"     . ?γ)
                                       ("delta"     . ?δ)
                                       ("epsilon"   . ?ε)
                                       ("zeta"      . ?ζ)
                                       ("eta"       . ?η)
                                       ("theta"     . ?θ)
                                       ("iota"      . ?ι)
                                       ("kappa"     . ?κ)
                                       ("lambda"    . ?λ)
                                       ("lamda"     . ?λ)
                                       ("mu"        . ?μ)
                                       ("nu"        . ?ν)
                                       ("omicron"   . ?ο)
                                       ("xi"        . ?ξ)
                                       ("::"        . ?∷)
                                       ("forall"    . ?∀)
                                       ("exists"    . ?∃)
                                       ("->"        . ?→)
                                       ("=>"        . ?⇒)
                                       ("<="        . ?⇐)
                                       ("undefined" . ?⊥)))
  (prettify-symbols-mode))

(with-eval-after-load 'haskell-cabal
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

(provide 'init-haskell)
