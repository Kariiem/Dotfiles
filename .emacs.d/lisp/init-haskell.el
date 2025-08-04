;; -*- lexical-binding: t -*-

(install-pkgs haskell-mode)

(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t))


(with-eval-after-load 'haskell
  (require 'ghcid)
  (require 'hindent)

  (and (listp 'which-func-modes) (add-to-list 'which-func-modes 'haskell-mode))
  (defun haskell-company-setup ()
    (set (make-local-variable 'company-backends)
         (append '((company-capf company-dabbrev-code)) company-backends)))

  (setq haskell-indentation-layout-offset 4)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'haskell-company-setup)
  ;;(add-hook 'haskell-mode-hook 'haskell-unicode-input-method-enable)
  (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
  (add-hook 'haskell-mode-hook 'hindent-mode)
  (add-hook 'haskell-mode-hook #'my-haskell-setup))

(with-eval-after-load 'haskell-cabal
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

(with-eval-after-load 'interactive-haskell-mode
  (define-key interactive-haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
  (add-hook 'haskell-interactive-mode-hook #'my-haskell-setup))

(defun my-haskell-setup ()
  (setq prettify-symbols-alist
        '(("alpha"     . ?α)
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
          ("<="        . ?≤)
          (">="        . ?≥)
          ("undefined" . ?⊥)))
  (prettify-symbols-mode 1))



(provide 'init-haskell)
