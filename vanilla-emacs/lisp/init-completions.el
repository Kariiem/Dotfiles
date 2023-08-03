;; (require-package 'ivy)
;;(require-package 'vertico)
(require-package 'yasnippet)
(require-package 'yasnippet-snippets)
(yas-global-mode 1)
;;(vertico-mode 1)
;; (ivy-mode 1)
;; (setq ivy-use-virtual-buffers t
;;        ivy-use-selectable-prompt t)
;; (ivy-define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
;; (ivy-define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
;; (ivy-define-key ivy-minibuffer-map (kbd "M-j") 'ivy-next-history-element)
;; (ivy-define-key ivy-minibuffer-map (kbd "M-k") 'ivy-previous-history-element)
;; (ivy-define-key ivy-switch-buffer-map (kbd "C-j") 'next-line)
;; (ivy-define-key ivy-switch-buffer-map (kbd "C-k") 'previous-line)
;; (define-key ivy-minibuffer-map (kbd "<tab>") '(lambda () (interactive) (ivy-partial)))


(fido-vertical-mode 1)
(setq my-icomplete-keymaps '(("<tab>" . icomplete-force-complete)
                             ("C-j"   . icomplete-forward-completions)
                             ("C-k"   . icomplete-backward-completions)))
(define-key icomplete-vertical-mode-minibuffer-map (kbd "<tab>") 'icomplete-force-complete)
;;(dolist (kv my-icomplete-keymaps)
;;  (define-key icomplete-vertical-mode-minibuffer-map (kbd (car kv)) (cdr kv)))


;; (setq enable-recursive-minibuffers nil)
;; (setq completion-styles '(basic partial-completion emacs22))
(setq completion-auto-help nil)
(require-package 'company)
(with-eval-after-load 'company
  (define-key company-active-map  (kbd "<tab>") #'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<backtab>") (lambda () (interactive) (company-complete-common-or-cycle -1))))

(setq company-selection-wrap-around t)
(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 3)
(add-hook 'after-init-hook 'global-company-mode)
(savehist-mode 1)

(provide 'init-completions)
