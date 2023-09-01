;; (require-package 'ivy)
;;(require-package 'vertico)
(require-package 'yasnippet)
(require-package 'yasnippet-snippets)
(require-package 'company)

(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "M-SPC") 'yas-maybe-expand)
  (define-key yas-minor-mode-map (kbd "C-c y") 'yas-expand))


(fido-vertical-mode 1)
(define-key icomplete-vertical-mode-minibuffer-map (kbd "<tab>") 'icomplete-force-complete)


(setq completion-auto-help nil)
(setq company-selection-wrap-around t
      company-dabbrev-downcase 0
      company-idle-delay 0
      company-minimum-prefix-length 3)

(with-eval-after-load 'icomplete
  (custom-set-faces '(icomplete-selected-match ((t (:foreground "#ffff00" :background "#52494e"))))))

(savehist-mode 1)

(provide 'init-completions)
