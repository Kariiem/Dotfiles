(require-package 'vertico)
(require-package 'yasnippet)
(require-package 'yasnippet-snippets)
(require-package 'company)

(add-hook 'prog-mode-hook 'company-mode)
(with-eval-after-load 'company
  (yas-minor-mode 1)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "M-SPC") 'yas-maybe-expand)
  (define-key yas-minor-mode-map (kbd "C-c y") 'yas-expand))

(with-eval-after-load 'vertico
  (setq vertico-cycle t
        completion-styles '(flex basic)
        vertico-preselect 'first)
  (keymap-set vertico-map "RET" #'vertico-directory-enter)
  (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
  (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word)
  (keymap-set vertico-map "C-c C-c" #'vertico-exit-input)

  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (custom-set-faces '(vertico-current ((t (:foreground "#ffff00" :background "#5a6e7e"))))))

(vertico-mode 1)


;; (advice-add #'icomplete--fido-mode-setup
;;             :after (lambda () (setq completion-styles '(flex basic))))
;; (with-eval-after-load 'icomplete
;;   (define-key icomplete-vertical-mode-minibuffer-map (kbd "<tab>") 'icomplete-force-complete)
;;   (define-key icomplete-vertical-mode-minibuffer-map (kbd "C-c C-c") 'icomplete-fido-exit)
;;   (custom-set-faces '(icomplete-selected-match ((t (:foreground "#ffff00" :background "#5a6e7e"))))
;;                     '(icomplete-first-match ((t (:foreground "#ff00ff"))))))

(custom-set-faces '(completions-common-part ((t (:foreground "#00d3ff" :weight bold))))
                  '(completions-first-difference ((t (:foreground "#ff00ff" :weight extra-bold)))))
;;(fido-vertical-mode 1)

(setq completion-auto-help nil)

(setq company-selection-wrap-around t
      company-dabbrev-downcase 0
      company-idle-delay 0
      company-minimum-prefix-length 3)


(savehist-mode 1)

(provide 'init-completions)
