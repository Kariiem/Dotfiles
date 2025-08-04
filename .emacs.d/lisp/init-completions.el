;; -*- lexical-binding: t -*-

(install-pkgs vertico
              yasnippet
              yasnippet-snippets
              company
              imenu-anywhere
              imenu-list)

(add-hook 'prog-mode-hook 'company-mode)
(with-eval-after-load 'company
  (yas-minor-mode 1)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "C-c y") 'yas-expand))

(with-eval-after-load 'vertico
  (setq vertico-cycle t
        completion-styles '(basic substring partial-completion emacs22)
        vertico-resize nil
        vertico-preselect 'no-prompt)
  (keymap-set vertico-map "RET" #'vertico-directory-enter)
  (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
  (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word)
  (keymap-set vertico-map "C-c C-c" #'vertico-exit-input)

  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (custom-set-faces '(vertico-current ((t (:foreground "#ffff00" :background "#5a6e7e"))))))

(custom-set-faces '(completions-common-part ((t (:foreground "#00d3ff" :weight bold))))
                  '(completions-first-difference ((t (:foreground "#ff00ff" :weight extra-bold)))))
;;(fido-vertical-mode 1)

(setq company-tooltip-align-annotations t
      completion-auto-help nil
      company-selection-wrap-around t
      company-dabbrev-downcase 0
      company-idle-delay 0
      company-minimum-prefix-length 3
      completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      imenu-case-fold-search 0
      imenu-max-index-time 99999999999999
      imenu-auto-rescan t)

(vertico-mode)
(global-set-key (kbd "M-g a") 'imenu-anywhere)
(global-set-key (kbd "M-g l") 'imenu-list)

(provide 'init-completions)
