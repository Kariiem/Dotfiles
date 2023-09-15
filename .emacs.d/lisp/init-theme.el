(require-package 'gruber-darker-theme)
(require-package 'solarized-theme)
(require-package 'dracula-theme)
(require-package 'modus-themes)

(advice-add #'load-theme :before (lambda (&rest r) (mapc 'disable-theme custom-enabled-themes)))
(load-theme 'gruber-darker t)

(custom-theme-set-faces 'gruber-darker '(widget-field ((t (:extend nil :background "gray")))))
(custom-set-faces '(info-menu-header ((t (:family "Jetbrains Mono" :weight bold)))))

(defun set-notmuch-faces ()
  (face-remap-add-relative 'notmuch-tag-face :foreground "blue")
  (face-remap-add-relative 'hl-line :background "#00ffff")
  (face-remap-add-relative 'default :foreground "#000000" :background "#f6f2f6")
  (face-remap-add-relative 'widget-button :foreground "#2376a5"))

(defun toggle-notmuch-theme ()
  (interactive)
  (add-hook 'notmuch-hello-mode-hook 'set-notmuch-faces)
  (add-hook 'notmuch-search-mode-hook 'set-notmuch-faces)
  (add-hook 'notmuch-message-mode-hook 'set-notmuch-faces)
  (add-hook 'notmuch-show-mode-hook 'set-notmuch-faces))


(provide 'init-theme)
