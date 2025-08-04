;; -*- lexical-binding: t -*-
(install-pkgs gruber-darker-theme
              dracula-theme
              ef-themes
              modus-themes
              ;; doom themes
              doom-themes
              fantom-theme
              ;; uncoloured themes
              tao-theme)

(defvar after-load-theme-hook nil)

(advice-add #'load-theme :before (lambda (&rest _) (mapc 'disable-theme custom-enabled-themes)))
(advice-add #'load-theme :after (lambda (&rest _) (run-hooks 'after-load-theme-hook)))

(custom-set-faces '(info-menu-header ((t (:family "Jetbrains Mono" :weight bold)))))

(add-hook 'after-load-theme-hook (lambda () (set-face-attribute 'region nil :extend t)))


(with-eval-after-load 'notmuch
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
    (add-hook 'notmuch-show-mode-hook 'set-notmuch-faces)))


;; (custom-set-variables
;;  '(custom-safe-themes
;;    (append '(default)
;;            (mapcar
;;             (lambda (filename)
;;               (with-temp-buffer
;;                 (insert-file-contents filename)
;;                 (secure-hash 'sha256 (buffer-string))))
;;             (file-expand-wildcards (concat user-emacs-directory
;;                                            "elpa/*/*-theme.el"))))))
(provide 'init-theme)
