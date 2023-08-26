(defun disable-all-themes ()
  (mapc 'disable-theme custom-enabled-themes))

(defvar mode-theme-dict '((notmuch-hello-mode . modus-operandi-deuteranopia)))

(defvar default-theme 'gruber-darker)

(defun switch-theme ()
  (interactive)
  (let* ((mode major-mode)
        (theme (alist-get mode mode-theme-dict)))
    (disable-all-themes)
    (if theme
        (load-theme theme t)
      (load-theme default-theme t))
    (message "%s" mode)))

;;(add-hook 'after-change-major-mode-hook 'switch-theme)
;;(add-hook 'buffer-list-update-hook 'switch-theme)
