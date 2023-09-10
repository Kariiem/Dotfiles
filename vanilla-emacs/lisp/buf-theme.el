(defvar per-mode-theme '((notmuch-hello-mode . modus-operandi-deuteranopia)
                         (Info-mode . modus-operandi-tinted)))

(defvar default-theme 'gruber-darker)

(defun per-mode-theme (buffer-or-string &optional no-record force-same-win)
  (with-current-buffer buffer-or-string
    (load-theme (or (alist-get `,major-mode per-mode-theme) default-theme) t)))

;;(add-hook 'after-change-major-mode-hook 'switch-theme)
;;(add-hook 'buffer-list-update-hook 'switch-theme)
(advice-add #'switch-to-buffer :before #'per-mode-theme)
