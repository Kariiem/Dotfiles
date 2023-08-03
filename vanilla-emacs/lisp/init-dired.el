(setq read-only-dirs '("~/git/bsd-user"))
(defun string-prefix-in-list (dir-list fname)
  (cl-some (lambda (dir) (string-prefix-p dir fname))
	   dir-list))

(defun set-read-only ()
  (let ((f (or (expand-file-name (or buffer-file-name "")) "")))
    (if (string-prefix-in-list (mapcar 'expand-file-name read-only-dirs) f)
	(read-only-mode 1))))

(add-hook 'prog-mode-hook 'set-read-only)

(add-hook 'dired-mode-hook (lambda ()
                             (dired-hide-details-mode 1)
                             (dired-omit-mode 1)
                             (visual-line-mode 1)))
(setq dired-kill-when-opening-new-dired-buffer t)

(provide 'init-dired)
