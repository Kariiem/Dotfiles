;; -*- lexical-binding: t -*-

(when (getenv "WSL_DISTRO_NAME")
  (setq ls-lisp-use-insert-directory-program nil)
  (require 'ls-lisp))

(setq read-only-dirs '("~/git/bsd-user")
      dired-dwim-target t
      dired-listing-switches "-ADlh --sort=time \"--time-style=+%d/%m/%Y %H:%M\" --group-directories-first --file-type")

(defun string-prefix-in-list (dir-list fname)
  (cl-some (lambda (dir) (string-prefix-p dir fname))
	       dir-list))

(defun set-read-only ()
  (let ((f (or (expand-file-name (or buffer-file-name "")) "")))
    (if (string-prefix-in-list (mapcar 'expand-file-name read-only-dirs) f)
	    (read-only-mode 1))))

(with-eval-after-load 'dired
  (require 'dired-x)
  (setq dired-omit-files
        (concat dired-omit-files "\\|\\`[.].+\\'")))

(add-hook 'prog-mode-hook 'set-read-only)
(add-hook 'dired-mode-hook (lambda ()
                             (dired-hide-details-mode -1)
                             (dired-omit-mode -1)
                             (visual-line-mode 1)))

(setq dired-kill-when-opening-new-dired-buffer t)

(add-hook 'find-file-hook 'set-read-only)

(provide 'init-dired)
