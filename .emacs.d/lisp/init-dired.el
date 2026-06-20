;; -*- lexical-binding: t -*-

(install-pkgs dired-subtree)


(when (getenv "WSL_DISTRO_NAME")
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

(setq read-only-dirs '()
      dired-dwim-target t
      dired-listing-switches "-ADlh --group-directories-first")

(defun string-prefix-in-list (dir-list fname)
  (cl-some (lambda (dir) (string-prefix-p dir fname))
	       dir-list))

(defun set-read-only ()
  (let ((f (or (expand-file-name (or buffer-file-name "")) "")))
    (if (string-prefix-in-list (mapcar 'expand-file-name read-only-dirs) f)
	    (read-only-mode 1))))

(with-eval-after-load 'dired
  (require 'dired-x)
  (require 'dired-subtree)
  (setq dired-omit-files (concat dired-omit-files "\\|\\`[.].+\\'")
        dired-subtree-line-prefix "  \u2502 "
        dired-subtree-use-backgrounds nil)
  (keymap-set dired-mode-map "<tab>"      #'dired-subtree-toggle)
  (keymap-set dired-mode-map "<backtab>"  #'dired-subtree-cycle))

(add-hook 'prog-mode-hook 'set-read-only)
(add-hook 'dired-mode-hook (lambda ()
                             (dired-hide-details-mode -1)
                             (dired-omit-mode -1)
                             (visual-line-mode 1)))

(setq dired-kill-when-opening-new-dired-buffer t)

(add-hook 'find-file-hook 'set-read-only)

(provide 'init-dired)
