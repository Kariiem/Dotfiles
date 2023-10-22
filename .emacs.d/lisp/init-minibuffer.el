;; -*- lexical-binding: t -*-

(require-package 'amx)
(amx-mode 1)
(setq recentf-max-menu-items 1000
      recentf-max-saved-items 1000
      recentf-auto-cleanup 'never)

(recentf-mode 1)

(defun recentf-fix-category (prop)
  (unless (completion-metadata-get vertico--metadata prop)
    (if (eq 'category prop)
        'file
      nil)))


(global-set-key (kbd "C-x C-r")
                (lambda ()
                  (interactive)
                  (unwind-protect
                      (progn (advice-add #'vertico--metadata-get :override 'recentf-fix-category)
                             (call-interactively 'recentf))
                    (advice-remove #'vertico--metadata-get 'recentf-fix-category))))

(setq compilation-ask-about-save nil
      compilation-always-kill t
      compilation-scroll-output 'first-error)

(provide 'init-minibuffer)