;; -*- lexical-binding: t; -*-

(setq recentf-max-menu-items 1000
      recentf-max-saved-items 1000
      recentf-auto-cleanup 'never)

(defun recentf-fix-category (prop)
  (unless (completion-metadata-get vertico--metadata prop)
    (if (eq 'category prop)
        'file
      nil)))

(global-set-key (kbd "C-x C-r")
                (lambda ()
                  (interactive)
                  (unwind-protect
                      (progn (advice-add
                              #'vertico--metadata-get :override 'recentf-fix-category)
                             (call-interactively 'recentf))
                    (advice-remove #'vertico--metadata-get 'recentf-fix-category))))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(column-number-mode 1)

(amx-mode 1)
(recentf-mode 1)
(savehist-mode 1)
(show-paren-mode 1)

(provide 'init-base)
