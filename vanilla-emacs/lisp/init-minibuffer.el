(require-package 'amx)
(amx-mode 1)
(setq recentf-max-menu-items 1000)
(setq recentf-max-saved-items 1000)
(recentf-mode 1)
(run-at-time nil (* 5 60) 'recentf-save-list)

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

(provide 'init-minibuffer)
