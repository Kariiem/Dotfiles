;; -*- lexical-binding: t; -*-

(setq inhibit-splash-screen t
      make-backup-files nil
      scroll-conservatively 101
      frame-inhibit-implied-resize t
      text-scale-mode-step (expt 2 (/ 1.0 3.0))
      mode-line-position-column-line-format '(" (%l,%C)")
      compilation-scroll-output t
      recentf-max-menu-items 1000
      recentf-max-saved-items 1000
      recentf-auto-cleanup 'never
      whitespace-style '(face tabs spaces trailing
                              indentation space-mark tab-mark
                              missing-newline-at-eof)
      my-font (font-spec :family "JetBrains Mono"
                         :size 16
                         :weight 'normal
                         :slant 'normal))

(setq-default create-lockfiles nil
              fill-column 80
              tab-width 4
              indent-tabs-mode nil)

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


(set-face-attribute 'default nil :font my-font)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(amx-mode 1)
(savehist-mode 1)
(recentf-mode 1)
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(show-paren-mode 1)
(whitespace-mode 1)
(which-function-mode 1)


(provide 'init-base)
