;; -*- lexical-binding: t; -*-
(install-pkgs flycheck-popup-tip)

(setq inhibit-splash-screen t
      make-backup-files nil
      scroll-conservatively 101
      frame-inhibit-implied-resize t
      text-scale-mode-step (expt 2 (/ 1.0 4.0))
      mode-line-position-column-line-format '(" (%l,%C)")
      compilation-scroll-output t
      recentf-max-menu-items 1000
      recentf-max-saved-items 1000
      recentf-auto-cleanup 'never
      display-line-numbers-type 'relative
      isearch-lazy-count t
      isearch-lazy-highlight t
      lazy-count-prefix-format nil
      lazy-count-suffix-format "[%s of %s]"
      ibuffer-expert t
      tags-case-fold-search nil
      tags-revert-without-query t
      flycheck-check-syntax-automatically '(save mode-enabled)
      flycheck-display-errors-delay 0.1
      whitespace-style '(face spaces trailing tabs
                              indentation space-mark tab-mark
                              missing-newline-at-eof)
      my-font (font-spec :family "JetBrains Mono"
                         :size 16
                         :weight 'normal
                         :slant 'normal))

;;;; NOTE https://github.com/tarsius/hl-todo/blob/f1fef158f99a70746926ef52c59f4863a29b7ed7/hl-todo.el#L105C1-L121C28
(setopt todowords-words '(("HOLD"   . "#d0bf8f")
                          ("TODO"   . "#229993")
                          ("NEXT"   . "#dca3a3")
                          ("PROG"   . "#7cb8bb")
                          ("DONT"   . "#5f7f5f")
                          ("FAIL"   . "#8c5353")
                          ("DONE"   . "#afd8af")
                          ("NOTE"   . "#d0bf8f")
                          ("MAYBE"  . "#d0bf8f")
                          ("HACK"   . "#d0bf8f")
                          ("TEMP"   . "#d0bf8f")
                          ("FIXME"  . "#cc9393")
                          ("XXXX*"  . "#cc9393")))

(setq-default create-lockfiles nil
              fill-column 80
              tab-width 8
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
(global-set-key (kbd "C-x C-b") 'ibuffer)


(set-face-attribute 'default nil :font my-font)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;(amx-mode 1)
(savehist-mode 1)
(recentf-mode 1)
(column-number-mode 1)
(whitespace-mode 1)
(show-paren-mode 1)
(which-function-mode 1)
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)

(global-nomouse-mode 1)
(global-todowords-mode 1)

(with-eval-after-load 'info
  (add-to-list 'Info-directory-list
               (expand-file-name "info" user-emacs-directory)))

(with-eval-after-load 'flycheck
  (flycheck-popup-tip-mode))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(install-pkgs tmr)
(provide 'init-base)
