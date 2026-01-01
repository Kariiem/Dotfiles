;; -*- lexical-binding: t; -*-
(install-pkgs flycheck-popup-tip
              kkp
              tmr
              clipetty)

(when (not (display-graphic-p))
  (defun uncoloured-emacs ()
    (interactive)
    (face-spec-set 'default nil 'reset)
    (set-face-attribute 'default nil :foreground nil :background nil)
    (set-face-attribute 'default t :foreground nil :background nil)
    (set-face-attribute 'default (selected-frame) :foreground nil :background nil)
    (mapc #'disable-theme custom-enabled-themes))

  (xterm-mouse-mode)
  (global-kkp-mode)
  (global-clipetty-mode)
  (setq mouse-wheel-progressive-speed nil
        xterm-window-title-flag t))

(setq ring-bell-function 'ignore
      visible-bell nil
      inhibit-splash-screen t
      make-backup-files nil
      backup-inhibited t
      backup-by-copying-when-linked t
      scroll-conservatively 101
      frame-inhibit-implied-resize t
      text-scale-mode-step (expt 2 (/ 1.0 4.0))
      mode-line-position-column-line-format '(" (%l,%C)")
      compilation-scroll-output t
      recentf-max-menu-items 1000
      recentf-max-saved-items 1000
      recentf-auto-cleanup 'never
      display-line-numbers-type 'relative
      imenu-max-item-length nil
      isearch-lazy-count t
      isearch-lazy-highlight t
      lazy-count-prefix-format nil
      lazy-count-suffix-format "[%s of %s]"
      ibuffer-expert t
      tags-case-fold-search nil
      tags-revert-without-query t
      flycheck-highlighting-mode 'lines
      flycheck-highlighting-style '(conditional 4 level-face (delimiters #1="" #1#))
      flycheck-check-syntax-automatically '(save mode-enabled)
      flycheck-indication-mode 'left-margin
      flycheck-display-errors-delay 0.1
      gdb-many-windows t
      gdb-default-window-configuration-file "gdbui"
      desktop-save t
      project-vc-extra-root-markers '(".project" ".projectile" )
      speedbar-show-unknown-files t
      speedbar-default-position 'left
      dired-mouse-drag-files t
      tab-bar-auto-width nil
      ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally
      browse-url-browser-function 'browse-url-generic
      browse-url-generic-program  (or (and (getenv "WSL_DISTRO_NAME")
                                           "/mnt/c/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")
                                      "chromium")
      whitespace-style '(face spaces trailing tabs
                              indentation space-mark tab-mark
                              missing-newline-at-eof)
      my-font (font-spec :family "JetBrains Mono"
                         :size 16
                         ;;:width 'normal
                         :weight 'normal
                         :slant 'normal))

(setq select-active-regions nil
      select-enable-clipboard t
      select-enable-primary nil)
(set-display-table-slot standard-display-table 'truncation ?»)
;;;; NOTE https://github.com/tarsius/hl-todo/blob/f1fef158f99a70746926ef52c59f4863a29b7ed7/hl-todo.el#L105C1-L121C28
(setopt todowords-words '(("HOLD"   . "#d0bf8f")
                          ("TODO"   . "#229993")
                          ("HINT"   . "#8845F3")
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
              display-fill-column-indicator-character #x2551
              indent-tabs-mode nil
              case-fold-search nil)

(defun recentf-fix-category (prop)
  (unless (completion-metadata-get vertico--metadata prop)
    (if (eq 'category prop)
        'file
      nil)))

(defun insert-date ()
  (interactive)
  (insert (current-time-string)))

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
(set-face-attribute 'fill-column-indicator nil :foreground "dim grey")
;;(set-frame-font my-font nil t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(delete-selection-mode 1)
(global-auto-revert-mode 1)

(pixel-scroll-precision-mode 1)
;;(amx-mode 1)
(savehist-mode 1)
(etags-regen-mode)
;;(recentf-mode 1)
(column-number-mode 1)
(whitespace-mode 1)
(show-paren-mode 1)
(which-function-mode 1)
;;(desktop-save-mode 1)
(global-display-line-numbers-mode 1)
;; (global-display-fill-column-indicator-mode 1)
(global-hl-line-mode 1)

(global-nomouse-mode -1)
(global-todowords-mode 1)

(with-eval-after-load 'info
  (add-to-list 'Info-directory-list
               (expand-file-name "info" user-emacs-directory)))

(with-eval-after-load 'flycheck
  (flycheck-popup-tip-mode))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'flycheck-mode-hook #'flycheck-set-indication-mode)
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window display-buffer-in-side-window)
               (side            . bottom)
               (reusable-frames . visible)
               (window-height   . 0.33)))


(defun split-window-1/n (&optional size window-to-split)
  (interactive `(,(when current-prefix-arg
                    (prefix-numeric-value current-prefix-arg))
                 ,(selected-window)))
  (let (new-window)
    (when (and size (< size 0) (< (- size) window-min-width))
      ;; `split-window' would not signal an error here.
      (error "Size of new window too small"))
    (setq new-window (split-window window-to-split
                                   (and size (- (window-width) (/ (window-width) size)))
                                   t))
    ;; Always copy quit-restore parameter in interactive use.
    (let ((quit-restore (window-parameter window-to-split 'quit-restore)))
      (when quit-restore
	(set-window-parameter new-window 'quit-restore quit-restore)))
    new-window))

(global-set-key (kbd "C-x 3") #'split-window-1/n)

;; so that I can copy the isearch highlighted text
(keymap-set isearch-mode-map "M-w" (lambda ()
                                     (interactive)
                                     (let ((search-string (if isearch-regexp
                                                              isearch-regexp
                                                            isearch-string)))
                                       (kill-new search-string)
                                       (message "Copied: %s" search-string))))


(add-to-list 'display-buffer-alist
             '((major-mode . pdf-outline-buffer-mode)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side . right)
               (window-width . 0.25)))

(add-to-list 'display-buffer-alist
             '((major-mode . help-mode)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side . bottom)
               (window-height . 0.25)))


(defun my/project-try-local (dir)
  "Check if DIR contains a .project file."
  (let ((root (locate-dominating-file dir ".project")))
    (and root (cons 'transient root))))

(add-hook 'project-find-functions #'my/project-try-local)
(setq project-find-functions
      (cons #'my/project-try-local project-find-functions))
(provide 'init-base)
