;; -*- lexical-binding: t -*-

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(column-number-mode 1)
;; use one-based column counting
(setq mode-line-position-column-line-format '(" (%l,%C)"))
;;(global-whitespace-mode t)
(global-display-line-numbers-mode 1)

(setq my-font
      (font-spec :family "JetBrains Mono" :size 16 :weight 'normal :slant 'normal))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-face-attribute 'default nil :font my-font)
            (scroll-bar-mode -1)))
(set-face-attribute 'default nil :font my-font)

(setq default-frame-alist nil)
(setq-default whitespace-display-mappings '((space-mark 32 [183] [46])
                                            (space-mark 160 [164] [95])
                                            (tab-mark 9 [187 9] [92 9])))

(setq-default inhibit-splash-screen t
              make-backup-files nil
              create-lockfiles nil
              fill-column 80
              tab-width 4
              scroll-conservatively 101
              indent-tabs-mode nil
              compilation-scroll-output t
              whitespace-style
              '(face empty spaces tabs newline trailing space-mark tab-mark)
              )
(add-hook 'after-init-hook 'visual-line-mode)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(defun toggle-dedicate-window ()
  (interactive)
  (if (not (window-dedicated-p (selected-window)))
      (progn (set-window-dedicated-p (selected-window) t)
             (message "window dedicated."))
    (set-window-dedicated-p (selected-window) nil)
    (message "window un-dedicated")))
(global-set-key (kbd "C-x 9") 'toggle-dedicate-window)


(provide 'init-display)
