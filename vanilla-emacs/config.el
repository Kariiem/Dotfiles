;;; -*- lexical-binding: t -*-

(let ((default-gc-threshold gc-cons-threshold)
      (default-gc-percentage gc-cons-percentage))
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.8)
  (add-hook 'after-init-hook
            (lambda ()
              (setq gc-cons-percentage default-gc-percentage
                    gc-cons-threshold default-gc-threshold))))


(defmacro install! (package)
  "Check if a package is installed, if not, then install it.
    A macro that expands to :
    \t(unless (package-installed ARG) (package-install ARG))"
  `(unless (package-installed-p ',package)
     (unless (memq ',package package-archive-contents)
       (package-refresh-contents))
     (package-install ',package)))


(defun my-read-file (filename)
  "Reads FILENAME content into a string"
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun my-load-theme ()
  (let* (
	 (theme-xname (my-read-file "~/.local/theme.txt"))
	 (theme-doom-name (cond ((string-equal theme-xname "NordLight") "doom-nord-light")
				((string-equal theme-xname "Dracula")   "doom-dracula")
				((string-equal theme-xname "DoomOne")   "doom-one")
				((string-equal theme-xname "Nord")      "doom-nord")
				(t "doome-one")))
	 (theme (intern theme-doom-name)))
    (message "%s" theme)
    (load-theme theme t)))

(defun icomp/fido-choose-from-recentf ()
  "List recent files"
  (interactive)
  (find-file (completing-read "Open file: " recentf-list nil t)))

(add-to-list 'load-path "~/vanilla-emacs/lisp/")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(eval-when-compile
  (package-initialize))

(setq scroll-step 1
      scroll-margin 3
      scroll-conservatively 101
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      inhibit-startup-screen t
      completion-auto-help nil)
(progn
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (indent-tabs-mode -1)
  (global-display-line-numbers-mode 1)
  (line-number-mode 1)
  (column-number-mode 1))

(set-face-attribute 'default nil :family "Hack" :height 100)

(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)

(my-load-theme)
(doom-themes-org-config)

(install! setup)
(install! evil)
(install! ivy)
(install! avy)
(install! vertico)
(install! marginalia)
(install! gruber-darker-theme)
(install! doom-themes)
(install! company)
(install! yasnippet-snippets)
(install! yasnippet)

(install! evil-org)
(install! org-superstar)
(install! toc-org)

(install! zig-mode)
(install! haskell-mode)
(install! sml-mode)
(install! fsharp-mode)

(savehist-mode 1)
(recentf-mode 1)
(setq server-client-instructions nil
      initial-scratch-message ""
      backup-directory-alist '(("." . "~/.emacs_saves")))

(setq company-idle-delay 0)
(global-company-mode 1)
(fido-vertical-mode 1)
(marginalia-mode 1)
(yas-global-mode 1)

(setq evil-cross-lines t)
(evil-mode 1)
(evil-set-undo-system 'undo-redo)

;;source: https://github.com/noctuid/evil-guide#leader-key

(define-prefix-command 'my-leader-map )
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "SPC") my-leader-map)
  (define-key my-leader-map (kbd "qq") 'save-buffers-kill-emacs)
  (define-key my-leader-map (kbd "bn") 'next-buffer)
  (define-key my-leader-map (kbd "bp") 'previous-buffer)
  (define-key my-leader-map (kbd "bl") 'switch-to-buffer)
  (define-key my-leader-map (kbd "bk") 'kill-this-buffer)
  (define-key my-leader-map (kbd "w") evil-window-map)
  (define-key my-leader-map (kbd "ff") 'find-file)
  (define-key my-leader-map (kbd "fr") 'icomp/fido-choose-from-recentf))

(setq org-edit-src-content-indentation 0
      org-ellipsis"⮷")
(defun org-minor-modes ()
  (toc-org-mode 1)
  (org-superstar-mode 1)
  (org-superstar-configure-like-org-bullets)
  (evil-org-mode 1))
(with-eval-after-load 'org
  (add-hook 'org-mode-hook 'org-minor-modes)
  (add-hook 'markdown-mode-hook 'toc-org-mode)
  (require 'org-tempo)
  (require 'evil-org)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(setq c-default-style "linux"
      c-basic-offset 4)
(custom-set-variables '(zig-format-on-save nil))
(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(setq haskell-tags-on-save t)
