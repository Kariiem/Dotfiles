;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Karim"
      user-mail-address "kariem.taha2.7@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;; global
(+global-word-wrap-mode)
;;; evil
(setq evil-cross-lines t)

;;; Projectile
(setq projectile-project-search-path '("~/FECU" "~/Workspaces" "~/repos" "~/.config"))

;;; treemacs
(setq treemacs-indent-guide-style 'line
      treemacs-show-hidden-files nil)
(require 'treemacs)
(treemacs-load-theme "doom-colors")
(with-eval-after-load 'doom-themes
  (doom-themes-treemacs-config))

;;; erc
(setq erc-prompt (lambda () (concat "[" (buffer-name) "]"))
      erc-server "irc.libera.chat"
      erc-autojoin-channels-alist '(("irc.libera.chat" "#haskell-in-depth" )
                                    ("irc.oftc.net" "#qemu-gsoc"))
      erc-nick "Kimo"
      erc-user-full-name "Karim Taha"
      erc-auto-query 'bury
      erc-kill-buffer-on-part t
      erc-fill-column 100
      erc-fill-function 'erc-fill-static
      erc-fill-static-center 20
      erc-log-channels-directory "~/.erc/logs/"
      erc-save-buffer-on-part t
      )

;;;;;;; lang ;;;;;;;;
;;; haskell
(setq haskell-stylish-on-save t)

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'org-babel-tangle )
            ))

;;; org
(add-hook! org-mode 'rainbow-mode)
(add-hook! prog-mode 'rainbow-mode)


(add-hook! 'org-after-todo-statistics-hook #'org-summary-todo)

;;; ess
(after! lsp-ui
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-delay 2)
  (setq lsp-eldoc-enable-hover t))
(defun my-inferior-ess-init ()
      (setq-local ansi-color-for-comint-mode 'filter)
      (smartparens-mode 1))
    (add-hook 'inferior-ess-mode-hook 'my-inferior-ess-init)
