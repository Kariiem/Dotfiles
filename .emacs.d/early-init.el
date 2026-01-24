;;; early-init.el --- Emacs 27+ pre-initialisation config -*- lexical-binding: t -*-

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

(setq package-enable-at-startup nil)
(setq source-directory "~/software/emacs/src")


;; see (elisp) Startup Summary
(when (getenv "WSL_DISTRO_NAME")
  (push '(fullscreen . maximized) default-frame-alist)
  (push '(undecorated . t)        default-frame-alist))
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)


(push '(foreground-color . "#f8f8f2") default-frame-alist)
(push '(background-color . "#292C31") default-frame-alist)

;; (set-face-attribute 'default nil
;;                     :foreground "#f8f8f2"
;;                     :background "#292C31")

(let ((normal-gc-cons-threshold (* 128 1024 1024)) ;; 128 mb
      (init-gc-cons-threshold most-positive-fixnum))
  (setq gc-cons-threshold init-gc-cons-threshold
        gc-cons-percentage 0.8)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold
                             gc-cons-percentage 0.1))))

(provide 'early-init)
;;; early-init.el ends here
