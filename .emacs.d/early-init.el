;;; early-init.el --- Emacs 27+ pre-initialisation config -*- lexical-binding: t -*-

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

;; see (elisp) Startup Summary, 7th element
(setq package-enable-at-startup nil)

;; see (elisp) Startup Summary, 10th element
;; (push '(alpha-background     . 90)        default-frame-alist) ;; see back-alpha-mode
(push '(foreground-color     . "#f8f8f2") default-frame-alist)
(push '(background-color     . "#292C31") default-frame-alist)
(push '(left-fringe          . 8)         default-frame-alist)
(push '(right-fringe         . 0)         default-frame-alist)
(push '(undecorated          . t)         default-frame-alist)
(push '(menu-bar-lines       . 0)         default-frame-alist)
(push '(tool-bar-lines       . 0)         default-frame-alist)
(push '(vertical-scroll-bars )            default-frame-alist)
(push '(horizontal-scroll-bars)           default-frame-alist)
(push '(visibility)                       default-frame-alist) ;; to even prevent the split second when the frame is not ready
(push '(fullscreen           . maximized) default-frame-alist)

(defun show-frame (frame)
  (make-frame-visible frame)
  (remove-hook 'after-make-frame-functions #'show-frame))

(add-hook 'after-make-frame-functions #'show-frame)

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)

(defun gc-restore-after-startup ()
  (setq gc-cons-threshold  (* 64 1024 1024))
  (setq gc-cons-percentage 0.5))

(add-hook 'emacs-startup-hook 'gc-restore-after-startup)

;; https://raw.githubusercontent.com/jamescherti/minimal-emacs.d/refs/heads/main/early-init.el
(if (and (featurep 'native-compile)
         (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (setq package-native-compile t)
  (setq features (delq 'native-compile features)))

(setq jka-compr-verbose nil)
(setq read-process-output-max (* 2 1024 1024))
(setq process-adaptive-read-buffering nil)
(setq inhibit-compacting-font-caches t)

(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

(setq frame-resize-pixelwise t
      window-resize-pixelwise t
      frame-inhibit-implied-resize t
      auto-mode-case-fold nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      initial-buffer-choice nil
      inhibit-startup-buffer-menu t
      inhibit-x-resources t
      ;; initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      inhibit-splash-screen t
      use-dialog-box nil)

(message "early-init took %.3fs | GC: %d times"
         (float-time (time-subtract (current-time) before-init-time))
         gcs-done)

(provide 'early-init)
;;; early-init.el ends here
