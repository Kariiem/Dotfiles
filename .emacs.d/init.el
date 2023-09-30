;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;;; This template is taken from Steve Purcell's config
;; src: https://github.com/purcell/emacs.d/blob/master/init.el


;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

;; Measure startup time
;;(require 'init-benchmarking)
;; src: https://github.com/jwiegley/dot-emacs/blob/master/init.org#report-time-spent-loading-this-module
(defconst emacs-start-time (current-time))

(defun report-time-since-load (&optional suffix)
  (message "Loading init...done (%.3fs)%s"
           (float-time (time-subtract (current-time) emacs-start-time))
           suffix))

(add-hook 'after-init-hook
          (lambda () (report-time-since-load " [after-init]"))
          t)

;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Bootstrap config
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site" user-emacs-directory))
(setq custom-file (locate-user-emacs-file "custom.el"))
;; src: https://www.emacswiki.org/emacs/DisabledCommands
(setq enable-commands-file (locate-user-emacs-file "enable-commands.el"))

(defun en/disable-commands-in-separate-file (orig-fun &rest orig-args)
  "Put declarations in `custom-file'."
  (let ((user-init-file enable-commands-file))
    (apply orig-fun orig-args)
    (with-current-buffer (find-file-noselect (substitute-in-file-name user-init-file))
      (goto-char (point-min))
      (if (search-forward "(provide " nil t)
	      (delete-region
	       (progn (beginning-of-line) (point))
	       (progn (forward-line 1) (point))))

      (goto-char (point-max))
      (unless (bolp) (newline))
      (insert "(provide '" (file-name-base enable-commands-file) ")\n")
      (save-buffer))))

(advice-add 'en/disable-command :around #'en/disable-commands-in-separate-file)

;;;; Base
(load (expand-file-name "enable-commands.el" user-emacs-directory) nil t)
(require 'init-package)
(require 'init-theme)
(require 'init-misc)
(require 'init-window)
(require 'init-display)
(require 'init-minibuffer)
(require 'init-edit)
(require 'init-dired)
(require 'init-completions)
(require 'init-compile)
;;(require 'init-evil)
;;(require 'init-keychords)

;;;; Tools
(require 'init-magit)
(require 'init-tramp)
(require 'init-rg)
(require 'init-dbg)
(require 'init-email)
(require 'init-pdf)
(require 'init-compile)
(require 'init-irc)
;;;; Text
(require 'init-zen)

;;;; Languages
(require 'init-org)
(require 'init-md)
(require 'init-c)
(require 'init-coq)
(require 'init-haskell)
(require 'init-ocaml)
(require 'init-sml)
(require 'init-gl)
(require 'init-scheme)
(require 'init-rust)
(require 'init-lisp)
(require 'init-python)
(require 'init-asm)
;; init.el ends here
