;;; init.el --- Load the full configuration -*- lexical-binding: t -*-




;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;;(require 'init-benchmarking) ;; Measure startup time

;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Bootstrap config
(setq custom-file (locate-user-emacs-file "custom.el"))

;;;; Base
(require 'init-package)
(require 'init-theme)
(require 'init-misc)
(require 'init-window)
(require 'init-display)
(require 'init-minibuffer)
(require 'init-edit)
(require 'init-dired)
(require 'init-completions)
;;(require 'init-evil)
(require 'init-keychords)

;;;; Tools
(require 'init-magit)
(require 'init-tramp)
(require 'init-rg)
(require 'init-dbg)
(require 'init-email)

;;;; Languages
(require 'init-org)
(require 'init-md)
(require 'init-c)
(require 'init-coq)
(require 'init-gl)
(require 'init-scheme)
(require 'init-rust)
(require 'init-lisp)
(require 'init-python)

;; init.el ends here
