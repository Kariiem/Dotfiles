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
          (lambda ()
            (report-time-since-load " [after-init]")
            (condition-case nil (call-interactively 'load-theme)
              (error nil)
              ('quit nil)))
          t)


;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024)) ;; 20 mb
      (init-gc-cons-threshold most-positive-fixnum))
  (setq gc-cons-threshold init-gc-cons-threshold
        gc-cons-percentage 0.6)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold
                             gc-cons-percentage 0.1))))

;; Bootstrap config
(let ((dirs '("lisp" "minor-modes" "site")))
  (dolist (d dirs)
    (add-to-list 'load-path (expand-file-name d user-emacs-directory))))

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

(defun generate&load-directory-autoloads (dir &optional autoload-file-name force)
  (let ((filepath (expand-file-name autoload-file-name dir))
        (m))
    (if (or (not (file-exists-p filepath)) force)
        (progn (loaddefs-generate dir filepath)
               (setq m `("`%s' autoload-file generated from `%s'"
                                ,filepath ,dir)))
      (setq m `("`%s' autoload-file already exists" ,filepath)))
    (load filepath nil t)
    (apply #'message m)))

(generate&load-directory-autoloads (expand-file-name "minor-modes"
                                                     user-emacs-directory)
                                   "autoload-minor-modes.el" t)

;;;; Base
(load (expand-file-name "enable-commands.el" user-emacs-directory) nil t)
(require 'init-package)
(require 'init-base)
(require 'init-theme)
(require 'init-misc)
(require 'init-window)
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
(require 'init-ement)
(require 'init-matrix)
(require 'init-eww)
(require 'init-zen)
(require 'init-hledger)

;;;; Languages
;; General settings
(with-eval-after-load 'prog-mode
  (electric-pair-mode 1))
(require 'init-nix)
(require 'init-org)
(require 'init-md)
(require 'init-gl)
(require 'init-asm)
(require 'init-c)
(require 'init-rust)
(require 'init-coq)
(require 'init-haskell)
(require 'init-ocaml)
(require 'init-sml)
(require 'init-fsharp)
(require 'init-kotlin)
(require 'init-scheme)
(require 'init-lisp)
(require 'init-python)
(require 'init-ruby)
(require 'init-lua)
(require 'init-raku)
(require 'init-go)
(require 'init-typescript)
(require 'init-julia)
(require 'init-z3)
(require 'init-elm)
;;;; Build tools
(require 'init-build)
;; init.el ends here
