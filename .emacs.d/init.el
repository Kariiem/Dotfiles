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

(defvar require-stats '())
(defun timed-require (feature &optional filename noerror)
  (let* ((tic (current-time))
         (f   (require feature filename noerror))
         (toc (current-time)))
    (push (cons f (float-time (time-subtract toc tic)))
          require-stats)))


(defun report-time-since-load (&optional suffix)
  (message "Loading init...done (%.3fs)%s with %d garbage collections"
           (float-time (time-subtract (current-time) emacs-start-time))
           suffix
           gcs-done))

(add-hook 'after-init-hook
          (lambda ()
            (let ((inhibit-message t))
              (dolist (item (sort require-stats :key 'cdr))
                (message "require %-20s took %.3fs" (car item) (cdr item)))
              (report-time-since-load " [after-init]")))
          t)

(fringe-mode 0)

(add-hook 'whitespace-mode-hook
          (lambda () (set-face-attribute 'whitespace-space nil
                                         :foreground "LavenderBlush4"
                                         :background nil)))

(defun light-emacs ()
  (interactive)
  (set-face-attribute 'default nil
                      :foreground "black"
                      :background "white"))

(defun dark-emacs ()
  (interactive)
  (set-face-attribute 'default nil
                      :foreground "#f8f8f2"
                      :background "#0a2a2f"))

(dark-emacs)

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 128 1024 1024)) ;; 128 mb
      (init-gc-cons-threshold most-positive-fixnum))
  (setq gc-cons-threshold init-gc-cons-threshold
        gc-cons-percentage 0.8)
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

(timed-require 'init-package)
(timed-require 'init-base)
(timed-require 'init-theme)
(timed-require 'init-misc)
(timed-require 'init-window)
(timed-require 'init-edit)
(timed-require 'init-dired)
(timed-require 'init-completions)
(timed-require 'init-compile)

;;(require 'init-evil)
;;(require 'init-keychords)

;;;; Tools
(timed-require 'init-magit)
(timed-require 'init-tramp)
(timed-require 'init-rg)
(timed-require 'init-dbg)
(timed-require 'init-email)
(timed-require 'init-pdf)
(timed-require 'init-epub)
(timed-require 'init-compile)
(timed-require 'init-irc)
(timed-require 'init-ement)
(timed-require 'init-matrix)
(timed-require 'init-eww)
(timed-require 'init-zen)
(timed-require 'init-hledger)
(timed-require 'init-translate)
(timed-require 'init-eglot)
;;;; Languages
;; General settings
(with-eval-after-load 'prog-mode
  (electric-pair-mode 1))
(timed-require 'init-nix)
(timed-require 'init-org)
(timed-require 'init-md)
(timed-require 'init-gl)
(timed-require 'init-asm)
(timed-require 'init-c)
(timed-require 'init-rust)
(timed-require 'init-coq)
(timed-require 'init-haskell)
(timed-require 'init-ocaml)
(timed-require 'init-sml)
(timed-require 'init-fsharp)
(timed-require 'init-kotlin)
(timed-require 'init-scheme)
(timed-require 'init-lisp)
(timed-require 'init-python)
(timed-require 'init-ruby)
(timed-require 'init-lua)
(timed-require 'init-raku)
(timed-require 'init-go)
(timed-require 'init-typescript)
(timed-require 'init-julia)
(timed-require 'init-z3)
(timed-require 'init-elm)
(timed-require 'init-elixir)
(timed-require 'init-dart)
(timed-require 'init-lean)
(timed-require 'init-prolog)
;;;; Build tools
(timed-require 'init-build)
(timed-require 'init-tex)
;;;; Database
(timed-require 'init-postgres)
;;;; WebDev
(timed-require 'init-rest)
;; init.el ends here
