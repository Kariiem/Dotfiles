;;; todowords.el --- highlight todowords -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Karim Taha <kariem.taha2.7@gmail.com>
;; created: January 2024
;; Keywords: convenience

;; This file is not part of GNU Emacs

;;; Commentary:

;; This package defines a minor mode for highlighting todo keywords.

;; Type `M-x customize-group RET nomouse RET' for user options.


(require 'derived)

(defgroup todowords  nil
  "Highlighting todo keywords."
  :group 'convenience)

(defcustom todowords-words '(("TODO" . "red")
                             ("NOTE" . "blue")
                             ("FIXME" . "orange"))
  "alist of todowords and corresponding color"
  :type '(repeat (cons string string))
  :set (lambda (symbol value)
         (set-default-toplevel-value symbol value)
         (dolist (buf (buffer-list))
	   (with-current-buffer buf
             (when (and (bound-and-true-p todowords-mode)
                        (boundp 'todowords--new-keywords))
               (todowords-mode -1)
               (setq todowords--new-keywords nil)
               (todowords-mode 1))))))

(defcustom todowords-major-modes '(prog-mode)
  "major modes where highlighting is active"
  :type '(repeat symbol))

(defvar-local todowords--new-keywords nil)

(defun todowords--generate-faces ()
  (mapcar (lambda (word)
            (let* ((w (car word))
                   (c (cdr word))
                   (f `((t :foreground ,c
                           :underline t
                           :weight ultra-bold))))
              f))
          todowords-words))

(defun todowords--generate-regexps ()
  (mapcar (lambda (word)
            (let* ((w (car word)))
              (format "\\<\\(%s\\)" w)))
            todowords-words))

(defun todowords--generate-pairs ()
  (let* ((faces (todowords--generate-faces))
         (regexps (todowords--generate-regexps))
         (pairs (cl-mapcar (lambda (f r) (list r 1 `',f t))
                           faces
                           regexps)))
    (setq todowords--new-keywords pairs)))

(defsubst todowords--new-keywords ()
  (or todowords--new-keywords (todowords--generate-pairs)))

;;;###autoload
(define-minor-mode todowords-mode
  "Enable/Disable Mouse bindings in the current buffer."
  :lighter "todowords"
  (if todowords-mode
      (font-lock-add-keywords nil (todowords--new-keywords))
    (font-lock-remove-keywords nil (todowords--new-keywords)))
  (when font-lock-mode
    ;;;; NOTE https://github.com/tarsius/hl-todo/blob/f1fef158f99a70746926ef52c59f4863a29b7ed7/hl-todo.el#L290
    (jit-lock-mode 1)))

(defun todowords--turn-on ()
  (when (apply 'derived-mode-p todowords-major-modes)
    (todowords-mode 1)))

;;;###autoload
(define-global-minor-mode global-todowords-mode
  todowords-mode todowords--turn-on)
