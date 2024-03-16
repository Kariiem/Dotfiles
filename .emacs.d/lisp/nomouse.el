;;; nomouse.el --- disable mouse bindings -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Karim Taha <kariem.taha2.7@gmail.com>
;; created: January 2024
;; Keywords: convenience

;; This file is not part of GNU Emacs

;;; Commentary:

;; This package defines a minor mode for disabling mouse in several major modes.

;; Type `M-x customize-group RET nomouse RET' for user options.


;;; Code:

(defgroup nomouse nil
  "Mouse bindings settings"
  :group 'convenience)

(defcustom nomouse-major-modes '(prog-mode)
  "list of major modes where nomouse is active"
  :type '(repeat symbol))

(defun nomouse--all-bindings ()
  (let ((alist nil)
        (mouse-events '( mouse-1 mouse-2 mouse-3
                         down-mouse-1 down-mouse-2 down-mouse-3
                         drag-mouse-1 drag-mouse-2 drag-mouse-3
                         double-mouse-1 double-mouse-2 double-mouse-3
                         triple-mouse-1 triple-mouse-2 triple-mouse-3
                         C-mouse-1 C-mouse-2 C-mouse-3
                         C-down-mouse-1 C-down-mouse-2 C-down-mouse-3
                         C-drag-mouse-1 C-drag-mouse-2 C-drag-mouse-3
                         S-mouse-1 S-mouse-2 S-mouse-3
                         S-down-mouse-1 S-down-mouse-2 S-down-mouse-3
                         S-drag-mouse-1 S-drag-mouse-2 S-drag-mouse-3))
        (pos-events '( header-line tab-line mode-line
                       vertical-scroll-bar horizontal-scroll-bar
                       vertical-line
                       right-divider bottom-divider
                       left-edge right-edge top-edge bottom-edge
                       bottom-right-corner bottom-left-corner
                       top-right-corner top-left-corner)))
    (dolist (m mouse-events)
      (dolist (p pos-events)
        (push `([,p ,m] . ignore) alist))
      (push `([,m] . ignore) alist))
    alist))

;;;###autoload
(define-minor-mode nomouse-mode
  "Enable/Disable Mouse bindings in the current buffer."
  :global nil
  :lighter "nomouse"
  :keymap (nomouse--all-bindings))


(defun nomouse--turn-on ()
  (let ((major-mode major-mode))
    (when (or (null nomouse-major-modes)
              (member major-mode nomouse-major-modes)
              (apply 'derived-mode-p nomouse-major-modes))
      (nomouse-mode))))

;;;###autoload
(define-globalized-minor-mode global-nomouse-mode
  nomouse-mode nomouse--turn-on)

(provide 'no-mouse)
