;; -*- lexical-binding: t -*-

(defgroup back-alpha nil
  "global minor mode for controlling background opacity."
  :group 'convenience)

(defcustom back-alpha-step 1
  "back-alpha mode default step")

(defcustom back-alpha-base 95
  "back-alpha mode default opacity")

(defcustom back-alpha-persist nil
  "back-alpha mode persist between toggling")

(defvar-keymap back-alpha-mode-map
  :repeat t
  "=" #'back-alpha-increase
  "+" #'back-alpha-increase
  "-" #'back-alpha-decrease)

(defvar-keymap back-alpha-prefix-map
  "C-c a" back-alpha-mode-map)

(defun back-alpha-increase (frames &optional future-persist)
  (interactive
   (list (if current-prefix-arg (frame-list) (list (selected-frame))))
   back-alpha-mode)
  (mapc (lambda (f)
          (let* ((current (or (frame-parameter f 'alpha-background) 100))
                 (new     (min 100 (+ current back-alpha-step))))
            (set-frame-parameter f 'alpha-background new)))
        frames)
  (when future-persist
    (setf (alist-get 'alpha-background default-frame-alist)
          (frame-parameter (selected-frame) 'alpha-background))))

(defun back-alpha-decrease (frames &optional future-persist)
  (interactive
   (list (if current-prefix-arg (frame-list) (list (selected-frame))))
   back-alpha-mode)
  (mapc (lambda (f)
          (let* ((current (or (frame-parameter f 'alpha-background) 100))
                 (new     (max 0 (-  current back-alpha-step))))
            (set-frame-parameter f 'alpha-background new)))
        frames)
  (when future-persist
    (setf (alist-get 'alpha-background default-frame-alist)
          (frame-parameter (selected-frame) 'alpha-background))))

(define-minor-mode back-alpha-mode
  "Global minor mode for controlling frame background opacity"
  :global t
  :keymap back-alpha-prefix-map
  :lighter nil
  (if back-alpha-mode
      (set-frame-parameter nil 'alpha-background back-alpha-base)
    (when back-alpha-persist
      (setq back-alpha-base (frame-parameter nil 'alpha-background)))
    (set-frame-parameter nil 'alpha-background nil)))

(provide 'init-frame)
