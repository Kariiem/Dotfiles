;; -*- lexical-binding: t -*-

(install-pkgs multiple-cursors)

(setq kill-whole-line t
      mc/always-run-for-all t)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)


;; https://emacs.stackexchange.com/questions/2347/kill-or-copy-current-line-with-minimal-keystrokes
(defun slick-cut (beg end &rest _)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

(advice-add 'kill-region :before #'slick-cut)

;;

(defun slick-copy (beg end &rest _)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))

(advice-add 'copy-region-as-kill :before #'slick-copy)
(global-set-key [remap kill-ring-save] 'copy-region-as-kill)
;;

(defun dup-line ()
  (interactive)
  (let* ((current-point-col (- (point) (point-at-bol)))
         (current-line-text (let ((text (thing-at-point 'line t)))
                              (if text (string-chop-newline text) ""))))
    (move-end-of-line 1)
    (newline)
    (insert current-line-text)
    (move-beginning-of-line 1)
    (forward-char current-point-col)))


(global-set-key (kbd "C-,") 'dup-line)

(defface hidden-region-face
  '((((class color) (min-colors 88) (background dark))
     :background "slateblue4" :extend t)
    (((class color) (min-colors 88) (background light))
     :background "lightseagreen" :extend t)
    (((class color) (min-colors 16) (background dark))
     :background "lightblue" :extend t)
    (((class color) (min-colors 16) (background light))
     :background "light grey" :extend t)
    (((class color) (min-colors 8))
     :background "lightblue" :foreground "white" :extend t)
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray" :extend t))
  "marked-as-hidden face.")

(defvar hidden-region-text "{...}"
  "marked-as-hidden display text")

(defun hidden-region--toggle (o &optional hide-p)
  "Toggle overlay `O' to between open/close.
When HIDE-P is nil, open the overlay. When HIDE-P is t, close it."
  (if hide-p
      (progn
        (overlay-put o 'invisible t)
        (overlay-put o 'display hidden-region-text))
    (overlay-put o 'invisible nil)
    (overlay-put o 'display nil)))

(defun hidden-region--open (o)
  "Permanently open overlay O when isearch finds a match inside."
  (hidden-region--toggle o nil))

(defun hidden-region--create-overlay (beg end)
  (let ((o (make-overlay beg end nil t nil)))
    (overlay-put o 'type 'hide-region)
    (overlay-put o 'invisible t)
    (overlay-put o 'display hidden-region-text)
    (overlay-put o 'face 'hidden-region-face)
    (overlay-put o 'isearch-open-invisible           #'hidden-region--open)
    (overlay-put o 'isearch-open-invisible-temporary #'hidden-region--toggle)
    (overlay-put o 'keymap (define-keymap
                             "<tab>" (lambda () (interactive) (hidden-region--toggle o (not (overlay-get o 'invisible))))
                             "C-<tab>" (lambda () (interactive) (delete-overlay o))))
    o))

(defun hide-region (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (if (bolp) (1- (region-end)) (region-end)))
                 (list (point-min) (point-max))))
  (deactivate-mark)
  (hidden-region--create-overlay beg end)
  (goto-char beg))

(defun unhide-region (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (mapc (lambda (o) (when (eq (overlay-get o 'type) 'hide-region) (delete-overlay o)))
        (overlays-in beg end)))

(provide 'init-edit)
