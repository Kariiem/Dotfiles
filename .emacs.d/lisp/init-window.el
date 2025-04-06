;; -*- lexical-binding: t -*-

(defalias 'shrink-window-vertically 'shrink-window)
(defalias 'enlarge-window-vertically 'enlarge-window)
(define-prefix-command 'window-ctrl)

(defvar-keymap window-ctrl-map
  :prefix 'window-ctrl
  "c"       'delete-window
  "v"       'split-window-right
  "s"       'split-window-below
  "<up>"    'enlarge-window-vertically
  "<down>"  'shrink-window-vertically
  "<right>" 'enlarge-window-horizontally
  "<left>"  'shrink-window-horizontally)

(put 'enlarge-window-vertically   'repeat-map 'window-ctrl-map)
(put 'shrink-window-vertically    'repeat-map 'window-ctrl-map)
(put 'enlarge-window-horizontally 'repeat-map 'window-ctrl-map)
(put 'shrink-window-horizontally  'repeat-map 'window-ctrl-map)
(put 'delete-window               'repeat-map 'window-ctrl-map)

(setq windmove-wrap-around t)
(windmove-default-keybindings)

(defun toggle-window-freeze (&optional window interactive)
  (interactive "i\np")
  (setq window (window-normalize-window window))
  (set-window-dedicated-p window (not (window-dedicated-p window)))
  (with-current-buffer (window-buffer window)
    (setq window-size-fixed (and (window-dedicated-p window) 'width)))
  (when interactive
    (message "Window is %s frozen to buffer %s"
             (let ((status (window-dedicated-p window)))
               (cond
                ((null status) "no longer")
                (t "now")))
             (current-buffer))
    (force-mode-line-update)))

(global-set-key (kbd "C-c w") window-ctrl)
(global-set-key (kbd "C-x 9") 'toggle-window-freeze)

(provide 'init-window)
