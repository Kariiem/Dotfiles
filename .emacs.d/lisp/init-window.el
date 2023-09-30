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

(put 'enlarge-window-vertically  'repeat-map 'window-ctrl-map)
(put 'shrink-window-vertically 'repeat-map 'window-ctrl-map)
(put 'enlarge-window-horizontally 'repeat-map 'window-ctrl-map)
(put 'shrink-window-horizontally 'repeat-map 'window-ctrl-map)
(put 'delete-window 'repeat-map 'window-ctrl-map)

(global-set-key (kbd "C-c w") window-ctrl)
(setq windmove-wrap-around t)
(windmove-default-keybindings)
(which-function-mode 1)

(provide 'init-window)
