(defalias 'shrink-window-vertically 'shrink-window)
(defalias 'enlarge-window-vertically 'enlarge-window)
(define-prefix-command 'window-ctrl)

(defvar-keymap window-ctrl-map
  :prefix 'window-ctrl
  "v" 'split-window-right
  "s" 'split-window-below
  "<up>" (repeat-command! enlarge-window-vertically)
  "<down>" (repeat-command! shrink-window-vertically)
  "<right>" (repeat-command! enlarge-window-horizontally)
  "<left>" (repeat-command! shrink-window-horizontally))

(global-set-key (kbd "C-x w") 'window-ctrl)
(provide 'init-window)
