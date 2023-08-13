(defalias 'shrink-window-vertically 'shrink-window)
(defalias 'enlarge-window-vertically 'enlarge-window)

(define-minor-mode window-ctrl "" :global t)
(defvar window-ctrl-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") (repeat-command! delete-window))
    (define-key map (kbd "v") 'split-window-right)
    (define-key map (kbd "s") 'split-window-below)
    (define-key map (kbd "<up>") (repeat-command! enlarge-window-vertically))
    (define-key map (kbd "<down>") (repeat-command! shrink-window-vertically))
    (define-key map (kbd "<right>") (repeat-command! enlarge-window-horizontally))
    (define-key map (kbd "<left>") (repeat-command! shrink-window-horizontally))
    map)
  "")
(put 'enlarge-window-vertically 'repeat-map 'window-ctrl-map)
(put 'shrink-window-vertically 'repeat-map 'window-ctrl-map)
(put 'enlarge-window-horizontally 'repeat-map 'window-ctrl-map)
(put 'shrink-window-horizontally 'repeat-map 'window-ctrl-map)
(put 'delete-window 'repeat-map 'window-ctrl-map)

(global-set-key (kbd "C-c w") window-ctrl-map)
(window-ctrl 1)

(provide 'init-window)
