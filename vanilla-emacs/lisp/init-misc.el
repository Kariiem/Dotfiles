(require 'repeat)

;; source: https://www.reddit.com/r/emacs/comments/vs4jqm/it_bears_repeating_emacs_28_repeat_mode/
(defmacro repeat-command! (command)
  "Repeat COMMAND."
  `(lambda ()
     (interactive)
     (let ((repeat-previous-repeated-command  ',command)
           (repeat-message-function           #'ignore)
           (last-repeatable-command           'repeat))
       (repeat nil))))

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key [remap kill-buffer] 'kill-current-buffer)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(provide 'init-misc)
