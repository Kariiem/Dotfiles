;; -*- lexical-binding: t -*-

;; source: https://www.reddit.com/r/emacs/comments/vs4jqm/it_bears_repeating_emacs_28_repeat_mode/
;; https://www.emacswiki.org/emacs/Repeatable
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

(setq Man-notify-method 'pushy
      Man-width 80)

(with-eval-after-load 'emacs
  (require 'repeat)
  (repeat-mode 1)
  (require 'auth-source-pass)
  (auth-source-pass-enable))

(custom-set-faces '(Info-quoted ((t (:foreground "orange" :slant italic :family "Jetbrains Mono")))))
(setq delete-by-moving-to-trash t)
(provide 'init-misc)
