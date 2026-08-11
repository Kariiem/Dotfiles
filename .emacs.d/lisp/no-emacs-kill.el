;; -*- lexical-binding: t -*-

(defgroup no-emacs-kill nil
  "Emacs-Keep-Alive mode."
  :version "24.1"
  :group 'convenience)

(defun no-emacs-kill--kill-emacs-hook ()
  "Signal an error if no-emacs-kill mode is on.
Used from `kill-emacs-hook' (which see)."
  (when no-emacs-kill-mode
    (error "Emacs cannot exit, No-Emacs-Kill-Mode is on.")))

(defun no-emacs-kill--kill-emacs-query-functions ()
  "Display a message if emacs is to be kept alive.
Return a value appropriate for `kill-emacs-query-functions' (which see)."
  (if (not no-emacs-kill-mode)
      t
    (message "Emacs cannot exit because No-Emacs-Kill-Mode is on.")
    nil))

(define-minor-mode no-emacs-kill-mode
  "Toggle Emacs keep alive mode.
If called with a plain prefix argument, toggle."
  :init-value nil
  :global t
  :lighter ("" (no-emacs-kill-mode  " NoExit"))
  :group 'no-emacs-kill
  (if no-emacs-kill-mode
      (progn
        (add-hook 'kill-emacs-hook 'no-emacs-kill--kill-emacs-hook)
        (add-hook 'kill-emacs-query-functions 'no-emacs-kill--kill-emacs-query-functions))
    (remove-hook 'kill-emacs-hook 'no-emacs-kill--kill-emacs-hook)
    (remove-hook 'kill-emacs-query-functions 'no-emacs-kill--kill-emacs-query-functions)))

(provide 'no-emacs-kill)
