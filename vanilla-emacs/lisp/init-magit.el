(require-package 'magit)

(setq magit-define-global-key-bindings 'recommended)
(with-eval-after-load 'magit
    (transient-define-argument magit-commit:--reedit-message ()
      :description "Reedit commit message"
      :class 'transient-option
      :shortarg "-c"
      :argument "--reedit-message="
      :reader #'magit-read-reedit-message
      :history-key 'magit-revision-history)

  (defun magit-read-reedit-message (prompt &optional default history)
    (magit-completing-read prompt (magit-list-refnames)
                           nil nil nil history
                           (or default
                               (and (magit-rev-verify "ORIG_HEAD")
                                    "ORIG_HEAD"))))
  (transient-append-suffix 'magit-commit "-C"
    '(magit-commit:--reedit-message)))
(provide 'init-magit)
