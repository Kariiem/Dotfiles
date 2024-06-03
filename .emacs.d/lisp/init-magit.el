;; -*- lexical-binding: t -*-
(install-pkgs magit)

(with-eval-after-load 'magit
  (setq magit-log-margin '(t "%Y-%m-%d %R" magit-log-margin-width t 18))

  (transient-define-infix magit-commit:--reedit-message ()
    :description "Reedit commit message"
    :class 'transient-option
    :shortarg "-c"
    :argument "--reedit-message="
    :reader #'magit-read-reedit-message
    :history-key 'magit-revision-history)

  (defun magit-remote-list-remotes()
    (interactive)
    (magit-run-git "remote" "-v"))

  (defun magit-read-reedit-message (prompt &optional default history)
    (magit-completing-read prompt (magit-list-refnames)
                           nil nil nil history
                           (or default
                               (and (magit-rev-verify "ORIG_HEAD")
                                    "ORIG_HEAD"))))

  (transient-append-suffix
    'magit-commit "-C" '(magit-commit:--reedit-message))
  (transient-append-suffix
    'magit-remote "a" '("l" "List all remotes" magit-remote-list-remotes)))

(provide 'init-magit)
