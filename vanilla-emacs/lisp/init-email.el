(autoload 'notmuch "notmuch" "notmuch mail" t)

(with-eval-after-load 'notmuch
  (setq user-mail-address "kariem.taha2.7@gmail.com"
        user-full-name "Karim Taha")

  (setq smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-stream-type 'ssl
        smtpmail-smtp-service 465
        smtpmail-debug-info t
        message-send-mail-function 'smtpmail-send-it)

  (setq mail-user-agent 'message-user-agent)
  (setq message-default-mail-headers "Cc: \nBcc: \n"
        message-auto-save-directory "~/mail/draft"
        message-kill-buffer-on-exit t
        message-directory "~/mail/")


  (setq-default notmuch-search-oldest-first nil)
  (setq notmuch-show-logo nil
        notmuch-show-all-tags-list t
        notmuch-show-indent-messages-width 8
        notmuch-show-max-text-part-size 0
        notmuch-show-relative-dates t
        notmuch-show-imenu-indent nil
        notmuch-show-header-line nil
        notmuch-wash-wrap-lines-length 80
        notmuch-hello-sections '(notmuch-hello-insert-recent-searches
                                 notmuch-hello-insert-alltags)))
;;(remove-hook 'notmuch-show-hook 'notmuch-show-turn-on-visual-line-mode)

(defun notmuch-show-subject-tabs-to-spaces ()
  "Replace tabs with spaces in subject line."
  (goto-char (point-min))
  (when (re-search-forward "^Subject:" nil t)
    (while (re-search-forward "\t" (line-end-position) t)
      (replace-match " " nil nil))))

(add-hook 'notmuch-show-markup-headers-hook 'notmuch-show-subject-tabs-to-spaces)

(provide 'init-email)
