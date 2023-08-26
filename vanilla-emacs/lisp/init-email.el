(autoload 'notmuch "notmuch" "notmuch mail" t)
(setq-default notmuch-search-oldest-first nil)
(setq notmuch-show-log nil
      notmuch-show-all-tags-list t
      notmuch-show-indent-messages-width 8
      notmuch-hello-sections '(notmuch-hello-insert-saved-searches
                               notmuch-hello-insert-alltags))

(provide 'init-email)
