;; -*- lexical-binding: t -*-
(install-pkgs jira)
(setq jira-base-url "https://sonnen.atlassian.net"
      jira-token-is-personal-access-token nil
      jira-debug nil
      jira-api-version 3
      jira-users-max-results 10000
      jira-comments-display-recent-first nil)

(add-to-list 'display-buffer-alist
             '("\\*jira-issues\\*"
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side . top)
               (window-height . 0.25)))

(provide 'init-jira)
