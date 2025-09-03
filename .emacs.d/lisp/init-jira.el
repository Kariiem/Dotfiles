;; -*- lexical-binding: t -*-
(install-pkgs jira)
(setq jira-base-url "https://sonnen.atlassian.net"
      jira-token-is-personal-access-token nil
      jira-debug nil
      jira-api-version 3)

(provide 'init-jira)
