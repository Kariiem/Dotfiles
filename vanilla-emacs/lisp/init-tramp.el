(require 'tramp)
(tramp-set-completion-function "ssh" '((tramp-parse-sconfig "/etc/ssh/ssh_config")
                                       (tramp-parse-sconfig "~/.ssh/config")))
;;(customize-set-variable 'tramp-default-method "ssh")
;;(tramp-change-syntax 'simplified)

(provide 'init-tramp)
