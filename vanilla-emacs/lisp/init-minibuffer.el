(require-package 'amx)
(amx-mode 1)
(setq recentf-max-menu-items 1000)
(setq recentf-max-saved-items 1000)
(recentf-mode 1)
(require 'recentf)

(global-set-key (kbd "C-x C-r") 'recentf)


(provide 'init-minibuffer)
