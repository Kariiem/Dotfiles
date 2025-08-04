;; -*- lexical-binding: t -*-
(install-pkgs sweeprolog flymake-swi-prolog)

;; (require 'flymake-swi-prolog)
;; (add-hook 'prolog-mode-hook #'flymake-swi-prolog-setup-backend)

(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

;; (add-to-list 'auto-mode-alist '("\\.pl\\'" . sweeprolog-mode))

(provide 'init-prolog)
