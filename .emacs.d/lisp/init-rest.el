;;; -*- lexical-binding: t -*-
(install-pkgs restclient
              company-restclient)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-restclient))

(add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode))

(provide 'init-rest)
