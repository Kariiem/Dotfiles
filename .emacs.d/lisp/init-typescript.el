;; -*- lexical-binding: t -*-

(install-pkgs typescript-mode
              tide
              web-mode)

(defun my-json-conf()
  (setq-local js-indent-level 2))

(add-hook 'json-mode-hook #'my-json-conf)

(require 'flycheck)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode)
  (eldoc-mode)
  (tide-hl-identifier-mode))

(defun tsx-tide-mode()
  (when (string-equal "tsx"
                      (file-name-extension buffer-file-name))
    (setup-tide-mode)))

;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)
;; aligns annotation to the right hand side

(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'web-mode-hook #'tsx-tide-mode)


(provide 'init-typescript)
