;;; -*- lexical-binding: t -*-

(install-pkgs x86-lookup
              nasm-mode)

(with-eval-after-load 'fasm-mode
  (setq fasm-basic-offset 4
        x86-lookup-pdf "~/probe/asm/x86_64.pdf"))

(defun imenu-setup-fasm-manual ()
  (interactive)
  (setq imenu-create-index-function 'imenu-default-create-index-function)
  (setq imenu-generic-expression
        '((nil
           "^\\(\\(?:[0-9]\\.\\)+[0-9]+ .*+\\)"
           1)))
  (setq vertico-sort-function 'vertico-sort-alpha))

(global-set-key (kbd "C-h x") #'x86-lookup)
(add-to-list 'auto-mode-alist '("\\.asm\\'" . fasm-mode))

(provide 'init-asm)
