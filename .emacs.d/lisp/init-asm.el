;;; -*- lexical-binding: t -*-

(install-pkgs x86-lookup
              nasm-mode)

(require 'fasm-mode)
(setq fasm-basic-offset 4
      x86-lookup-pdf "~/probe/asm/x86_64.pdf")

(global-set-key (kbd "C-h x") #'x86-lookup)
(add-to-list 'auto-mode-alist '("\\.asm\\'" . fasm-mode))

(provide 'init-asm)
