;;; init-eglot.el --- setup eglot -*- lexical-binding: t -*-

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(text-mode . ("harper-ls" "--stdio"))))

(provide 'init-eglot)
