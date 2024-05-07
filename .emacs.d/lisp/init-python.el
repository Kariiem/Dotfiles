;; -*- lexical-binding: t -*-

(install-pkgs elpy
              ein
              polymode
              poly-markdown)
(advice-add 'python-mode :before 'elpy-enable)
(provide 'init-python)
