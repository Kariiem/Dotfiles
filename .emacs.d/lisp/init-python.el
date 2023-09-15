(require-package 'elpy)
(advice-add 'python-mode :before 'elpy-enable)
(provide 'init-python)
