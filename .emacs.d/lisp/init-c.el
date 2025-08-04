;; -*- lexical-binding: t -*-
(setq c-default-style "stroustrup"
      c-syntactic-indentation t)


(defun my-c-mode-add-typedefs-to-imenu ()
  "Append typedef matching to imenu for c-mode without overwriting existing patterns."
  (let ((typedef-patterns
         '(("Class"
            ;; Match typedef with struct/union/enum body
            "^[ \t]*typedef[ \t]+\\(?:struct\\|enum\\|union\\)[^{;]*{[^}]*}[ \t\n]*\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]*;"
            1)
           ("Alias"
            ;; Match typedef without body (simple types and forward decls)
            "^[ \t]*typedef[ \t]+\\(?:struct\\|enum\\|union\\)?[ \t\n]*\\(?:[^;{]*[ \t\n]+\\)?\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]*;"
            1)
           )))
    ;; Combine safely
    (setq-local imenu-generic-expression
                (append typedef-patterns imenu-generic-expression))
    (setq-local imenu-create-index-function #'imenu-default-create-index-function)
    (imenu-add-to-menubar "Imenu")))

(add-hook 'c-mode-hook #'my-c-mode-add-typedefs-to-imenu)


(with-eval-after-load 'c-mode
  (defun c-help()
  (interactive)
  (other-window-prefix)
  (man (concat (word-at-point t) "(2)")))
  (define-key c-mode-map (kbd "C-c C-i") 'c-help))
(provide 'init-c)
