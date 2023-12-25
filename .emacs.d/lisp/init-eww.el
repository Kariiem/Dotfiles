;; -*- lexical-binding: t; -*-

(with-eval-after-load 'shr
  (setq shr-use-fonts nil
        shr-use-colors nil
        shr-indentation 0
        shr-table-horizontal-line ?\-
        shr-width 80
        shr-inhibit-images t
        eww-auto-rename-buffer 'title)
  (add-hook 'eww-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)))

  (add-to-list 'shr-external-rendering-functions
               '(haskell . eww-tag-haskell))
  ;; (defun shr-tag-pre (dom)                ;;
  ;;   (let ((shr-folding-mode nil)          ;;
  ;;         (shr-current-font 'default))    ;;
  ;;     (shr-ensure-newline)                ;;
  ;;     (shr-generic dom)                   ;;
  ;;     (shr-ensure-newline)))              ;;
  )

(provide 'init-eww)
