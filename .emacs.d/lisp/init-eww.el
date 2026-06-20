;; -*- lexical-binding: t; -*-

(install-pkgs shrface
              shr-tag-pre-highlight)

;; Standard Single Lines
(setq shr-table-vertical-line #x2502)
(setq shr-table-horizontal-line #x2500)
;; OR: Heavy Lines if the standard ones are too thin
;; (setq shr-table-vertical-line #x2503)
;; (setq shr-table-horizontal-line #x2501)

(setq shr-use-fonts nil
      shr-use-colors nil
      shr-indentation 0
      shr-width 80
      shr-indentation 4
      shr-inhibit-images nil
      shr-cookie-policy nil
      shr-image-ascent 'center
      eww-auto-rename-buffer 'title)

;; (set-face-attribute 'variable-pitch nil
;;                     :family "Inter"
;;                     :height 120
;;                     :weight 'regular)

;; (set-face-attribute 'fixed-pitch nil
;;                     :family "JetBrains Mono"
;;                     :height 10)

;; (set-face-attribute 'shr-text nil :inherit 'fixed-pitch)

(defun eww/init ()
  (display-line-numbers-mode -1)
  (setq-local line-spacing nil)
  ;; (variable-pitch-mode)
  )

(add-hook 'eww-mode-hook #'eww/init)

(defun kk/shr-tag-pre-highlight (dom)
  "Place code in PRE inside an org src_block."
  (let* ((shr-folding-mode 'none)
         (shr-current-font 'default)
         (shr-external-rendering-functions ;; to fight against some stupid nested code tags
          (remove '(pre . kk/shr-tag-pre-highlight) shr-external-rendering-functions)))
    (shr-ensure-newline)
    (shr-ensure-newline)
    (setq start (point))
    (shr-insert (propertize "#+BEGIN_SRC\n" 'face 'org-block-begin-line))
    (shr-generic dom)
    (shr-ensure-newline)
    (setq inside-codeblock nil)
    (shr-insert (propertize "#+END_SRC" 'face 'org-block-end-line))
    (shr-ensure-newline)
    (setq end (point))
    (shr-ensure-newline)
    (shr-ensure-newline)
    (pcase (frame-parameter nil 'background-mode)
      ('light
       (add-face-text-property start end '(:background "#D8DEE9" :extend t)))
      ('dark
       (add-face-text-property start end '(:background "#292b2e" :extend t))))
    (shr-ensure-newline)))

(with-eval-after-load 'eww
  (require 'org-faces)
  (push '(pre . kk/shr-tag-pre-highlight) shr-external-rendering-functions))

(with-eval-after-load 'nov
  (push '(pre . kk/shr-tag-pre-highlight) nov-shr-rendering-functions))

(provide 'init-eww)
