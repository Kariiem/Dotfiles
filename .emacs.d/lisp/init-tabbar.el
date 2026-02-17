;;; init-tabbar.el --- Modern browser-like tab-bar styling -*- lexical-binding: t; -*-

;; written by claude code

;;; Commentary:
;; Modern, clean tab-bar faces that look like browser tabs
;; - No boxes/borders
;; - Seamless integration with background
;; - Rounded/slanted edges using Unicode characters
;; - Active tab blends with editor background

;;; Code:

;; === FACE CUSTOMIZATION ===

;; Base tab-bar (the bar itself)
(set-face-attribute 'tab-bar nil
                    :background "#1e2024"    ; Match your background
                    :foreground "#f8f8f2"    ; Match your foreground
                    :box nil                 ; NO BOX!
                    :inherit 'unspecified)

;; Active/selected tab - blends with editor background
(set-face-attribute 'tab-bar-tab nil
                    :background "#292C31"    ; Same as editor background
                    :foreground "#f8f8f2"    ; Active text color
                    :weight 'bold            ; Make it stand out
                    :box       nil ;; '(:line-width (1 . -1) :color "white")
                    :underline nil ;; '(:color "white" :style line :position t)
                    :overline "white"
                    :inherit 'unspecified)

;; Inactive tabs - slightly dimmed
(set-face-attribute 'tab-bar-tab-inactive nil
                    :background "#1e2024"    ; Darker than bar
                    :foreground "#7a7f8a"    ; Dimmed text
                    :weight 'normal
                    :box nil                 ; NO BOX!
                    :underline nil
                    :overline nil
                    :inherit 'unspecified)


(defun my/tab-bar-format (tab i)
  "Format TAB with Chrome-like appearance."
  (let* ((current-p (eq (car tab) 'current-tab))
         (tab-name (alist-get 'name tab))
         (face (if current-p 'tab-bar-tab 'tab-bar-tab-inactive))
         (indicator (if current-p "[" " ")))
    (concat
     (propertize " " 'face face)
     (propertize (format "%s" tab-name) 'face  face)
     (propertize " " 'face face))))


;; === TAB-BAR DYNAMIC NAMING ===

(defvar tab-name-width 16)

;; courtesy of Claude Code
(defun tab-bar-center-string (str max-width &optional pad-char ellipsis)
  "Center STR within MAX-WIDTH characters.
If STR is shorter than MAX-WIDTH, pad with PAD-CHAR (default space).
If STR is longer than MAX-WIDTH, truncate with ELLIPSIS."
  (let* ((pad-char (or pad-char ?\s))
         (ellipsis (or ellipsis ""))
         (str-length (string-width str)))
    (if (<= str-length max-width)
        (let* ((padding (- max-width str-length))
               (left-pad (/ padding 2))
               (right-pad (- padding left-pad)))
          (concat (make-string left-pad pad-char)
                  str
                  (make-string right-pad pad-char)))
      ;; If string is longer than max-width, truncate it
      (truncate-string-to-width str max-width nil pad-char ellipsis))))

(defun tab-bar-prompt-name-function ()
  "Prompt for and return a tab name, with truncation."
  (let* ((repeat-mode nil)
         (raw-name (read-string "New-tab name: " nil nil "default"))
         (trimmed (string-trim (or raw-name "")))
         (name (or (and (string-empty-p trimmed) "Untitled")
                   trimmed)))
    (tab-bar-center-string name tab-name-width ?\s "")))

(defun tab-bar-rename-current-tab-prompt ()
  "Rename the current tab interactively."
  (interactive)
  (let ((new-name (tab-bar-prompt-name-function)))
    (tab-bar-rename-tab new-name)))

(defun tab-bar--rename-initial-tab ()
  "Rename the initial tab when tab-bar-mode is enabled."
  (when (and tab-bar-mode
             (= (length (tab-bar-tabs)) 1))
    (unless (alist-get 'explicit-name (tab-bar--current-tab))
      (tab-bar-rename-current-tab-prompt))
    (tab-bar--update-tab-bar-lines)))

(defun tab-bar--mark-tab-explicit-name (tab)
  "Mark TAB as having an explicit name to prevent auto-renaming."
  (setf (alist-get 'explicit-name (cdr tab)) t))

(defun tab-bar--reapply-string-fmt (tab)
  "Mark TAB as having an explicit name to prevent auto-renaming."
  (let ((name (alist-get 'name (cdr tab))))
    (setf name (tab-bar-center-string name tab-name-width ?\s ""))
    (force-mode-line-update)))

;; new tab buffer
(defun my-tab-bar-new-tab-buffer ()
  (if current-prefix-arg
      (current-buffer)
    (scratch-buffer)))

;; Tab bar format - remove close buttons, add separators
(setq tab-bar-show 0)
(setopt tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-tab-hints t
        tab-bar-separator " "
        tab-bar-select-tab-modifiers '(meta)
        tab-bar-tab-name-format-function #'my/tab-bar-format
        tab-bar-new-tab-choice           #'my-tab-bar-new-tab-buffer
        tab-bar-tab-name-function        #'tab-bar-prompt-name-function
        tab-bar-tab-post-open-functions   '(tab-bar--mark-tab-explicit-name tab-bar--reapply-string-fmt))

(add-hook 'tab-bar-mode-hook #'tab-bar--rename-initial-tab)

;; === KEYBINDINGS ===

(defvar-keymap tab-bar-repeat-map
  "k" #'tab-bar-close-tab
  "n" #'tab-bar-switch-to-next-tab
  "p" #'tab-bar-switch-to-prev-tab)

(put 'tab-bar-close-tab 'repeat-map tab-bar-repeat-map)
(put 'tab-bar-switch-to-next-tab 'repeat-map tab-bar-repeat-map)
(put 'tab-bar-switch-to-prev-tab 'repeat-map tab-bar-repeat-map)


(global-set-key (kbd "C-c t") #'tab-bar-new-tab)
(global-set-key (kbd "C-c k") #'tab-bar-close-tab)
(global-set-key (kbd "C-c n") #'tab-bar-switch-to-next-tab)
(global-set-key (kbd "C-c p") #'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-c r") #'tab-bar-rename-tab)

;; Quick switch with number
(dotimes (i 9)
  (global-set-key (kbd (format "M-%d" (1+ i)))
                  `(lambda () (interactive) (tab-bar-select-tab ,(1+ i)))))


;; Enable tab-bar mode
;; (tab-bar-mode 1)

(provide 'init-tabbar)
