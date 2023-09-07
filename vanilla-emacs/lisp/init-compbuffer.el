(defun backward-updir ()
  "Delete char before or go up directory, like `ido-mode'."
  (interactive)
  (cond ((and (eq (char-before) ?/)
              (eq (icomplete--category) 'file))
         (when (string-equal (icomplete--field-string) "~/")
           (delete-region (icomplete--field-beg) (icomplete--field-end))
           (insert (expand-file-name "~/"))
           (goto-char (line-end-position)))
         (save-excursion
           (goto-char (1- (point)))
           (when (search-backward "/" (point-min) t)
             (delete-region (1+ (point)) (point-max)))))
        (t (call-interactively 'backward-delete-char))))

(setq completion-show-help nil)
(setq completions-format 'one-column)
(setq completions-max-height 10)

(setq enable-recursive-minibuffers nil)
(define-key minibuffer-mode-map (kbd "C-n") 'minibuffer-next-completion)
(define-key minibuffer-mode-map (kbd "C-p") 'minibuffer-previous-completion)


(define-key completion-in-region-mode-map (kbd "C-n") 'minibuffer-next-completion)
(define-key completion-in-region-mode-map (kbd "C-p") 'minibuffer-previous-completion)
(setq completion-auto-help 'visible)
(setq completion-auto-select 'second-tab)
(setq completion-auto-wrap t)
