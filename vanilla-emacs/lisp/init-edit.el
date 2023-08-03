(package-install 'multiple-cursors)

(setq kill-whole-line t)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(defun dup-line ()
  (interactive)
  (let* ((current-point-col (- (point) (point-at-bol)))
         (current-line-text (let ((text (thing-at-point 'line t)))
                              (if text (string-chop-newline text) ""))))
    (move-end-of-line 1)
    (newline)
    (insert current-line-text)
    (move-beginning-of-line 1)
    (forward-char current-point-col)))


(global-set-key (kbd "C-,") 'dup-line)

(provide 'init-edit)
