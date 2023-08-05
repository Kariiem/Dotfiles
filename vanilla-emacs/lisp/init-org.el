(defun position-link ()
  (interactive)
  (kill-new
   (format "[[%s::%d][%s]]"
           (buffer-file-name)
           (save-restriction (widen) (line-number-at-pos))
           (symbol-at-point))))

(add-hook 'prog-mode-hook (lambda ()
                            (local-set-key (kbd "C-c c") 'position-link)))

(setq org-src-fontify-natively t
    org-src-tab-acts-natively t
    org-confirm-babel-evaluate nil
    org-edit-src-content-indentation 0)
(provide 'init-org)
