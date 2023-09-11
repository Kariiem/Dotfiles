(defun position-link ()
  (interactive)
  (kill-new
   (format "[[file:%s::%d][%s]]"
           (buffer-file-name)
           (save-restriction (widen) (line-number-at-pos))
           (symbol-at-point))))

(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil
      org-id-locations-file-relative t
      org-edit-src-content-indentation 0)

(defun tangle-in-org (&optional arg target-file lang-re)
  (when (string-equal major-mode 'org-mode)
    (org-babel-tangle arg target-file lang-re)))

(with-eval-after-load 'org
  (add-to-list 'org-src-lang-modes '("haskell" . haskell))

  (add-hook 'org-mode-hook (lambda ()
                             (add-hook 'after-save-hook 'tangle-in-org)))
  (require 'org-tempo)
  (global-set-key (kbd "C-c s") #'org-store-link)
  (global-set-key (kbd "C-c l") #'org-insert-last-stored-link)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture))

(add-hook 'prog-mode-hook (lambda ()
                            (local-set-key (kbd "C-c c") 'position-link)))

(provide 'init-org)
