;; -*- lexical-binding: t -*-

(install-pkgs org-tidy)

(defun position-link ()
  (interactive)
  (kill-new
   (format "[[file:%s::%d][%s]]"
           (buffer-file-name)
           (save-restriction (widen) (line-number-at-pos))
           (symbol-at-point))))

(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
      org-return-follows-link t
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil
      org-id-locations-file-relative t
      org-ellipsis "\n➤"
      org-edit-src-content-indentation 0)

(defun tangle-in-org (&optional arg target-file lang-re)
  (when (string-equal major-mode 'org-mode)
    (org-babel-tangle arg target-file lang-re)))


(add-hook 'org-mode-hook #'org-tidy-mode)
;;(add-hook 'prog-mode-hook (local-set-key (kbd "C-c c") 'position-link))

(with-eval-after-load 'org
  (require 'ox-extra)
  (require 'org-tempo)
  (ox-extras-activate '(latex-header-blocks ignore-headlines))
  (add-to-list 'org-src-lang-modes '("haskell" . haskell))

  (add-hook 'after-save-hook 'tangle-in-org)

  (global-set-key (kbd "C-c s") #'org-store-link)
  (global-set-key (kbd "C-c l") #'org-insert-last-stored-link)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture))

(provide 'init-org)
