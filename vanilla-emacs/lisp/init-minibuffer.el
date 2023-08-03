(require-package 'amx)
(amx-mode 1)
(recentf-mode 1)
(require 'recentf)
(defun fido-open-recentf-files ()
  (interactive)
  (let ((selection (completing-read "Open recent files: " recentf-list)))
    (if (find-file selection)
        nil
      (message "File not found!"))))
(global-set-key (kbd "C-x C-r") 'fido-open-recentf-files)

(provide 'init-minibuffer)
