;; -*- lexical-binding: t -*-

(install-pkgs mistty
              ghostel)

;; Force eshell to show all completions
(setq eshell-cmpl-cycle-completions nil)
(setq pcomplete-cycle-completions nil)

;; Use completing-read (minibuffer) for eshell completion
;; (add-hook 'shell-mode-hook
;;           (lambda ()
;;             (setq eshell-cmpl-ignore-case t)
;;             (define-key shell-mode-map (kbd "TAB") 'completion-at-point)))

;; Force completions to minibuffer
;; (setq completion-in-region-function
;;       (lambda (start end collection &optional predicate)
;;         (let* ((initial (buffer-substring-no-properties start end))
;;                (completion (completing-read "Complete: " collection predicate t initial)))
;;           (unless (string= initial completion)
;;             (delete-region start end)
;;             (insert completion)))))

(defun term/disable-hl-and-line-numbers ()
  (and global-hl-line-mode (hl-line-mode 'toggle))  ;; that 'toggle is needed, other negative args can't undo the global mode
  (display-line-numbers-mode -1))

(with-eval-after-load 'mistty
  (defun mistty-hard-clear (n)
    "Clear the MisTTY buffer until the end of the last output.
With an argument, clear from the end of the last Nth output."
    (interactive "p")
    (let ((range (save-excursion (mistty-previous-output 0))))
      (mistty-truncate (min mistty-sync-marker (cdr range)))))
    (define-key mistty-mode-map (kbd "C-c M-o") 'mistty-hard-clear))

(dolist (hook '(ghostel-mode-hook mistty-mode-hook shell-mode-hook term-mode-hook))
  (add-hook hook #'term/disable-hl-and-line-numbers))

(define-key project-prefix-map "s" 'mistty-in-project)

(provide 'init-shell)
