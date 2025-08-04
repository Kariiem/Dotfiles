;;; init-hledger.el --- Add support for hledger journal files -*- lexical-binding: t -*-
(install-pkgs hledger-mode
              flycheck-hledger)

(add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))

(with-eval-after-load 'hledger
  (global-set-key (kbd "C-c j") 'hledger-run-command)
  (global-set-key (kbd "C-c e") 'hledger-jentry)
  (define-key hledger-mode-map (kbd "<kp-add>") 'hledger-increment-entry-date)
  (define-key hledger-mode-map (kbd "<kp-subtract>") 'hledger-decrement-entry-date)
  (define-key hledger-mode-map (kbd "M-p") 'hledger/prev-entry)
  (define-key hledger-mode-map (kbd "M-n") 'hledger/next-entry)

  ;; Personal Accounting
;;; src: README.md from https://github.com/narendraj9/hledger-mode
  (defun hledger/next-entry ()
    "Move to next entry and pulse."
    (interactive)
    (hledger-next-or-new-entry)
    (hledger-pulse-momentary-current-entry))

  (defun hledger/prev-entry ()
    "Move to last entry and pulse."
    (interactive)
    (hledger-backward-entry)
    (hledger-pulse-momentary-current-entry)))

(add-hook 'hledger-view-mode-hook #'hl-line-mode)
(add-hook 'hledger-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends 'hledger-company)))

(provide 'init-hledger)
