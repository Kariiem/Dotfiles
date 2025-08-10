;; -*- lexical-binding: t -*-

(install-pkgs multiple-cursors)

(setq kill-whole-line t
      mc/always-run-for-all t)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)


;; https://emacs.stackexchange.com/questions/2347/kill-or-copy-current-line-with-minimal-keystrokes
(defun slick-cut (beg end &rest _)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

(advice-add 'kill-region :before #'slick-cut)

;;

(defun slick-copy (beg end &rest _)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))

(advice-add 'copy-region-as-kill :before #'slick-copy)
(global-set-key [remap kill-ring-save] 'copy-region-as-kill)
;;

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


(defvar hidden-regions nil
  "list of hidden regions")

(defun hide-region ()
  (interactive)
  (when mark-active
    (let ((beg (region-beginning))
          (end (region-end)))
      (put-text-property beg end 'invisible 'hidden)
      (push (cons beg end) hidden-regions))))

(defun unhide-last-region ()
  (interactive)
  (if hidden-regions
      (let ((last-region (pop hidden-regions)))
        (put-text-property (car last-region) (cdr last-region) 'invisible nil))
    (error "No hidden regions found")))


(provide 'init-edit)
