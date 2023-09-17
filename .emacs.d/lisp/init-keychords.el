;; -*- lexical-binding: t -*-

(defun define-key-chord (key-map key fun)
  (let ((key1 (aref (string-to-vector (kbd key)) 0))
        (key2 (aref (string-to-vector (kbd key)) 1))
        (rkey1 (read-key)))
    (if (eq rkey1 key1)
               (let ((rkey2 (read-event nil nil 0.25)))
                 (if (eq rkey2 key2)
                     (funcall fun)
                   (insert rkey1 rkey2)))
      (insert rkey1))))
;;(define-key-chord nil "jk" (lambda () (message "hello from keychords")))
(global-set-key (kbd "C-q") 'save-buffers-kill-emacs)
(provide 'init-keychords)
