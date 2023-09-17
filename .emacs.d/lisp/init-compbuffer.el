;; -*- lexical-binding: t -*-

(defun id (p) p)
(defun mk-completion-metadata (string table pred category display-sort-function)
  (let ((metadata (if (functionp table)
                      (funcall table string pred 'metadata)
                    `(metadata (category ,category)
                               (display-sort-function ,display-sort-function)))))
    (cons 'metadata
          (if (eq (car-safe metadata) 'metadata)
              (cdr metadata)))))

(mk-completion-metadata "Load themes" (mapcar #'symbol-name custom-known-themes) t 'themes 'id)
(cons 'metadata '(category . file))
(alist-get 'category (mk-completion-metadata "Load theme: "  custom-known-themes t 'one-theme 'id))
(mk-completion-metadata "Load themes" (mapcar #'symbol-name custom-known-themes) t 'themes 'id)
(completion-metadata-get (mk-completion-metadata "Load themes" (mapcar #'symbol-name custom-known-themes) t 'themes 'id)
                         'display-sort-function)
(completing-read "Load theme: " custom-known-themes)
