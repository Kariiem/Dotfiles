;; -*- lexical-binding: t -*-
(install-pkgs elm-mode)

;; TODO: PR to fix it upstream
(defun elm-package-latest-version (package)
  "Get the latest version of PACKAGE."
  (let ((entry (assoc `(name . ,package) elm-package--contents)))
    (if (not entry)
        (error "Package not found")
      (let ((versions (cdadr entry)))
        (elt versions 0)))))

(provide 'init-elm)
