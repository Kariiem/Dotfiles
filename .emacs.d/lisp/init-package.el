;; -*- lexical-binding: t -*-

(require 'package)



(defun install-package (package)
  (if (package-installed-p package)
      (push package package-selected-packages)
    (package-install package)))

(defun maybe-install-package (package)
  (condition-case err
      (install-package package)
    (error (message "Package `%s' couldn't be installed: %s" package err))))

(defmacro install-pkgs (one &rest pkgs)
  `(let ((pkgs (cons ',one ',pkgs)))
     (mapc #'maybe-install-package pkgs)))


(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq package-install-upgrade-built-in t
      package-native-compile t
      load-prefer-newer t)
;; (package-initialize)
(package-activate-all)

(provide 'init-package)
