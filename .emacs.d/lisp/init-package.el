;; -*- lexical-binding: t -*-

(require 'package)

(defun install-package (package)
  (unless (package-installed-p package)
    (package-install package)))

(defun maybe-install-package (package)
  (condition-case err
      (install-package package)
    (error (message "Package `%s' couldn't be installed: %s" package err))))

(defmacro install-pkgs (one &rest pkgs)
  `(let ((pkgs (cons ',one ',pkgs)))
     (mapc #'maybe-install-package pkgs)))

(setq package-enable-at-startup nil
      package-install-upgrade-built-in t
      package-native-compile t
      load-prefer-newer t)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(provide 'init-package)
