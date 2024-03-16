;; -*- lexical-binding: t -*-

(require 'package)

(defun require-package (package)
  (unless (package-installed-p package)
    (package-install package)))

(defun maybe-require-package (package)
  (condition-case err
      (require-package package)
    (error (message "Package `%s' couldn't be installed: %s" package err))))

(setq package-enable-at-startup nil
      package-install-upgrade-built-in t
      package-native-compile t
      load-prefer-newer t)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(provide 'init-package)
