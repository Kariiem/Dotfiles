;; -*- lexical-binding: t -*-
(eval-and-compile
  (require 'package))

(defvar package-selected-packages)

(defun maybe-install-package (package)
  (condition-case err
      (unless (package-installed-p package)
        (package-install package))
    (error (message "Package `%s' couldn't be installed: %s" package err))))

(defmacro install-pkgs (&rest pkgs)
  "Compiles into a simple mapc call over a quoted list of symbols."
  `(mapc #'maybe-install-package ',pkgs))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq package-install-upgrade-built-in t
      package-native-compile t
      load-prefer-newer t)

;; (package-initialize)
;; (package-activate-all)

(provide 'init-package)
