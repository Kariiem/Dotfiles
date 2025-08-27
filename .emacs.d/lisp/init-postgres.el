;; -*- lexical-binding: t -*-

(unless (package-installed-p 'pg)
   (package-vc-install "https://github.com/emarsden/pg-el" nil nil 'pg))

(unless (package-installed-p 'pgmacs)
   (package-vc-install "https://github.com/emarsden/pgmacs" nil nil 'pgmacs))

(require 'pgmacs)

(provide 'init-postgres)
