;; -*- lexical-binding: t -*-
(unless (package-installed-p 'ada-light-mode)
  (package-vc-install "https://github.com/sebastianpoeplau/ada-light-mode.git"
                      nil nil 'ada-light-mode))
(provide 'init-ada)
