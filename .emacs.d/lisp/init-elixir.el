;; -*- lexical-binding: t; -*-

(install-pkgs elixir-mode
              inf-elixir
              mix)

;; Autoload inf-elixir commands
(autoload 'inf-elixir "inf-elixir" nil t)
(autoload 'inf-elixir-project "inf-elixir" nil t)
(autoload 'inf-elixir-send-line "inf-elixir" nil t)
(autoload 'inf-elixir-send-region "inf-elixir" nil t)
(autoload 'inf-elixir-send-buffer "inf-elixir" nil t)
(autoload 'inf-elixir-reload-module "inf-elixir" nil t)

(defun elixir/config-pretty-symbols()
  (push '(">=" . ?\u2265) prettify-symbols-alist)
  (push '("<=" . ?\u2264) prettify-symbols-alist)
  (push '("!=" . ?\u2260) prettify-symbols-alist)
  (push '("==" . ?\u2A75) prettify-symbols-alist)
  (push '("=~" . ?\u2245) prettify-symbols-alist)
  (push '("<-" . ?\u2190) prettify-symbols-alist)
  (push '("->" . ?\u2192) prettify-symbols-alist)
  (push '("<-" . ?\u2190) prettify-symbols-alist)
  (push '("|>" . ?\u25B7) prettify-symbols-alist)
  (prettify-symbols-mode))

(with-eval-after-load 'elixir-mode
  (add-hook 'elixir-mode-hook #'elixir/config-pretty-symbols)
  (add-hook 'elixir-mode-hook #'mix-minor-mode)
  ;; (add-hook 'before-save-hook #'elixir-format)
  (define-key elixir-mode-map (kbd "C-c i i") 'inf-elixir)
  (define-key elixir-mode-map (kbd "C-c i p") 'inf-elixir-project)
  (define-key elixir-mode-map (kbd "C-c i l") 'inf-elixir-send-line)
  (define-key elixir-mode-map (kbd "C-c i r") 'inf-elixir-send-region)
  (define-key elixir-mode-map (kbd "C-c i b") 'inf-elixir-send-buffer)
  (define-key elixir-mode-map (kbd "C-c i R") 'inf-elixir-reload-module))

(provide 'init-elixir)
