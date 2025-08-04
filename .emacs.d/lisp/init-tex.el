(install-pkgs auctex)

(setq TeX-auto-save t
      TeX-parse-self t
      TeX-command-extra-options "-shell-escape -file-line-error -interaction=nonstopmode"
      TeX-save-query nil
      TeX-show-compilation nil)

;; Enable AUCTeX for .tex files
(add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)
(add-hook 'LaTeX-mode-hook #'TeX-PDF-mode)

(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))

(with-eval-after-load 'latex
  (add-to-list 'TeX-command-list
               '("LatexMkXeLaTeX" "latexmk -xelatex -shell-escape -interaction=nonstopmode %s"
                 TeX-run-TeX nil t :help "Run LatexMk with XeLaTeX")))

(with-eval-after-load 'latex
  (add-to-list 'TeX-command-list
               '("PDFLaTeX" "pdflatex -file-line-error -interaction=nonstopmode %s"
                 TeX-run-TeX nil t :help "Run pdflatex")))


(provide 'init-tex)
