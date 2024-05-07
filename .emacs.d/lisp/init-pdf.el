;;; -*- lexical-binding: t -*-

(install-pkgs pdf-tools
              saveplace-pdf-view)
(pdf-loader-install)

(setq-default pdf-view-display-size 'fit-page)
;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
(setq pdf-view-use-scaling t
      pdf-view-use-imagemagick nil)


(defun pdf/scroll-other-window-down (&optional lines)
  "Scroll next window downward LINES lines; or near full screen if no ARG.
See `scroll-down-command' for details."
  (interactive "P")
  (with-selected-window (other-window-for-scrolling)
    (if (eq major-mode 'pdf-view-mode)
        (pdf-view-previous-line-or-previous-page (or lines 1)))))

(defun pdf/scroll-other-window-up (&optional lines)
  "Scroll next window downward LINES lines; or near full screen if no ARG.
See `scroll-down-command' for details."
  (interactive "P")
  (with-selected-window (other-window-for-scrolling)
    (if (eq major-mode 'pdf-view-mode)
        (pdf-view-next-line-or-next-page (or lines 1)))))

(with-eval-after-load 'pdf-view
  (add-hook 'pdf-view-mode-hook (lambda ()
                                  (display-line-numbers-mode -1)))

  (global-set-key [remap scroll-other-window] 'pdf/scroll-other-window-up)
  (global-set-key  [remap scroll-other-window-down] 'pdf/scroll-other-window-down)
  (define-key pdf-view-mode-map "n" 'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map "p" 'pdf-view-previous-line-or-previous-page)
  (define-key pdf-view-mode-map "N" 'pdf-view-next-page-command)
  (define-key pdf-view-mode-map "P" 'pdf-view-previous-page-command)
  (require 'saveplace-pdf-view))

(with-eval-after-load 'pdf-history
  (define-key pdf-history-minor-mode-map "N" nil))

(provide 'init-pdf)
