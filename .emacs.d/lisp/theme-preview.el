;; -*- lexical-binding: t -*-

(defun load-theme-completion-function (string predicate flag)
  "Completion function for `load-theme'.
STRING is the theme name prefix to complete.
PREDICATE filters candidates, or nil for no filtering.
FLAG specifies the completion operation type."
  (let ((themes (mapcar #'symbol-name (custom-available-themes))))
    (pcase flag
      ('nil
       (try-completion string themes predicate))
      ('t
       (all-completions string themes predicate))
      ('lambda
        (test-completion string themes predicate))
      (`(boundaries . ,suffix)
       (completion-boundaries string themes predicate suffix))
      ('metadata
       '(metadata (category . theme)))
      (_
       nil))))

(defvar theme-preview--load-theme-advice ;; defvar + lambda instead of defun, to avoid introducing a new named command (the spec is interactive, so ...)
  (lambda (orig-fun &rest args)
    (interactive
     (list (intern-soft (completing-read "Load custom theme: " ;; the intern call is need for load-theme original behaviour
                                    #'load-theme-completion-function))))
    (apply orig-fun args)))

(defvar theme-preview--original-themes nil)

(define-minor-mode theme-preview-mode
  "Preview themes as you cycle through them in Vertico."
  :global t
  (if theme-preview-mode
      (progn
        (advice-add 'load-theme       :around  theme-preview--load-theme-advice)
        (advice-add 'vertico--update  :after  #'theme-preview--on-move))
    (advice-remove 'load-theme                 theme-preview--load-theme-advice)
    (advice-remove 'vertico-update            #'theme-preview--on-move)))

(defun theme-preview--on-move (&rest _)
  (when (eq (completion-metadata-get (completion-metadata (minibuffer-contents)
                                                          minibuffer-completion-table
                                                          minibuffer-completion-predicate)
             'category)
            'theme)
    (unless theme-preview--original-themes
      (setq theme-preview--original-themes (or custom-enabled-themes '(user))))
    (let ((theme (intern-soft (vertico--candidate))))
      (message nil)
      (unless (equal custom-enabled-themes (list theme))
        (condition-case err
            (progn
              (mapc #'disable-theme custom-enabled-themes)
              (load-theme theme :no-confirm :no-enable)
              (enable-theme theme))
          (error (message "theme-preview-enable: %s" (error-message-string err))))))))

(defun theme-preview--restore ()
  (when theme-preview--original-themes
    (condition-case err
        (progn
          (mapc #'disable-theme custom-enabled-themes)
          (mapc #'enable-theme theme-preview--original-themes))
      (error (message "theme-preview-restore: %s" (error-message-string err))))
    (setq theme-preview--original-themes nil)))


(add-hook 'minibuffer-exit-hook #'theme-preview--restore t)

(provide 'theme-preview)
