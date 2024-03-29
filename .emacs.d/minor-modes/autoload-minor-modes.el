;;; autoload-minor-modes.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:



;;; Generated autoloads from nomouse.el

(autoload 'nomouse-mode "nomouse" "\
Enable/Disable Mouse bindings in the current buffer.

This is a minor mode.  If called interactively, toggle the
`nomouse mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `nomouse-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{nomouse-mode-map}

(fn &optional ARG)" t)
(put 'global-nomouse-mode 'globalized-minor-mode t)
(defvar global-nomouse-mode nil "\
Non-nil if Global Nomouse mode is enabled.
See the `global-nomouse-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-nomouse-mode'.")
(custom-autoload 'global-nomouse-mode "nomouse" nil)
(autoload 'global-nomouse-mode "nomouse" "\
Toggle Nomouse mode in all buffers.
With prefix ARG, enable Global Nomouse mode if ARG is positive; otherwise,
disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Nomouse mode is enabled in all buffers where `nomouse--turn-on' would do it.

See `nomouse-mode' for more information on Nomouse mode.

(fn &optional ARG)" t)
(register-definition-prefixes "nomouse" '("nomouse-"))


;;; Generated autoloads from todowords.el

(autoload 'todowords-mode "todowords" "\
Enable/Disable Mouse bindings in the current buffer.

This is a minor mode.  If called interactively, toggle the
`todowords mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `todowords-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(put 'global-todowords-mode 'globalized-minor-mode t)
(defvar global-todowords-mode nil "\
Non-nil if Global Todowords mode is enabled.
See the `global-todowords-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-todowords-mode'.")
(custom-autoload 'global-todowords-mode "todowords" nil)
(autoload 'global-todowords-mode "todowords" "\
Toggle Todowords mode in all buffers.
With prefix ARG, enable Global Todowords mode if ARG is positive; otherwise,
disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Todowords mode is enabled in all buffers where `todowords--turn-on' would do
it.

See `todowords-mode' for more information on Todowords mode.

(fn &optional ARG)" t)
(register-definition-prefixes "todowords" '("todowords-"))


;;; End of scraped data

(provide 'autoload-minor-modes)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; autoload-minor-modes.el ends here
