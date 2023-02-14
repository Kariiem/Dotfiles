(defmacro bind-key (key-name command &optional keymap predicate)

  (let ((namevar (make-symbol "name"))
        (keyvar (make-symbol "key"))
        (kmapvar (make-symbol "kmap"))
        (kdescvar (make-symbol "kdesc"))
        (bindingvar (make-symbol "binding")))
    `(let* ((,namevar ,key-name)
            (,keyvar ,(if (stringp key-name) (read-kbd-macro key-name)
                        `(if (vectorp ,namevar) ,namevar
                           (read-kbd-macro ,namevar))))
            (,kmapvar (or (if (and ,keymap (symbolp ,keymap))
                              (symbol-value ,keymap) ,keymap)
                          global-map))
            (,kdescvar (cons (if (stringp ,namevar) ,namevar
                               (key-description ,namevar))
                             (if (symbolp ,keymap) ,keymap (quote ,keymap))))
            (,bindingvar (lookup-key ,kmapvar ,keyvar)))
       (let ((entry (assoc ,kdescvar personal-keybindings))
             (details (list ,command
                            (unless (numberp ,bindingvar)
                              ,bindingvar))))
         (if entry
             (setcdr entry details)
           (add-to-list 'personal-keybindings (cons ,kdescvar details))))
       ,(if predicate
            `(define-key ,kmapvar ,keyvar
               '(menu-item "" nil :filter (lambda (&optional _)
                                            (when ,predicate
                                              ,command))))
          `(define-key ,kmapvar ,keyvar ,command)))))
(macroexpand '(bind-key KEYNAME COMMAND))
