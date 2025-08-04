(install-pkgs go-translate)

(with-eval-after-load 'go-translate
  (setq gt-preset-translators
        `((ts-1 . ,(gt-translator
                    :taker (gt-taker :langs '(en ar) :text 'word)
                    :engines (gt-google-engine)
                    :render (gt-buffer-render)))
          (ts-2 . ,(gt-translator
                    :taker (gt-taker :langs '(en ar) :text 'sentence)
                    :engines (gt-google-engine)
                    :render (gt-buffer-render))))))
(global-set-key (key-parse "C-c C-t") #'gt-do-translate)
(provide 'init-translate)
