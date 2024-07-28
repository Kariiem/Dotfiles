;; -*- lexical-binding: t; -*-


(defgroup hinfo nil
  "Major mode for happy generated info files"
  :group 'languages
  :prefix "hinfo-")



(defconst hinfo--section-regex
  (rx (+ "-")
      (char "\n")
      (group (: (+ alpha) (| (? ?-) (? space)) (+ alpha)))
      (char "\n")
      (+ "-")))

(defvar-local hinfo--sections nil)

(defun hinfo--section-start (name)
  (alist-get name hinfo--sections nil nil #'equal))

(defun hinfo--populate-sections ()
  (beginning-of-buffer)
  (save-excursion
    (while (re-search-forward hinfo--section-regex nil t)
      (push `(,(match-string 1) . ,(match-beginning 1))
            hinfo--sections)))
  hinfo--sections)

(defun hinfo--current-section ()
  "return current section in (`NAME' . `STARTPOS') format"
  (let ((current-position (point))
        (hs hinfo--sections))
    (while (and hs (< current-position (cdar hs)))
      (pop hs))
    (car hs)))

(defun hinfo--next-section (dir)
  "return next section in direction `DIR' in (`NAME' . `STARTPOS') format"
  (let* ((f (if (< 0 dir) 'reverse 'identity))
        (sections (funcall f (append hinfo--sections hinfo--sections)))
        (current-section (hinfo--current-section)))
    (or (cadr (member current-section sections))
        `("Grammar" . ,(hinfo--section-start "Grammar")))))

;;; exported functions
(defun hinfo-goto-section (name)
  (interactive (list (completing-read "Section: " hinfo--sections nil t)))
  (goto-char (hinfo--section-start name))
  (when (called-interactively-p)
    (recenter 1)))

(defun hinfo-goto-next-section ()
  (interactive)
  (goto-char (cdr (hinfo--next-section 1)))
  (recenter 1))

(defun hinfo-goto-prev-section ()
  (interactive)
  (goto-char (cdr (hinfo--next-section -1)))
  (recenter 1))



(defconst hinfo--state-regex
  (rx bol (group "State" (+ space) (group (+ digit)) eol)))

(defvar-local hinfo--states nil)

(defvar-local hinfo--states-count 0)

(defun hinfo--state-start (n)
  (alist-get n hinfo--states nil nil #'equal))

(defun hinfo--populate-states ()
  (save-excursion
    (hinfo-goto-section "States")
    ;;;(goto-char (hinfo--section-start "States"))
    (let ((search-end (cdr (hinfo--next-section 1))))
      (while (re-search-forward hinfo--state-regex search-end t)
        (push `(,(string-to-number (match-string 2)) . ,(match-beginning 1))
              hinfo--states))))
  (setq hinfo--states-count (length hinfo--states))
  hinfo--states)

(defun hinfo--current-state ()
  "return current state in (`NAME' . `STARTPOS') format"
  (let ((current-position (point))
        (hs hinfo--states))
    (while (and hs (< current-position (cdar hs)))
      (pop hs))
    (and (equal (car (hinfo--current-section)) "States") (car hs))))

(defun hinfo--next-state (dir)
  (let* ((current-state (car (hinfo--current-state)))
         (next-state (or (and current-state
                              (mod (+ current-state dir) hinfo--states-count))
                         0)))
    `(,next-state . ,(hinfo--state-start next-state))))

;;; exported functions
(defun hinfo-goto-state (n)
  (interactive (list (read-number "State: ")))
  (goto-char (hinfo--state-start n))
  (recenter 1))

(defun hinfo-goto-next-state ()
  (interactive)
  (goto-char (cdr (hinfo--next-state 1)))
  (recenter 1))

(defun hinfo-goto-prev-state ()
  (interactive)
  (goto-char (cdr (hinfo--next-state -1)))
  (recenter 1))


(defconst hinfo--rule-regex
  (rx (group (+ (in ?% ?_ alpha digit)))
      (+ space)
      "->"
      (+ nonl)
      ?\(
      (group (+ digit))
      ?\)
      eol))

(defvar-local hinfo--rules nil)

(defun hinfo--rule-start (n)
  (car (alist-get n hinfo--rules nil nil #'equal)))

(defun hinfo--populate-rules ()
  (save-excursion
    (hinfo-goto-section "Grammar")
    ;;;(goto-char (hinfo--section-start "Grammar"))
    (let ((search-end (cdr (hinfo--next-section 1))))
      (while (re-search-forward hinfo--rule-regex search-end t)
        (push `(,(string-to-number (match-string 2)) ,(match-beginning 1) ,(match-string 1))
              hinfo--rules)))
    hinfo--rules))

;;; exported functions
(defun hinfo-goto-rule (n)
  (interactive (list (read-number "Rule: ")))
  (goto-char (hinfo--rule-start n))
  (recenter 1))


(defconst hinfo--nonterminal-regex
  (rx (group (+ (in alnum ?% ?_)))
      (+ space)
      "rules"
      (group (: (+ (+ space) (+ numeric))))))

(defvar-local hinfo--nonterminals nil)

(defun hinfo--nonterminal-start (name)
  (alist-get name hinfo--nonterminals nil nil #'equal))

(defun hinfo--populate-nonterminals ()
  (save-excursion
    (hinfo-goto-section "Non-terminals")
    ;;;(goto-char (hinfo--section-start "Non-terminals"))
    (let ((search-end (cdr (hinfo--next-section 1))))
      (while (re-search-forward hinfo--nonterminal-regex search-end t)
        (push `(,(match-string 1) . ,(match-beginning 1))
              hinfo--nonterminals)))
    hinfo--nonterminals))

;;; exported functions
(defun hinfo-goto-nonterminal (name)
  (interactive (list (completing-read "NonTerminal: " hinfo--nonterminals)))
  (goto-char (hinfo--nonterminal-start name))
  (recenter 1))


(defconst hinfo--terminal-regex
  (rx (group (| (: ?' (+ (not ?')) ?')
                (+ (in alpha ?_))))
      (+ space)
      ?{))

(defvar-local hinfo--terminals nil)

(defun hinfo--terminal-start (name)
  (alist-get name hinfo--terminals nil nil #'equal))

(defun hinfo--populate-terminals ()
  (save-excursion
    (hinfo-goto-section "Terminals")
    ;;;(goto-char (hinfo--section-start "Terminals"))
    (let ((search-end (cdr (hinfo--next-section 1))))
      (while (re-search-forward hinfo--terminal-regex search-end t)
        (push `(,(match-string 1) . ,(match-beginning 1))
              hinfo--terminals)))
    hinfo--terminals))

;;; exported functions
(defun hinfo-goto-terminal (name)
  (interactive (list (completing-read "Terminal: " hinfo--terminals)))
  (goto-char (hinfo--terminal-start name))
  (recenter 1))



(defconst hinfo-keywords
  '("State" "rule" "shift" "reduce" "goto"))

(defconst hinfo-font-lock-keywords nil)

;;;###autoload
(defvar-keymap hinfo-mode-map
  "M-n" #'hinfo-goto-next-section
  "M-p" #'hinfo-goto-prev-section
  "N"   #'hinfo-goto-next-state
  "P"   #'hinfo-goto-prev-state
  "S"   #'hinfo-goto-section
  "t"   #'hinfo-goto-terminal
  "r"   #'hinfo-goto-rule
  "s"   #'hinfo-goto-state)

;;;###autoload
(define-derived-mode hinfo-mode prog-mode "hinfo"
  "Major mode for Happy generated info files"
  (hinfo--populate-sections)
  (hinfo--populate-rules)
  (hinfo--populate-terminals)
  (hinfo--populate-states)
  (read-only-mode 1))

(defvar hinfo-mode-hook nil)
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hinfo\\'" . hinfo-mode))
