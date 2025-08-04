;;; kotlin-decl-scan.el --- Declaration scanning module for Kotlin Mode -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'kotlin-mode)
(require 'syntax)
(require 'imenu)
(require 'subr-x)

(defgroup kotlin-decl-scan nil
  "Kotlin declaration scanning (`imenu' support)."
  :group 'kotlin
  :prefix "kotlin-decl-scan-")

(defcustom kotlin-decl-scan-bindings-as-variables nil
  "Whether to put top-level value bindings into a \"Variables\" category."
  :group 'kotlin-decl-scan
  :type 'boolean)

(defcustom kotlin-decl-scan-add-to-menubar t
  "Whether to add a \"Declarations\" menu entry to menu bar."
  :group 'kotlin-decl-scan
  :type 'boolean)

(defcustom kotlin-decl-scan-sort-imenu t
  "Whether to sort the candidates in imenu."
  :group 'kotlin-decl-scan
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General declaration scanning functions.

(defvar kotlin-ds-start-keywords-re
  (concat "\\(\\<"
          "fun\\|"
          "object\\|data\\|class\\|interface\\|"
          "import\\|typealias"
          "\\)\\>")
  "Keywords that may start a declaration.")

(defvar kotlin-ds-syntax-table
  (let ((table (copy-syntax-table kotlin-mode-syntax-table)))
    (modify-syntax-entry ?_  "w" table)
    table)
  "Syntax table used for Kotlin declaration scanning.")

(defun kotlin-ds-get-variable (prefix)
  "Return variable involved in value binding or type signature.
Assumes point is looking at the regexp PREFIX followed by the
start of a declaration (perhaps in the middle of a series of
declarations concerning a single variable).  Otherwise return nil.
Point is not changed."
  ;; I think I can now handle all declarations bar those with comments
  ;; nested before the second lexeme.
  (save-excursion
    (with-syntax-table kotlin-ds-syntax-table
      (if (looking-at prefix) (goto-char (match-end 0)))
      ;; Keyword.
      (if (looking-at kotlin-ds-start-keywords-re)
          nil
        (or ;; Parenthesized symbolic variable.
         (and (looking-at "(\\(\\s_+\\))") (match-string-no-properties 1))
         ;; General case.
         (if (looking-at
              (if (eq ?\( (char-after))
                  ;; Skip paranthesised expression.
                  (progn
                    (forward-sexp)
                    ;; Repeating this code and avoiding moving point if
                    ;; possible speeds things up.
                    "\\(\\'\\)?\\s-*\\(\\s_+\\|`\\(\\sw+\\)`\\)")
                "\\(\\sw+\\)?\\s-*\\(\\s_+\\|`\\(\\sw+\\)`\\)"))
             (let ((match2 (match-string-no-properties 2)))
               ;; Weed out `::', `∷',`=' and `|' from potential infix
               ;; symbolic variable.
               (if (member match2 '("::" "∷" "=" "|"))
                   ;; Variable identifier.
                   (match-string-no-properties 1)
                 (if (eq (aref match2 0) ?\`)
                     ;; Infix variable identifier.
                     (match-string-no-properties 3)
                   ;; Infix symbolic variable.
                   match2))))
         ;; Variable identifier.
         (and (looking-at "\\sw+") (match-string-no-properties 0)))))))

(defun kotlin-ds-move-to-start-regexp (inc regexp)
  "Move to beginning of line that succeeds/precedes (INC = 1/-1)
current line that starts with REGEXP and is not in `font-lock-comment-face'."
  ;; Making this defsubst instead of defun appears to have little or
  ;; no effect on efficiency.  It is probably not called enough to do
  ;; so.
  (while (and (= (forward-line inc) 0)
              (or (not (looking-at regexp))
                  (eq (get-text-property (point) 'face)
                      'font-lock-comment-face)))))

(defun kotlin-ds-move-to-start-regexp-skipping-comments (inc regexp)
  "Like kotlin-ds-move-to-start-regexp, but uses syntax-ppss to
  skip comments"
  (let (p)
    (cl-loop
     do (setq p (point))
     (kotlin-ds-move-to-start-regexp inc regexp)
     while (and (nth 4 (syntax-ppss))
                (/= p (point))))))

(defvar kotlin-ds-start-decl-re "\\(\\sw\\|(\\)"
  "The regexp that starts a Kotlin declaration.")

(defun kotlin-ds-whitespace-p (char)
  "Test if CHAR is a whitespace character."
  ;; the nil is a bob/eob test
  (member char '(nil ?\t ?\n ?\ )))

(defun kotlin-ds-move-to-decl (direction fix)
  "General function for moving to the start of a declaration,
either forwards or backwards from point, with normal or with Bird-style
literate scripts.  If DIRECTION is t, then forward, else backward.
Returns point if point is left at the start of a
declaration, and nil otherwise, ie. because point is at the beginning
or end of the buffer and no declaration starts there.  If FIX is t,
then point does not move if already at the start of a declaration."
  ;; As `kotlin-ds-get-variable' cannot separate an infix variable
  ;; identifier out of a value binding with non-alphanumeric first
  ;; argument, this function will treat such value bindings as
  ;; separate from the declarations surrounding it.
  (let ( ;; The variable typed or bound in the current series of
        ;; declarations.
        name
        ;; The variable typed or bound in the new declaration.
        newname
        ;; Hack to solve hard problem for Bird-style literate scripts
        ;; that start with a declaration.  We are in the abyss if
        ;; point is before start of this declaration.
        abyss
        (line-prefix "")
        ;; The regexp to match for the start of a declaration.
        (start-decl-re kotlin-ds-start-decl-re)
        (increment (if direction 1 -1))
        (bound (if direction (point-max) (point-min))))
    ;; Change syntax table.
    (with-syntax-table kotlin-ds-syntax-table
      ;; move to beginning of line that starts the "current
      ;; declaration" (dependent on DIRECTION and FIX), and then get
      ;; the variable typed or bound by this declaration, if any.
      (let ( ;; Where point was at call of function.
            (here (point))
            ;; Where the declaration on this line (if any) starts.
            (start (progn
                     (beginning-of-line)
                     ;; Checking the face to ensure a declaration starts
                     ;; here seems to be the only addition to make this
                     ;; module support LaTeX-style literate scripts.
                     (if (and (looking-at start-decl-re)
                              (not (elt (syntax-ppss) 4)))
                         (match-beginning 1)))))
        (if (and start
                 ;; This complicated boolean determines whether we
                 ;; should include the declaration that starts on the
                 ;; current line as the "current declaration" or not.
                 (or (and (or (and direction (not fix))
                              (and (not direction) fix))
                          (>= here start))
                     (and (or (and direction fix)
                              (and (not direction) (not fix)))
                          (> here start))))
            ;; If so, we are already at start of the current line, so
            ;; do nothing.
            ()
          ;; If point was before start of a declaration on the first
          ;; line of the buffer (possible for Bird-style literate
          ;; scripts) then we are in the abyss.
          (if (and start (bobp))
              (setq abyss t)
            ;; Otherwise we move to the start of the first declaration
            ;; on a line preceding the current one, skipping comments
            (kotlin-ds-move-to-start-regexp-skipping-comments -1 start-decl-re))))
      ;; If we are in the abyss, position and return as appropriate.
      (if abyss
          (if (not direction)
              nil
            (re-search-forward (concat "\\=" line-prefix) nil t)
            (point))
        ;; Get the variable typed or bound by this declaration, if any.
        (setq name (kotlin-ds-get-variable line-prefix))
        (if (not name)
            ;; If no such variable, stop at the start of this
            ;; declaration if moving backward, or move to the next
            ;; declaration if moving forward.
            (if direction
                (kotlin-ds-move-to-start-regexp-skipping-comments 1 start-decl-re))
          ;; If there is a variable, find the first
          ;; succeeding/preceding declaration that does not type or
          ;; bind it.  Check for reaching start/end of buffer and
          ;; comments.
          (kotlin-ds-move-to-start-regexp-skipping-comments increment start-decl-re)
          (while (and (/= (point) bound)
                      (and (setq newname (kotlin-ds-get-variable line-prefix))
                           (string= name newname)))
            (setq name newname)
            (kotlin-ds-move-to-start-regexp-skipping-comments increment start-decl-re))
          ;; If we are going backward, and have either reached a new
          ;; declaration or the beginning of a buffer that does not
          ;; start with a declaration, move forward to start of next
          ;; declaration (which must exist).  Otherwise, we are done.
          (if (and (not direction)
                   (or (and (looking-at start-decl-re)
                            (not (string= name
                                          ;; Note we must not use
                                          ;; newname here as this may
                                          ;; not have been set if we
                                          ;; have reached the beginning
                                          ;; of the buffer.
                                          (kotlin-ds-get-variable
                                           line-prefix))))
                       (and (not (looking-at start-decl-re))
                            (bobp))))
              (kotlin-ds-move-to-start-regexp-skipping-comments 1 start-decl-re)))
        ;; Store whether we are at the start of a declaration or not.
        ;; Used to calculate final result.
        (let ((at-start-decl (looking-at start-decl-re)))
          ;; If we are at the beginning of a line, move over
          ;; line-prefix, if present at point.
          (if (bolp)
              (re-search-forward (concat "\\=" line-prefix) (point-max) t))
          ;; Return point if at the start of a declaration and nil
          ;; otherwise.
          (if at-start-decl (point) nil))))))

(defun kotlin-ds-backward-decl ()
  "Move backward to the first character that starts a top-level declaration.
A series of declarations concerning one variable is treated as one
declaration by this function.  So, if point is within a top-level
declaration then move it to the start of that declaration.  If point
is already at the start of a top-level declaration, then move it to
the start of the preceding declaration.  Returns point if point is
left at the start of a declaration, and nil otherwise, because
point is at the beginning of the buffer and no declaration starts
there."
  (interactive)
  (kotlin-ds-move-to-decl nil nil))

(defun kotlin-ds-comment-p
    (&optional
     pt)
  "Test if the cursor is on whitespace or a comment.

`PT' defaults to `(point)'"
  ;; ensure result is `t' or `nil' instead of just truthy
  (if (or
       ;; is cursor on whitespace
       (kotlin-ds-whitespace-p (following-char))
       ;; http://emacs.stackexchange.com/questions/14269/how-to-detect-if-the-point-is-within-a-comment-area
       ;; is cursor at begging, inside, or end of comment
       (let ((fontfaces (get-text-property (or pt
                                               (point)) 'face)))
         (when (not (listp fontfaces))
           (setf fontfaces (list fontfaces)))
         (delq nil (mapcar
                    #'(lambda (f)
                        (member f '(font-lock-comment-face
                                    font-lock-doc-face
                                    font-lock-comment-delimiter-face)))
                    fontfaces))))
      t
    nil))

(defun kotlin-ds-line-commented-p ()
  "Tests if all characters from `point' to `end-of-line' pass
`kotlin-ds-comment-p'"
  (let ((r t))
    (while (and r (not (eolp)))
      (if (not (kotlin-ds-comment-p))
          (setq r nil))
      (forward-char))
    r))

(defun kotlin-ds-forward-decl ()
  "Move forward to the first character that starts a top-level
declaration.  As `kotlin-ds-backward-decl' but forward."
  (interactive)
  (let ((p (point)) b e empty was-at-bob)
    ;; Go back to beginning of defun, then go to beginning of next
    (kotlin-ds-move-to-decl nil nil)
    (setq b (point))
    (kotlin-ds-move-to-decl t nil nil)
    (setq e (point))
    ;; tests if line is empty
    (setq empty (and (<= (point) p)
                     (not (eolp))))
    (setq was-at-bob (and (= (point-min) b)
                          (= b p)
                          (< p e)))
    ;; this conditional allows for when empty lines at end, first
    ;; `C-M-e' will go to end of defun, next will go to end of file.
    (when (or was-at-bob
              empty)
      (if (or (and was-at-bob
                   (= ?\n
                      (save-excursion
                        (goto-char (point-min))
                        (following-char))))
              empty)
          (kotlin-ds-move-to-decl t nil nil))
      ;; Then go back to end of current
      (forward-line -1)
      (while (and (kotlin-ds-line-commented-p)
                  ;; prevent infinite loop
                  (not (bobp)))
        (forward-line -1))
      (forward-line 1)))
  (point))

(defun kotlin-ds-generic-find-next-decl ()
  "Find the name, position and type of the declaration at or after point.
Return ((NAME . (START-POSITION . NAME-POSITION)) . TYPE)
if one exists and nil otherwise.  The start-position is at the start
of the declaration, and the name-position is at the start of the name
of the declaration.  The name is a string, the positions are buffer
positions and the type is one of the symbols \"variable\", \"datatype\",
\"class\", \"import\" and \"instance\"."
  (let ( ;; The name, type and name-position of the declaration to
        ;; return.
        name
        type
        name-pos
        ;; Buffer positions marking the start and end of the space
        ;; containing a declaration.
        start
        end)
    ;; Change to declaration scanning syntax.
    (with-syntax-table kotlin-ds-syntax-table
      ;; Stop when we are at the end of the buffer or when a valid
      ;; declaration is grabbed.
      (while (not (or (eobp) name))
        ;; Move forward to next declaration at or after point.
        (kotlin-ds-move-to-decl t t)
        ;; Start and end of search space is currently just the starting
        ;; line of the declaration.
        (setq start (point)
              end   (line-end-position))
        (cond
         ;; If the start of the top-level declaration does not begin
         ;; with a starting keyword, then (if legal) must be a type
         ;; signature or value binding, and the variable concerned is
         ;; grabbed.
         ((not (looking-at kotlin-ds-start-keywords-re))
          (setq name (kotlin-ds-get-variable ""))
          (if name
              (progn
                (setq type 'variable)
                (re-search-forward (regexp-quote name) end t)
                (setq name-pos (match-beginning 0)))))
         ;; companion objects
         ((re-search-forward "\\=\\(object\\)\\>" end t)
          (if (looking-at "[ \t]*\\(\\sw+\\)")
              (progn
                (setq name (match-string-no-properties 1))
                (setq name-pos (match-beginning 1))
                (setq type 'object))))
         ;; functions
         ((re-search-forward "\\=\\(fun\\)\\>" end t)
          (if (looking-at "[ \t]*\\(\\sw+\\)")
              (progn
                (setq name (match-string-no-properties 1))
                (setq name-pos (match-beginning 1))
                (setq type 'fun))))
         ;; User-defined type/class declaration.
         ((re-search-forward "\\=\\(data[ \t]+\\)?\\(class\\|typealias\\)\\>" end t)
          (if (looking-at "[ \t]*\\(\\sw+\\)")
              (progn
                (setq name (match-string-no-properties 1))
                (setq name-pos (match-beginning 1))
                (setq type 'class))))
         ;; Interface declaration.
         ((re-search-forward "\\=interface\\>" end t)
          (if (looking-at "[ \t]*\\(\\sw+\\)")
              (progn
                (setq name (match-string-no-properties 1))
                (setq name-pos (match-beginning 1))
                (setq type 'interface))))
         ;; Import declaration.
         ((looking-at "import[ \t]+\\(\\(?:\\sw\\|.\\)+\\)")
          (setq name (match-string-no-properties 1))
          (setq name-pos (match-beginning 1))
          (setq type 'import)))
        ;; Move past start of current declaration.
        (goto-char end))
      ;; If we have a valid declaration then return it, otherwise return
      ;; nil.
      (if name
          (cons (cons name (cons (copy-marker start t) (copy-marker name-pos t)))
                type)
        nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declaration scanning via `imenu'.

;;;###autoload
(defun kotlin-ds-create-imenu-index ()
  "Function for finding `imenu' declarations in Kotlin mode.
Finds all declarations (classes, variables, imports, instances and
datatypes) in a Kotlin file for the `imenu' package."
  ;; Each list has elements of the form `(INDEX-NAME . INDEX-POSITION)'.
  ;; These lists are nested using `(INDEX-TITLE . INDEX-ALIST)'.
  (let* ((index-alist '())
         (imenu (make-hash-table :test 'equal))
         ;; The result we wish to return.
         result)
    (goto-char (point-min))
    ;; Loop forwards from the beginning of the buffer through the
    ;; starts of the top-level declarations.
    (while (< (point) (point-max))
      ;; Grab the next declaration.
      (setq result (kotlin-ds-generic-find-next-decl))
      (if result
          ;; If valid, extract the components of the result.
          (let* ((name-posns (car result))
                 (name (car name-posns))
                 (posns (cdr name-posns))
                 (start-pos (car posns))
                 (type (cdr result)))
            (puthash type
                     (cons (cons name start-pos) (gethash type imenu '()))
                     imenu))))
    ;; Now sort all the lists, label them, and place them in one list.
    (dolist (type '((fun . nil)
                    (object . "Objects")
                    (class . "Classes")
                    (interface . "Interfaces")
                    (import   . "Imports")))
      (when-let ((curr-alist (gethash (car type) imenu)))
        (if (cdr type)
            (push (cons (cdr type)
                        (if kotlin-decl-scan-sort-imenu
                            (sort curr-alist 'kotlin-ds-imenu-label-cmp)
                          (reverse curr-alist)))
                  index-alist)
          (setq index-alist (append index-alist
                                    (if kotlin-decl-scan-sort-imenu
                                        (sort curr-alist 'kotlin-ds-imenu-label-cmp)
                                      (reverse curr-alist)))))))
    (when-let ((var-alist (gethash 'variable imenu)))
      (if kotlin-decl-scan-bindings-as-variables
          (push (cons "Variables"
                      (if kotlin-decl-scan-sort-imenu
                          (sort var-alist 'kotlin-ds-imenu-label-cmp)
                        (reverse var-alist)))
                index-alist)
        (setq index-alist (append index-alist
                                  (if kotlin-decl-scan-sort-imenu
                                      (sort var-alist 'kotlin-ds-imenu-label-cmp)
                                    (reverse var-alist))))))
    ;; Return the alist.
    index-alist))

(defun kotlin-ds-imenu-label-cmp (el1 el2)
  "Predicate to compare labels in lists from `kotlin-ds-create-imenu-index'."
  (string< (car el1) (car el2)))

(defun kotlin-ds-imenu ()
  "Install `imenu' for Kotlin scripts."
  (setq imenu-create-index-function 'kotlin-ds-create-imenu-index)
  (when kotlin-decl-scan-add-to-menubar
    (imenu-add-to-menubar "Declarations")))

;; The main functions to turn on declaration scanning.
;;;###autoload
(define-minor-mode kotlin-decl-scan-mode
  "Toggle Kotlin declaration scanning minor mode on or off.
With a prefix argument ARG, enable minor mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.

See also info node `(kotlin-mode)kotlin-decl-scan-mode' for
more details about this minor mode.

Top-level declarations are scanned and listed in the menu item
\"Declarations\" (if enabled via option
`kotlin-decl-scan-add-to-menubar').  Selecting an item from this
menu will take point to the start of the declaration.

\\[beginning-of-defun] and \\[end-of-defun] move forward and backward to the start of a declaration.

This may link with `kotlin-doc-mode'.

For non-literate and LaTeX-style literate scripts, we assume the
common convention that top-level declarations start at the first
column.  For Bird-style literate scripts, we assume the common
convention that top-level declarations start at the third column,
ie. after \"> \".

Anything in `font-lock-comment-face' is not considered for a
declaration.  Therefore, using Kotlin font locking with comments
coloured in `font-lock-comment-face' improves declaration scanning.

Literate Kotlin scripts are supported: If the value of
`kotlin-literate' (set automatically by `kotlin-literate-mode')
is `bird', a Bird-style literate script is assumed.  If it is nil
or `tex', a non-literate or LaTeX-style literate script is
assumed, respectively.

Invokes `kotlin-decl-scan-mode-hook' on activation."
  :group 'kotlin-decl-scan

  (kill-local-variable 'beginning-of-defun-function)
  (kill-local-variable 'end-of-defun-function)
  (kill-local-variable 'imenu-create-index-function)
  (unless kotlin-decl-scan-mode
    ;; How can we cleanly remove the "Declarations" menu?
    (when kotlin-decl-scan-add-to-menubar
      (local-set-key [menu-bar index] nil)))

  (when kotlin-decl-scan-mode
    (setq-local beginning-of-defun-function 'kotlin-ds-backward-decl)
    (setq-local end-of-defun-function 'kotlin-ds-forward-decl)
    (kotlin-ds-imenu)))


;; Provide ourselves:

(provide 'kotlin-imenu)

;;; kotlin-imenu.el ends here
