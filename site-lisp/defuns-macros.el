(defmacro with-every-line (&rest forms)
  "Execute FORMS on each line following point to the end of file."
  (declare (indent 0))
  `(progn
     (beginning-of-line)
     (while (not (eobp))
       ,@forms
       (forward-line))))

(defmacro fix-reset-after-each (&rest forms)
  (declare (indent 0))
  `(progn
     ,@(apply 'append (mapcar (lambda (form) (list '(goto-char (point-min)) form)) forms))))

(defvar my-macro-names
  '(
    "fix-reset-after-each"
    "bind-keys"
    ))

(font-lock-add-keywords 'emacs-lisp-mode `((,(concat "(\\<"
                                                     (regexp-opt my-macro-names 'paren)
                                                     "\\>")
                                            1 font-lock-keyword-face)) 'append)
