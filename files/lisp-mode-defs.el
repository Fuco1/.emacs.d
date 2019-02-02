(require 'thingatpt)

(bind-keys :map emacs-lisp-mode-map
  ("M-'" . my-elisp-goto-definition)
  ("C-M-;" . clippy-describe-function)
  ("C-c C-c" . overseer-test-run-test)
  ("C-c t" . my-elisp-run-buttercup)
  ("C-c C-t" . ft-find-test-or-source)
  ("C-. ." . my-describe-thing-in-buffer))
(bind-key "C-x C-d"
          (defhydra hydra-elisp-refactor (:color blue)
            "Refactor"
            ("l" my-extract-to-let "extract to let")
            ("m" my-merge-let-forms "merge let forms")
            ("c" my-lisp-if-to-cond "if => cond")
            ("i" my-lisp-cond-to-if "cond => if")
            ("d" my-lisp-flip-or-and "and <=> or")
            ("n" my-lisp-remove-excess-not "not . not => id"))
          emacs-lisp-mode-map)
(unbind-key "C-x C-a" emacs-lisp-mode-map)

(defun my-elisp-goto-definition ()
  "Jump to definition of function at point."
  (interactive)
  (find-function (function-called-at-point)))

(defvar my-macro-names
  '(
    "my-with-every-line"
    "my-with-each-line"
    "my-fix-reset-after-each"
    "bind-keys"
    ))

(font-lock-add-keywords 'emacs-lisp-mode
                        `(("\\(?:^\\| \\)\\('\\sw\\(?:\\sw\\|\\s_\\)*\\)"
                           1 'font-lock-constant-face)
                          ("\\(?:^\\| \\)\\(#'\\sw\\(?:\\sw\\|\\s_\\)*\\)"
                           1 'font-lock-type-face)
                          ("(\\(defhydra\\)\\>[[:blank:]]+\\(.*?\\)\\([[:blank:]]\\|$\\)"
                           (1 font-lock-keyword-face)
                           (2 font-lock-type-face))
                          (,(concat "(\\<"
                                    (regexp-opt my-macro-names 'paren)
                                    "\\>")
                           1 font-lock-keyword-face))
                        'append)

(defvar my-emacs-lisp-open-line-list '(
                                       if
                                          when
                                          unless
                                        defun
                                        defmacro
                                        defvar
                                        defcustom
                                        let
                                        let*
                                        progn
                                        )
  "Forms after which we should automatically place closing paren
on extra line.")

(defun my-emacs-lisp-open-line (&optional arg)
  "Opens a new line if the point is at the closing parens of
function on `my-emacs-lisp-open-line-list'."
  (ignore-errors
    (when (and (save-excursion
                 (forward-char)
                 (backward-sexp)
                 (when (listp (sexp-at-point))
                   (memq (car (sexp-at-point)) my-emacs-lisp-open-line-list)))
               (eq (following-char) ?\)))
      (newline)
      (indent-according-to-mode)
      (forward-line -1)
      (indent-according-to-mode))))

(defun my-describe-thing-in-buffer ()
  "Display the full documentation of FUNCTION (a symbol) in the help buffer."
  (interactive)
  (let ((function (function-called-at-point))
        (variable (variable-at-point)))
    (cond
     ((not (numberp variable)) (describe-variable variable))
     (function (describe-function function)))))

(defun my-backward-down-list (&optional n)
  (down-list (- (prefix-numeric-value n))))

(defun my-wrap-region (beg end)
  (goto-char end)
  (insert ")")
  (goto-char beg)
  (insert "("))

(defun my-goto-dominating-form (what)
  "Find dominating form starting with WHAT.

If WHAT is a list, test for any symbol in WHAT.

Return element of WHAT which matched or nil if no dominating form
starting with any of the specified symbols exists."
  (unless (listp what) (setq what (list what)))
  (unless (save-excursion
            (when (ignore-errors (down-list))
              (memq (symbol-at-point) what)))
    (catch 'done
      (while (> (car (syntax-ppss)) 0)
        (ignore-errors
          (backward-up-list)
          (save-excursion
            (down-list)
            (-when-let (match (memq (symbol-at-point) what))
              (throw 'done (car match)))))))))

(defun my-extract-to-let (name &optional arg)
  "Extract the form at point into a variable called NAME placed
in a let form ARG levels up.

If ARG is \\[universal-argument] place the variable into the most
inner let form point is inside of."
  (interactive "sName of variable: \nP")
  (let ((new-vform (sp-get (sp--next-thing-selection) (delete-and-extract-region :beg-prf :end)))
        (raw (sp--raw-argument-p arg))
        (arg (prefix-numeric-value arg)))
    (save-excursion
      (cond
       (raw
        (my-goto-dominating-form '(let let*))
        (progn
          (down-list)
          (forward-sexp 2)
          (my-backward-down-list)
          (insert "\n(" name " " new-vform ")")
          (indent-according-to-mode)))
       (t
        (backward-up-list arg)
        (if (> arg 0)
            (sp-get (sp--next-thing-selection) (my-wrap-region :beg-prf :end))
          (insert "()")
          (backward-char))
        (insert "let ((" name " " new-vform "))\n")
        (backward-up-list)
        (indent-sexp))))
    (if (> arg 0)
        (insert name)
      (forward-sexp)
      (my-backward-down-list)
      (save-excursion
        (insert "\n")
        (backward-up-list)
        (indent-sexp)))))

(defun my-merge-let-forms ()
  "Merge the most inner let form into the next outer one."
  (interactive)
  (save-excursion
    (my-goto-dominating-form '(let let*))
    (down-list)
    (my-next-sexp)
    (let ((var-list (sp-get (sp--next-thing-selection) (delete-and-extract-region :beg-prf :end))))
      (sp-splice-sexp-killing-backward 1)
      (my-goto-dominating-form '(let let*))
      (down-list)
      (forward-sexp 2)
      (my-backward-down-list)
      (insert "\n" var-list)
      (my-backward-down-list)
      (sp-unwrap-sexp)
      (backward-up-list 2)
      (indent-sexp))))

(defun my-next-sexp ()
  "Move to the beginning of sexp after the current one."
  (condition-case nil
      (progn
        (forward-sexp 2)
        (backward-sexp)
        t)
    (error nil)))

(defun my-sexp-at-point ()
  "Extract sexp at point as text."
  (sp-get (sp--next-thing-selection) (buffer-substring-no-properties :beg-prf :end)))

(defun my-lisp-if-to-cond ()
  "Transform an `if' form to a `cond' form."
  (interactive)
  (save-excursion
    (my-goto-dominating-form 'if)
    (let ((p (point)))
      (down-list)
      (my-next-sexp)
      (let ((condition (my-sexp-at-point))
            (body1 (progn (my-next-sexp) (my-sexp-at-point)))
            (body2 (progn (my-next-sexp) (my-sexp-at-point))))
        (goto-char p)
        (-let [(beg . end) (bounds-of-thing-at-point 'sexp)] (delete-region beg end))
        (insert
         (format "(cond
                   (%s
                     %s)
                   (t
                     %s))" condition body1 body2))
        (goto-char p)
        (indent-sexp)))))

(defun my-lisp-cond-to-if ()
  "Transform a `cond' form into an `if' form."
  (interactive)
  (save-excursion
    (my-goto-dominating-form 'cond)
    (let ((p (point))
          (forms nil))
      (down-list)
      (my-next-sexp)
      (while (progn
               (push (cons
                      (sexp-at-point)
                      (my-sexp-at-point))
                     forms)
               (my-next-sexp)))
      (setq forms (nreverse forms))
      (goto-char p)
      (-let (((beg . end) (bounds-of-thing-at-point 'sexp))) (delete-region beg end))
      (let ((last-is-t (eq (caar (-last-item forms)) t)))
        (--each (if last-is-t (-butlast forms) forms)
          (insert (format "(if %s\n" (substring (cdr it) 1 -1))))
        (if last-is-t
            (progn
              (insert (format "%s" (cdr (-last-item forms))))
              (backward-sexp)
              (down-list)
              (forward-sexp)
              (sp-splice-sexp-killing-backward 1)
              (forward-sexp)
              (insert (make-string (1- (length forms)) 41)))
          (delete-char -1)
          (insert (format "%s" (make-string (length forms) 41)))))
      (backward-sexp)
      (indent-sexp))))

(defun my-lisp-demorganify (to)
  "Apply De Morgan's law.

TO is either 'and or 'or."
  (let* ((sexp (sexp-at-point))
         (body (-map (lambda (x)
                       (if (and (listp x)
                                (eq (car x) 'not))
                           (cadr x)
                         `(not ,x))) (cdr sexp))))
    `(not (,to ,@body))))

(defun my-lisp-remove-excess-not ()
  "Replace (not (not form)) with form."
  (interactive)
  (save-excursion
    (catch 'done
      (while (my-goto-dominating-form 'not)
        (let* ((sexp (sexp-at-point))
               (kill-ring kill-ring))
          (when (and (listp sexp)
                     (eq (car sexp) 'not)
                     (listp (cadr sexp))
                     (eq (caadr sexp) 'not))
            (kill-sexp)
            (pp (cadr (cadr sexp)) (current-buffer))
            (throw 'done t)))))))

(defun my-lisp-flip-or-and ()
  "Transform `or' into an equivalent `and' and vice versa."
  (interactive)
  (save-excursion
    (my-goto-dominating-form '(or and))
    (let* ((type (save-excursion
                   (down-list)
                   (symbol-at-point)))
           (replacement (if (eq type 'or)
                            (my-lisp-demorganify 'and)
                          (my-lisp-demorganify 'or)))
           (kill-ring kill-ring ))
      (kill-sexp)
      (pp replacement (current-buffer))
      (my-lisp-remove-excess-not))))

(defun my-elisp-run-buttercup ()
  "Run buttercup tests of current project (determined by Cask)."
  (interactive)
  (when (buffer-file-name)
    (-when-let (root-dir (locate-dominating-file (buffer-file-name) "Cask"))
      (with-temp-buffer
        (cd root-dir)
        (compile "cask exec buttercup -L . -L tests")))))

(provide 'my-lisp-mode-defs)
;;; lisp-mode-defs.el ends here
