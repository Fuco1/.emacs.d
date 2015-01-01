(use-package lisp-mode
  :defer t
  :init
  (progn
    (defun my-emacs-lisp-init ()
      (bind-keys :map emacs-lisp-mode-map
        ("<return>" . my-emacs-lisp-open-line)
        ("C-M-;" . clippy-describe-function)
        ("C-. ." . my-describe-thing-in-buffer)
        ("C-x C-d l" . my-extract-to-let)
        ("C-x C-d m" . my-merge-let-forms)
        ("C-x C-d c" . my-lisp-condify-if))

      (set-input-method "english-prog")
      (eldoc-mode 1)
      (letcheck-mode t))
    (add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-init))
  :config
  (progn
    (require 'thingatpt)
    (use-package eldoc :diminish eldoc-mode)

    (define-abbrev-table 'emacs-lisp-mode-abbrev-table
      '(("sm" "string-match") ("mm" "major-mode")
        ("rb" "region-beginning") ("ca" "char-after")
        ("smd" "save-match-data") ("mb" "match-beginning")
        ("pm" "point-min") ("ir" "indent-region")
        ("sf" "search-forward") ("ci" "call-interactively")
        ("sn" "symbol-name") ("se" "save-excursion")
        ("scb" "skip-chars-backward") ("fc" "forward-char")
        ("ff" "find-file") ("fs" "forward-sexp")
        ("pa" "prefix-arg") ("re" "region-end")
        ("dc" "delete-char") ("ms" "match-string")
        ("tc" "this-command") ("dd" "default-directory")
        ("bc" "backward-char") ("rsf" "re-search-forward")
        ("snp" "substring-no-properties")
        ("bsnp" "buffer-substring-no-properties")
        ("lep" "line-end-position") ("bs" "buffer-substring")
        ("cc" "condition-case") ("ul" "up-list")
        ("bfn" "buffer-file-name") ("lb" "looking-back")
        ("tap" "thing-at-point") ("rm" "replace-match")
        ("fl" "forward-line") ("df" "declare-function")
        ("ntr" "narrow-to-region") ("dr" "delete-region")
        ("rsb" "re-search-backward") ("scf" "skip-chars-forward")
        ("wcb" "with-current-buffer") ("ie" "ignore-errors")
        ("gc" "goto-char") ("jos" "just-one-space")
        ("la" "looking-at") ("ow" "other-window")
        ("dk" "define-key") ("dm" "deactivate-mark")
        ("bod" "beginning-of-defun") ("sic" "self-insert-command")
        ("eol" "end-of-line") ("me" "match-end")
        ("nai" "newline-and-indent") ("cb" "current-buffer")
        ("atl" "add-to-list") ("rris" "replace-regexp-in-string")))

    (font-lock-add-keywords 'emacs-lisp-mode '(("\\(?:^\\| \\)\\('\\sw\\(?:\\sw\\|\\s_\\)*\\)"
                                                1 'font-lock-constant-face)))

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

    (defun my-emacs-lisp-open-line ()
      "Opens a new line if the point is at the closing parens of
function on `my-emacs-lisp-open-line-list'."
      (interactive)
      (ignore-errors
        (my-newline)
        (when (and (save-excursion
                     (forward-char)
                     (backward-sexp)
                     (when (listp (sexp-at-point))
                       (memq (car (sexp-at-point)) my-emacs-lisp-open-line-list)))
                   (thing-at-point 'sexp)
                   (eq (following-char) ?\)))
          (newline)
          (indent-according-to-mode)
          (forward-line -1)
          (indent-according-to-mode))))
    (put 'my-emacs-lisp-open-line 'delete-selection t)

    (defun my-describe-thing-in-buffer ()
      "Display the full documentation of FUNCTION (a symbol) in the help buffer."
      (interactive)
      (let ((function (function-called-at-point))
            (variable (variable-at-point)))
        (cond
         ((not (numberp variable)) (describe-variable variable))
         (function (describe-function function)))))

    (defun backward-down-list (&optional n)
      (down-list (- (prefix-numeric-value n))))

    (defun my-wrap-region (beg end)
      (goto-char end)
      (insert ")")
      (goto-char beg)
      (insert "("))

    (defun my-goto-dominating-form (what)
      "Find dominating form starting with WHAT."
      (unless (listp what) (setq what (list what)))
      (while (and (> (car (syntax-ppss)) 0)
                  (not (ignore-errors
                         (backward-up-list)
                         (save-excursion
                           (down-list)
                           (memq (symbol-at-point) what)))))))

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
              (backward-down-list)
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
          (backward-down-list)
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
        (forward-sexp 2)
        (backward-sexp)
        (let ((var-list (sp-get (sp--next-thing-selection) (delete-and-extract-region :beg-prf :end))))
          (sp-splice-sexp-killing-backward 1)
          (my-goto-dominating-form '(let let*))
          (down-list)
          (forward-sexp 2)
          (backward-down-list)
          (insert "\n" var-list)
          (backward-down-list)
          (sp-unwrap-sexp)
          (backward-up-list 2)
          (indent-sexp))))

    (defun my-next-sexp ()
      (ignore-errors
        (forward-sexp 2)
        (backward-sexp)))

    (defun my-lisp-condify-if ()
      (interactive)
      (save-excursion
        (my-goto-dominating-form 'if)
        (let ((p (point)))
          (down-list)
          (my-next-sexp)
          (let ((condition (sexp-at-point))
                (body1 (progn
                         (my-next-sexp)
                         (sexp-at-point)))
                (body2 (progn
                         (my-next-sexp)
                         (sexp-at-point))))
            (goto-char p)
            (cl-destructuring-bind (beg . end) (bounds-of-thing-at-point 'sexp)
              (delete-region beg end))
            (insert
             (format "(cond
                   (%s
                     %s)
                   (t
                     %s))" condition body1 body2))
            (goto-char p)
            (indent-sexp)))))))
