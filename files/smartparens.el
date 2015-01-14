;;;;;;;;;
;; global
(require 'smartparens-config)

;;;;;;;;;;;;;;;;;;;;;;;;
;; keybinding management
(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

(define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
(define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
(define-key sp-keymap (kbd "C-S-d") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "C-S-a") 'sp-end-of-sexp)

(define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
(define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
(define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

(define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
(define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)

(define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
(define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

(define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

(define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
(define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
(define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
(define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

(define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
(define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)

(define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
(define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

(bind-key "H-t" 'sp-prefix-tag-object sp-keymap)
(bind-key "H-p" 'sp-prefix-pair-object sp-keymap)
(bind-key "H-y" 'sp-prefix-symbol-object sp-keymap)
(bind-key "H-h" 'sp-highlight-current-sexp sp-keymap)
(bind-key "H-e" 'sp-prefix-save-excursion sp-keymap)
(bind-key "H-s c" 'sp-convolute-sexp sp-keymap)
(bind-key "H-s a" 'sp-absorb-sexp sp-keymap)
(bind-key "H-s e" 'sp-emit-sexp sp-keymap)
(bind-key "H-s p" 'sp-add-to-previous-sexp sp-keymap)
(bind-key "H-s n" 'sp-add-to-next-sexp sp-keymap)
(bind-key "H-s j" 'sp-join-sexp sp-keymap)
(bind-key "H-s s" 'sp-split-sexp sp-keymap)
(bind-key "H-s r" 'sp-rewrap-sexp sp-keymap)
(defvar hyp-s-x-map)
(define-prefix-command 'hyp-s-x-map)
(bind-key "H-s x" hyp-s-x-map sp-keymap)
(bind-key "H-s x x" 'sp-extract-before-sexp sp-keymap)
(bind-key "H-s x a" 'sp-extract-after-sexp sp-keymap)
(bind-key "H-s x s" 'sp-swap-enclosing-sexp sp-keymap)

(bind-key "C-x C-t" 'sp-transpose-hybrid-sexp sp-keymap)

(bind-key ";" 'sp-comment emacs-lisp-mode-map)

;;;;;;;;;;;;;;;;;;
;; pair management

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
(bind-key "C-(" 'sp---wrap-with-40 minibuffer-local-map)

;;; markdown-mode
(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*" :wrap "C-*" :skip-match 'sp--gfm-skip-asterisk)
  (sp-local-pair "_" "_" :wrap "C-_")
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

(defun sp--gfm-skip-asterisk (ms mb me)
  (save-excursion
    (goto-char mb)
    (save-match-data (looking-at "^\\* "))))

;;; org-mode
(sp-with-modes 'org-mode
  (sp-local-pair "*" "*" :actions '(insert wrap) :unless '(sp-point-after-word-p sp-point-at-bol-p) :wrap "C-*" :skip-match 'sp--org-skip-asterisk)
  (sp-local-pair "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_" :skip-match 'sp--org-skip-markup)
  (sp-local-pair "/" "/" :unless '(sp-point-after-word-p) :skip-match 'sp--org-skip-markup)
  (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :skip-match 'sp--org-skip-code)
  (sp-local-pair "«" "»"))

(defun sp--org-skip-markup (ms mb me)
  (save-excursion
    (and (progn
           (goto-char mb)
           (save-match-data (looking-back "\\sw\\|\\s_\\|\\s.")))
         (progn
           (goto-char me)
           (save-match-data (looking-at "\\sw\\|\\s_\\|\\s."))))))

(defun sp--org-skip-asterisk (ms mb me)
  (or (save-excursion
        (goto-char (line-beginning-position))
        (save-match-data (looking-at "^[*]+")))
      (sp--org-skip-markup ms mb me)))

(defun sp--org-skip-code (ms mb me)
  (or (sp--org-skip-markup ms mb me)
      (save-excursion
        (and (progn
               (goto-char mb)
               (save-match-data (looking-back "\\s-")))
             (progn
               (goto-char me)
               (save-match-data (looking-at "\\s-")))))))

;;; tex-mode latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "\"<" "\">"))

;;; lisp modes
(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil
                 :wrap "C-("
                 :pre-handlers '(my-add-space-before-sexp-insertion)
                 :post-handlers '(my-add-space-after-sexp-insertion)))



(defun my-add-space-after-sexp-insertion (id action _context)
  (when (eq action 'insert)
    (save-excursion
      (forward-char (sp-get-pair id :cl-l))
      (when (or (eq (char-syntax (following-char)) ?w)
                (looking-at (sp--get-opening-regexp)))
        (insert " ")))))

(defun my-add-space-before-sexp-insertion (id action _context)
  (when (eq action 'insert)
    (save-excursion
      (backward-char (length id))
      (when (or (eq (char-syntax (preceding-char)) ?w)
                (and (looking-back (sp--get-closing-regexp))
                     (not (eq (char-syntax (preceding-char)) ?'))))
        (insert " ")))))

;;; C++
(sp-with-modes '(malabar-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
(sp-local-pair 'c++-mode "/*" "*/" :post-handlers '((" | " "SPC")
                                                    ("* ||\n[i]" "RET")))

;;; haskell mode
(sp-with-modes '(haskell-mode)
  (sp-local-pair "'" nil :unless '(my-after-symbol-p))
  (sp-local-pair "\\\(" nil :actions :rem))

(defun my-after-symbol-p (_id action _context)
  (when (eq action 'insert)
    (save-excursion
      (backward-char 1)
      (looking-back "\\sw\\|\\s_\\|\\s'"))))

(sp-with-modes '(php-mode)
  (sp-local-pair "/**" "*/" :post-handlers '(("| " "SPC")
                                             (my-php-handle-docstring "RET")))
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))

(defun my-php-handle-docstring (&rest _ignored)
  (-when-let (line (save-excursion
                     (forward-line)
                     (sp-get (sp-get-hybrid-sexp)
                       (buffer-substring-no-properties :beg :end))))
    (cond
     ((string-match-p "function" line)
      (save-excursion
        (insert "\n")
        (let (args)
          (save-match-data
            (with-temp-buffer
              (insert line)
              (goto-char (point-min))
              (while (re-search-forward "\\(\\$.*?\\)[ \n\t,)]" nil t)
                (push (match-string 1) args))))
          (setq args (nreverse args))
          (--each args
            (insert (format "* @param %s\n" it)))))
      (insert "* "))
     ((string-match-p "class" line)
      (save-excursion (insert "\n*\n* @author\n"))
      (insert "* ")))
    (let ((o (sp--get-active-overlay)))
       (indent-region (overlay-start o) (overlay-end o)))))
