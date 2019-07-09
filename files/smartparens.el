;; -*- lexical-binding: t -*-
;;;;;;;;;
;; global
(require 'smartparens-config)

(add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)

;;;;;;;;;;;;;;;;;;;;;;;;
;; keybinding management
(define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

(define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
(define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
(define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)

(define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)

(define-key smartparens-mode-map (kbd "C-M-n") 'sp-forward-hybrid-sexp)
(define-key smartparens-mode-map (kbd "C-M-p") 'sp-backward-hybrid-sexp)

(define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

(define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
(define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

(define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)

(define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)
(define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
(define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
(define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

(define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key smartparens-mode-map (kbd "C-<left_bracket>") 'sp-select-previous-thing)
(define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)

(define-key smartparens-mode-map (kbd "M-F") 'sp-forward-symbol)
(define-key smartparens-mode-map (kbd "M-B") 'sp-backward-symbol)

(define-key smartparens-mode-map (kbd "C-\"") 'sp-change-inner)
(define-key smartparens-mode-map (kbd "M-i") 'sp-change-enclosing)

(bind-key "C-c f" (lambda () (interactive) (sp-beginning-of-sexp 2)) smartparens-mode-map)
(bind-key "C-c b" (lambda () (interactive) (sp-beginning-of-sexp -2)) smartparens-mode-map)

(bind-key "C-M-s"
          (defhydra smartparens-hydra ()
            "Smartparens"
            ("d" sp-down-sexp "Down")
            ("e" sp-up-sexp "Up")
            ("u" sp-backward-up-sexp "Up")
            ("a" sp-backward-down-sexp "Down")
            ("f" sp-forward-sexp "Forward")
            ("b" sp-backward-sexp "Backward")
            ("k" sp-kill-sexp "Kill" :color blue)
            ("q" nil "Quit" :color blue))
          smartparens-mode-map)

(bind-key "H-t" 'sp-prefix-tag-object smartparens-mode-map)
(bind-key "H-p" 'sp-prefix-pair-object smartparens-mode-map)
(bind-key "H-y" 'sp-prefix-symbol-object smartparens-mode-map)
(bind-key "H-h" 'sp-highlight-current-sexp smartparens-mode-map)
(bind-key "H-e" 'sp-prefix-save-excursion smartparens-mode-map)
(bind-key "H-s c" 'sp-convolute-sexp smartparens-mode-map)
(bind-key "H-s a" 'sp-absorb-sexp smartparens-mode-map)
(bind-key "H-s e" 'sp-emit-sexp smartparens-mode-map)
(bind-key "H-s p" 'sp-add-to-previous-sexp smartparens-mode-map)
(bind-key "H-s n" 'sp-add-to-next-sexp smartparens-mode-map)
(bind-key "H-s j" 'sp-join-sexp smartparens-mode-map)
(bind-key "H-s s" 'sp-split-sexp smartparens-mode-map)
(bind-key "H-s r" 'sp-rewrap-sexp smartparens-mode-map)
(defvar hyp-s-x-map)
(define-prefix-command 'hyp-s-x-map)
(bind-key "H-s x" hyp-s-x-map smartparens-mode-map)
(bind-key "H-s x x" 'sp-extract-before-sexp smartparens-mode-map)
(bind-key "H-s x a" 'sp-extract-after-sexp smartparens-mode-map)
(bind-key "H-s x s" 'sp-swap-enclosing-sexp smartparens-mode-map)

(bind-key "C-x C-t" 'sp-transpose-hybrid-sexp smartparens-mode-map)

(bind-key ";" 'sp-comment emacs-lisp-mode-map)

(bind-key [remap c-electric-backspace] 'sp-backward-delete-char smartparens-strict-mode-map)

;;;;;;;;;;;;;;;;;;
;; pair management

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
(bind-key "C-(" 'sp---wrap-with-40 minibuffer-local-map)

(sp-with-modes 'org-mode
  (sp-local-pair "=" "=" :wrap "C-="))

(sp-with-modes 'textile-mode
  (sp-local-pair "*" "*")
  (sp-local-pair "_" "_")
  (sp-local-pair "@" "@"))

(sp-with-modes 'web-mode
  (sp-local-pair "{{#if" "{{/if")
  (sp-local-pair "{{#unless" "{{/unless"))

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


(sp-local-pair 'js2-mode "/**" "*/" :post-handlers '(("| " "SPC")
                                                     ("* ||\n[i]" "RET")))

;;; PHP
(sp-with-modes '(php-mode)
  (sp-local-pair "/**" "*/" :post-handlers '(("| " "SPC")
                                             (my-php-handle-docstring "RET")))
  (sp-local-pair "/*." ".*/" :post-handlers '(("| " "SPC")))
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET") my-php-wrap-handler))
  (sp-local-pair "(" nil :prefix "\\(\\sw\\|\\s_\\)*"))

(defun my-php-wrap-handler (&rest _ignored)
  (save-excursion
    (sp-get sp-last-wrapped-region
      (goto-char :beg-in)
      (unless (looking-at "[ \t]*$")
        (newline-and-indent))
      (goto-char :end-in)
      (beginning-of-line)
      (unless (looking-at "[ \t]*}[ \t]*$")
        (goto-char :end-in)
        (newline-and-indent))
      (indent-region :beg-prf :end-suf))))

(defun my-php-handle-docstring (&rest _ignored)
  (-when-let (line (save-excursion
                     (forward-line)
                     (thing-at-point 'line)))
    (cond
     ;; variable
     ((string-match (rx (or "private" "protected" "public" "var") (1+ " ") (group "$" (1+ alnum))) line)
      (let ((var-name (match-string 1 line))
            (type ""))
        ;; try to guess the type from the constructor
        (-when-let (constructor-args (my-php-get-function-args "__construct" t))
          (setq type (or (cdr (assoc var-name constructor-args)) "")))
        (insert "* @var " type)
        (save-excursion
          (insert "\n"))))
     ((string-match-p "function" line)
      (save-excursion
        (let ((args (save-excursion
                      (forward-line)
                      (my-php-get-function-args nil t))))
          (--each args
            (when (my-php-should-insert-type-annotation (cdr it))
              (insert (format "* @param %s%s\n"
                              (my-php-translate-type-annotation (cdr it))
                              (car it))))))
        (let ((return-type (save-excursion
                             (forward-line)
                             (my-php-get-function-return-type))))
          (when (my-php-should-insert-type-annotation return-type)
            (insert (format "* @return %s\n" (my-php-translate-type-annotation return-type))))))
      (re-search-forward (rx "@" (or "param" "return") " ") nil t))
     ((string-match-p ".*class\\|interface" line)
      (save-excursion (insert "\n"))
      (insert "* ")))
    (let ((o (sp--get-active-overlay)))
      (indent-region (overlay-start o) (overlay-end o)))))
