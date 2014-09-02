;;; Tab management

;; If there is a tab, make it the size of 8 spaces
(setq default-tab-width 4)
(setq-default tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))
;; Spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Mode specific indent sizes
;; TODO: Consider putting these in their own mode specific inits
;; (setq c-basic-offset 4)
;; (setq css-indent-offset 2)
;; (setq sh-basic-offset 2)
;; (setq-default javascript-indent-level 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hippie expand.  Groovy vans with tie-dyes.

(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-complete-lisp-symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart Tab
;; Borrowed from snippets at
;; http://www.emacswiki.org/emacs/TabCompletion
;; TODO: Take a look at https://github.com/genehack/smart-tab

(defun smart-tab (prefix)
  "Needs `transient-mark-mode' to be on. This smart tab is
  minibuffer compliant: it acts as usual in the minibuffer.

  In all other buffers: if PREFIX is \\[universal-argument], calls
  `smart-indent'. Else if point is at the end of a symbol,
  expands it. Else calls `smart-indent'."
  (interactive "P")
  (labels ((smart-tab-must-expand (&optional prefix)
                                  (unless (or (consp prefix)
                                              mark-active)
                                    (looking-at "\\_>")))
           (default ()
             (cond
              ((smart-tab-must-expand prefix)
               (hippie-expand prefix))
              ((smart-indent)))))
    (cond
     ((bound-and-true-p elfeed-search-live)
      (completion-at-point))
     ((minibufferp)
      (unless (minibuffer-complete)
        (unless (completion-at-point)
          (default))))
     ((and (eq major-mode 'org-mode)
           (looking-back "^<\\sw")
           (org-cycle)))
     ((and (eq major-mode 'org-mode)
           (unless (default)
             (org-cycle))))
     (:else (default)))))

(defun smart-indent ()
  "Indents region if mark is active, or current line otherwise."
  (interactive)
  (if (use-region-p)
      (indent-region (region-beginning)
                     (region-end))
    (indent-for-tab-command)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-key "TAB" 'smart-tab)
(bind-key "TAB" 'smart-tab minibuffer-local-map)
(bind-key "TAB" 'smart-tab read-expression-map)
