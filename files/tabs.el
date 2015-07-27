;;; tabs.el --- Tab management.

;;; Commentary:
;;; Code:

(require 'bind-key)

;; If there is a tab, make it the size of 8 spaces
(setq-default tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))
;; Spaces instead of tabs
(setq-default indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hippie expand.  Groovy vans with tie-dyes.

(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-all-abbrevs
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

(defun my-smart-tab-must-expand (prefix)
  "Return non-nil if we must expand."
  (unless (or (consp prefix)
              mark-active)
    (looking-at "\\_>")))

(defun my-smart-tab-default-action (prefix)
  "Execute the default smart tab action."
  ;; (unless (completion-at-point)
  (cond
     ((my-smart-tab-must-expand prefix)
      (ignore-errors (hippie-expand prefix)))
     ((my-smart-indent)))
  ;; )
  )

(defun smart-tab (prefix)
  "Do what I mean when I hit tab.

Needs `transient-mark-mode' to be on.  This smart tab is
minibuffer compliant: it acts as usual in the minibuffer.

In all other buffers: if PREFIX is \\[universal-argument], calls
`my-smart-indent'.  Else if point is at the end of a symbol,
expands it.  Else calls `my-smart-indent'."
  (interactive "P")
  (cond
   ((bound-and-true-p elfeed-search-live)
    (completion-at-point))
   ((eq major-mode 'org-mode)
    (cond
     ((looking-back "^<\\sw")
      (org-cycle))
     ((my-smart-tab-must-expand prefix)
      (hippie-expand prefix))
     ((use-region-p)
      (indent-region (region-beginning) (region-end)))
     (:otherwise (org-cycle))))
   (:otherwise (my-smart-tab-default-action prefix))))

(defun my-smart-indent ()
  "Indent region if mark is active, or current line otherwise."
  (interactive)
  (if (use-region-p)
      (indent-region (region-beginning)
                     (region-end))
    (indent-for-tab-command)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-key "<tab>" 'smart-tab)
(bind-key "<tab>" 'smart-tab minibuffer-local-map)
(bind-key "<tab>" 'smart-tab read-expression-map)

;;; tabs.el ends here
