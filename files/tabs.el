;;; tabs.el --- Tab management.

;;; Commentary:
;;; Code:

(require 'bind-key)
(require 'company)

;; If there is a tab, make it the size of 8 spaces
(setq-default tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))
;; Spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Copy-paste from hippie-exp.el, function try-expand-all-abbrevs
(defun my-try-expand-abbrevs (old)
  "Try to expand word before point according to all abbrev tables.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible expansions of the same
string).  It returns t if a new expansion is found, nil otherwise."
  (if (not old)
      (progn
        (he-init-string (he-dabbrev-beg) (point))
        (setq he-expand-list
              (and (not (equal he-search-string ""))
                   (mapcar (function (lambda (sym)
                                       (if (and (boundp sym) (vectorp (eval sym)))
                                           (abbrev-expansion (downcase he-search-string)
                                                             (eval sym)))))
                           ;; FUCO: here we only use the local table,
                           ;; not all tables
                           (list 'local-abbrev-table))))))
  (while (and he-expand-list
              (or (not (car he-expand-list))
                  (he-string-member (car he-expand-list) he-tried-table t)))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
        (if old (he-reset-string))
        ())
    (progn
      (he-substitute-string (car he-expand-list) t)
      (setq he-expand-list (cdr he-expand-list))
      t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hippie expand.  Groovy vans with tie-dyes.

(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand
        my-try-expand-abbrevs
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
    (or (looking-at "\\_>")
        ;; TODO: make this mode specific? ... this entire framework
        ;; needs to go, is utter shite
        (looking-back "\\."))))

(defun my-smart-tab-default-action (prefix)
  "Execute the default smart tab action."
  (if (use-region-p)
      (indent-region (region-beginning)
                     (region-end))
    (unless (ignore-errors (completion-at-point))
      (cond
       (company-mode
        (company-complete))
       ((my-smart-tab-must-expand prefix)
        (ignore-errors (hippie-expand prefix)))
       ((my-smart-indent))))))

(defvar my-magit-read-files-is-active nil
  "Set to t when in `magit-read-files'.

This is a hack to turn off `smart-tab' behaviour and just use
`minibuffer-complete' (because the smart shit doesn't work with
`ido-ubiquitous-mode').")

(defadvice magit-read-files (around set-flag activate)
  (let ((my-magit-read-files-is-active t))
    ad-do-it))

(defun smart-tab (prefix)
  "Do what I mean when I hit tab.

Needs `transient-mark-mode' to be on.  This smart tab is
minibuffer compliant: it acts as usual in the minibuffer.

In all other buffers: if PREFIX is \\[universal-argument], calls
`my-smart-indent'.  Else if point is at the end of a symbol,
expands it.  Else calls `my-smart-indent'."
  (interactive "P")
  (cond
   ((eq major-mode 'term-mode)
    (term-send-raw-string "\t"))
   ((bound-and-true-p elfeed-search-live)
    (completion-at-point))
   ((bound-and-true-p my-magit-read-files-is-active)
    (minibuffer-complete))
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
