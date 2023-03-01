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
      '(try-complete-file-name-partially
        try-complete-file-name
        yas-hippie-try-expand
        my-try-expand-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
        try-complete-lisp-symbol
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart Tab
;; Borrowed from snippets at
;; http://www.emacswiki.org/emacs/TabCompletion

(defun my-smart-tab-must-expand ()
  "Return non-nil if we must expand."
  (unless mark-active
    (or (looking-at "\\_>")
        ;; TODO: make this mode specific? ... this entire framework
        ;; needs to go, is utter shite
        (looking-back "\\." 1)
        (looking-back "/" 1))))

(defun my-smart-tab-default-action ()
  "Execute the default smart tab action."
  (if (use-region-p)
      (indent-region (region-beginning) (region-end))
    (cond
     ((and (not (looking-at-p "[[:blank:]]*$"))
           (sp--looking-back-p "^[[:blank:]]*" (- (point) (line-beginning-position)))
           (let ((p (point)))
             (indent-for-tab-command)
             (/= p (point)))))
     ;; First try to indent with tab-command.
     ;; ((and (not (derived-mode-p 'comint-mode))
     ;;       (not (my-smart-tab-must-expand))
     ;;       (let ((p (point)))
     ;;         (indent-for-tab-command)
     ;;         (/= p (point)))))
     ;; If point did not change, try to indent the current defun.
     ;; ((and (not (derived-mode-p 'comint-mode))
     ;;       (let ((bs (buffer-substring-no-properties (point-min) (point-max))))
     ;;         (indent-defun)
     ;;         (not
     ;;          (equal bs (buffer-substring-no-properties (point-min) (point-max)))))))
     ;; If buffer did not change, try various completions.
     ((progn
        (cond
         ((and company-mode (shut-up (company-complete))))
         ((ignore-errors (hippie-expand) (>= he-num 0)))
         ;; Finally, run indent again.
         ((indent-for-tab-command)))
        nil))
     ;; copilot is always run last so that other synchronous commands
     ;; don't cancel its overlay
     ((and copilot-mode (copilot-complete))))))

(defvar my-magit-read-files-is-active nil
  "Set to t when in `magit-read-files'.

This is a hack to turn off `smart-tab' behaviour and just use
`minibuffer-complete' (because the smart shit doesn't work with
`ido-ubiquitous-mode').")

(defadvice magit-read-files (around set-flag activate)
  (let ((my-magit-read-files-is-active t))
    ad-do-it))

(defun smart-tab ()
  "Do what I mean when I hit tab."
  (interactive)
  (cond
   ((and allout-mode (allout-on-current-heading-p))
    (allout-toggle-current-subtree-exposure))
   ((eq major-mode 'term-mode)
    (term-send-raw-string "\t"))
   (ido-cur-item
    (ido-complete))
   ((and copilot--overlay
         ;; most likely we are in a copilot overlay, try to accept
         ;; (copilot-accept-completion)
         (copilot-clear-overlay)))
   ((bound-and-true-p elfeed-search-live)
    (completion-at-point))
   ((bound-and-true-p my-magit-read-files-is-active)
    (minibuffer-complete))
   ((memq major-mode '(org-mode markdown-mode gfm-mode))
    (cond
     ((my-smart-tab-must-expand)
      (hippie-expand))
     ((use-region-p)
      (indent-region (region-beginning) (region-end)))
     (:otherwise (if (eq major-mode 'org-mode) (org-cycle) (markdown-cycle)))))
   (:otherwise (my-smart-tab-default-action))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-key "<tab>" 'smart-tab)
(bind-key "<tab>" 'smart-tab minibuffer-local-map)
(bind-key "<tab>" 'smart-tab read-expression-map)

;;; tabs.el ends here
