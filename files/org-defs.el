(use-package org-drill
  :commands org-drill
  :init
  (progn
    (use-package pcase)

    (bind-keys :map org-mode-map
      ("H-d" . org-drill)
      ("H-r" . org-drill-resume)
      ("H-a" . org-drill-again))

    (defun my-org-convert-tab-word-list-to-drill ()
      (interactive)
      (goto-char (point-min))
      (replace-regexp "\\s-*- # [0-9]*" "")
      (goto-char (point-min))
      (while (not (eobp))
        (let ((russian (buffer-substring-no-properties
                        (point)
                        (progn
                          (search-forward "\t" nil t)
                          (backward-char)
                          (point))))
              (english (buffer-substring-no-properties
                        (progn
                          (forward-char)
                          (point))
                        (line-end-position))))
          (beginning-of-line)
          (kill-line 1)
          (insert
           (format
            "* Word                                                                :drill:
    :PROPERTIES:
    :DRILL_CARD_TYPE: twosidednocloze
    :END:
** Russian
%s
** English
%s
** Examples
** Notes
"
            russian english)))))

    (defun my-org-add-drill-entry ()
      (interactive)
      (insert
       (format
        "* Word :drill:
    :PROPERTIES:
    :DRILL_CARD_TYPE: twosidednocloze
    :END:
** %s

** English

** Examples

** Notes

"
        my-org-drill-language))
      (re-search-backward ":PROPERTIES:" nil t)
      (org-cycle)
      (re-search-forward ":END:" nil t)
      (forward-line 2))

    (defun my-format-russian-verb ()
      (interactive)
      (beginning-of-line)
      (forward-word)
      (kill-sexp)
      (delete-char 1)
      (save-excursion (insert " "))
      (transpose-words 1)
      (backward-word 2)
      (insert "- ")
      (forward-word)
      (insert ":")
      (forward-word)
      (newline)
      (delete-char 2)
      (insert "- ")
      (forward-char 4)
      (kill-word 1)
      (insert ":")
      (end-of-line)
      (delete-char -1))

    (defun my-format-meaning ()
      "Format meaning of a drill card."
      (interactive)
      (pcase my-org-drill-language
        ("Russian" (my-format-russian-meaning))
        ("Latin" (my-format-latin-meaning))))

    (defun my-format-russian-meaning ()
      "Format meaning of a russian drill card."
      (interactive)
      (delete-char -1)
      (outline-previous-visible-heading 1)
      (forward-line)
      (delete-char 1)
      (let ((beg (point))
            (end (progn
                   (outline-next-visible-heading 1)
                   (forward-line -1)
                   (point))))
        (my-org-make-numbered-list beg end)))

    (defun my-format-latin-meaning ()
      "Format meaning of a latin drill card."
      (interactive)
      (let ((end (1- (cdr (bounds-of-thing-at-point 'line)))))
        (ignore-errors
          (while (search-forward ";" end t)
            (delete-char -1)
            (forward-char)
            (newline)))
        (delete-char -1)
        (forward-line -1)
        (let ((end (point))
              (start (progn
                       (outline-next-visible-heading -1)
                       (forward-line)
                       (point))))
          (my-org-make-numbered-list start end)))))
  :config
  (progn
    (defun org-drill-present-two-sided-card-no-cloze ()
      (with-hidden-comments
       (let ((drill-sections (org-drill-hide-all-subheadings-except nil)))
         (when drill-sections
           (save-excursion
             (goto-char (nth (random* (min 2 (length drill-sections)))
                             drill-sections))
             (org-show-subtree)))
         (ignore-errors
           (org-display-inline-images t))
         (org-cycle-hide-drawers 'all)
         (prog1 (org-drill-presentation-prompt)
           (org-drill-hide-subheadings-if 'org-drill-entry-p)))))))

;; To fix the stickyness issue with incorrectly named buffers (just
;; (m) instead of (m/keys:query)):
;; in org-tags-view, after "catch 'exit", put
;; (unless match
;;   (setq match (org-completing-read-no-i
;;                "Match: " 'org-tags-completion-function nil nil nil
;;                'org-tags-history)))
(use-package org-agenda
  :defer t
  :init
  (progn
    ;; Custom agenda command definitions
    (defvar my-org-show-media-closed-since (apply 'encode-time (org-parse-time-string "1980-01-01"))
      "Time since which we show the closed media")

    (defun my-org-agenda-filter (prefix title &rest args)
      `((,prefix . ,title)
        (,(concat prefix "a") . "All")
        (,(concat prefix "p") . "Ready")
        (,(concat prefix "d") . "Done")
        ,@(--mapcat
           `((,(concat prefix (car it)) tags-todo ,(concat "+" (cdr it) "+TODO=\"NEXT\""))
             (,(concat prefix "a" (car it)) tags ,(concat "+" (cdr it) "-folder"))
             (,(concat prefix "p" (car it)) tags ,(concat "+" (cdr it) "-folder-TODO=\"DONE\"")
              ((org-agenda-skip-function '(let ((next-headline (save-excursion
                                                                 (or (outline-next-heading)
                                                                     (point-max)))))
                                            (when (member "BOOKS" (org-get-tags))
                                              next-headline)))))
             (,(concat prefix "d" (car it)) tags ,(concat "+" (cdr it) "-folder+TODO=\"DONE\"")
              ((org-agenda-cmp-user-defined 'my-org-compare-closed-entries)
               (org-agenda-sorting-strategy '(user-defined-up))
               (org-agenda-skip-function '(let ((next-headline (save-excursion
                                                                 (or (outline-next-heading)
                                                                     (point-max)))))
                                            (let ((closed-at (org-time-string-to-time
                                                              (org-entry-get (point) "CLOSED"))))
                                              (when (time-less-p closed-at
                                                                 my-org-show-media-closed-since)
                                                next-headline)))))))
           args)))

    (setq org-agenda-custom-commands
          `((" " "Agenda"
             ((agenda "" nil)
              (tags "REFILE"
                    ((org-agenda-overriding-header "Tasks to Refile")
                     (org-tags-match-list-sublevels nil)))
              (tags-todo "BUG/!NEXT"
                         ((org-agenda-overriding-header "Bugs")
                          (org-tags-match-list-sublevels nil)))
              (tags-todo "-STOP/!-WAIT"
                         ((org-agenda-overriding-header "Stuck Projects")
                          (org-agenda-skip-function 'my-org-skip-non-stuck-projects)))
              (tags-todo "-WAIT-HOLD-STOP-BOOKS-BUG/!NEXT"
                         ((org-agenda-overriding-header "Next Tasks")
                          (org-agenda-skip-function 'my-org-skip-projects-and-habits-and-single-tasks)
                          (org-agenda-todo-ignore-scheduled t)
                          (org-agenda-todo-ignore-deadlines t)
                          (org-agenda-todo-ignore-with-date t)
                          (org-tags-match-list-sublevels t)
                          (org-agenda-sorting-strategy '(priority-down todo-state-down effort-up category-keep))))
              (tags-todo "-REFILE-STOP-BOOKS-download/!-HOLD-WAIT-IDEA"
                         ((org-agenda-overriding-header "Tasks")
                          (org-agenda-skip-function 'my-org-skip-project-tasks-maybe)
                          (org-agenda-todo-ignore-scheduled t)
                          (org-agenda-todo-ignore-deadlines t)
                          (org-agenda-todo-ignore-with-date t)
                          (org-agenda-sorting-strategy '(priority-down category-keep))))
              (tags-todo "-STOP/!+WAIT"
                         ((org-agenda-overriding-header "Waiting Tasks")
                          (org-agenda-skip-function 'my-org-skip-projects)
                          (org-tags-match-list-sublevels nil)
                          (org-agenda-todo-ignore-scheduled 'future)
                          (org-agenda-todo-ignore-deadlines 'future)))
              ;; Active projects and projects that wait on something
              ;; Things we are working on
              ;; TODO: should show immediate children tasks if narrowed
              (tags-todo "-HOLD-STOP-GENERAL/!"
                         ((org-agenda-overriding-header (if (my-org-restricted-p)
                                                            "Subprojects (and children tasks)"
                                                          "Projects"))
                          (org-agenda-skip-function 'my-org-skip-non-projects)
                          (org-tags-match-list-sublevels 'indented)
                          (org-agenda-sorting-strategy '(priority-down category-keep))))
              ;; Projects/tasks on HOLD: projects that are not cancelled, but we don't want to work on them now
              (tags-todo "-STOP/!+HOLD"
                         ((org-agenda-overriding-header "Postponed Projects and Tasks")
                          (org-agenda-skip-function 'my-org-skip-stuck-projects)
                          (org-tags-match-list-sublevels nil)
                          (org-agenda-todo-ignore-scheduled 'future)
                          (org-agenda-todo-ignore-deadlines 'future))))
             nil)
            ,@(my-org-agenda-filter "f" "Media filter" '("b" . "BOOKS") '("m" . "MOV"))
            ("k" . "Knowledge-base operations")
            ("ks" "Knowledge-base search" search nil
             ((org-agenda-files '("~/org/kb.org"))))
            ("km" "Knowledge-base tag match" tags nil
             ((org-agenda-files '("~/org/kb.org"))))
            ("b" . "Bookmarks operations")
            ("bs" "Bookmarks search" search nil
             ((org-agenda-files '("~/org/bookmarks.org"))))
            ("bm" "Bookmakrs tag match" tags nil
             ((org-agenda-files '("~/org/bookmarks.org"))))
            ("d" "Downloads" tags "+download/!TODO")
            ;; Reading items are not marked with TODO, we use this view instead.
            ;; Item that we are reading at the moment, however, is marked
            ;; NEXT as every other current task.
            ("r" "Reading"
             ((tags "+Reading/-DONE"
                    ((org-agenda-overriding-header "To read")))
              (tags "+Reading/DONE"
                    ((org-agenda-overriding-header "Finished"))))))))
  :config
  (progn
    (defun org-agenda-time-limit (time)
      "Call `org-agenda' with media timestamp limited."
      (interactive "sTimestamp: ")
      (let ((my-org-show-media-closed-since
             (apply 'encode-time (org-parse-time-string time))))
        (org-agenda)))

    ;; View
    (defun my-org-agenda-is-task-p ()
      "Return non-nil if line at point is a task."
      (org-get-at-bol 'org-marker))

    (defun my-org-agenda-remove-empty-lists ()
      (let ((headers '("Tasks to Refile"
                       "Bugs"
                       "Stuck Projects"
                       "Next Tasks"
                       "Tasks"
                       "Waiting Tasks"
                       "Projects"
                       "Subprojects (and children tasks)"
                       "Postponed Projects and Tasks"
                       "Reading")))
        (let ((case-fold-search nil))
          (--each headers
            (save-excursion
              (goto-char (point-min))
              (when (re-search-forward (concat "^" (regexp-quote it)) nil t)
                (unless (save-excursion
                          (forward-line)
                          (my-org-agenda-is-task-p))
                  (delete-region (line-beginning-position) (1+ (line-end-position)))))))
          (save-excursion
            (goto-char (point-min))
            (when (re-search-forward (concat "^" (regexp-opt headers)) nil t)
              (goto-char (match-beginning 0))
              (backward-char)
              (insert (propertize (concat "\n" (make-string (/ (window-width) 2) ?─)) 'face 'org-time-grid)))))))

    (add-hook 'org-agenda-finalize-hook 'my-org-agenda-remove-empty-lists)

    ;; Better links
    (defun my-org-agenda-open-at-point (&optional arg)
      "Open the first link after the headline under point."
      (interactive "P")
      (org-with-point-at (org-get-at-bol 'org-hd-marker)
        (my-org-open-at-point arg)))

    ;; Better filters
    (defun my-org--get-agenda-tags ()
      "Return all tags present in current agenda view."
      (let (tags)
        (my-with-each-line
          (--when-let (org-get-at-bol 'tags)
            (--each it (push it tags))))
        (-uniq tags)))

    (defun my-org-agenda-filter-by-tag-refine (strip &optional char)
      "Just like `org-agenda-filter-by-tag-refine' but with tags from
current agenda view added to `org-tag-alist'."
      (interactive "P")
      (unless (local-variable-p 'org-global-tags-completion-table (current-buffer))
        (org-set-local 'org-global-tags-completion-table
                        (-uniq (-map 'downcase
                                      (-concat (my-org--get-agenda-tags)
                                               (-filter 'stringp (-map 'car org-tag-alist)))))))
      (org-agenda-filter-by-tag-refine strip char))

    (defun my-org-agenda-filter-by-tag (strip &optional char narrow)
      "Just like `org-agenda-filter-by-tag' but with tags from
current agenda view added to `org-tag-alist'."
      (interactive "P")
      (unless (local-variable-p 'org-global-tags-completion-table (current-buffer))
        (org-set-local 'org-global-tags-completion-table
                        (-uniq (-map 'downcase
                                      (-concat (my-org--get-agenda-tags)
                                               (-filter 'stringp (-map 'car org-tag-alist)))))))
      (org-agenda-filter-by-tag strip char narrow))

    (bind-keys :map org-agenda-mode-map
      ("C-n" . org-agenda-next-item)
      ("C-p" . org-agenda-previous-item)
      ("P" . my-org-narrow-to-project)
      ("U" . my-org-narrow-to-parent)
      ("N" . my-org-narrow-to-subtree)
      ("W" . my-org-widen)
      ("/" . my-org-agenda-filter-by-tag)
      ("\\" . my-org-agenda-filter-by-tag-refine)
      ("o" . my-org-agenda-open-at-point))))

;; TODO: figure out a way to add dependencies to `use-package', then
;; load this after either notmuch or org
(use-package org-notmuch)

(use-package org-protocol
  :init
  (progn
    (use-package async)
    (defun my-org-protocol-save-youtube (info)
      (let* ((parts (org-protocol-split-data info t org-protocol-data-separator))
             (link (car parts)))
        (save-window-excursion
          (async-start-process "ydown" "ydown" nil link)
          (message "Youtube download started: %s" link)
          nil)))

    (push '("save-youtube"
            :protocol "save-youtube"
            :function my-org-protocol-save-youtube
            :kill-client nil)
          org-protocol-protocol-alist)))

(use-package org-contacts)

(use-package org-velocity
  :commands org-velocity
  :load-path "projects/org-velocity/"
  :init
  (progn
    (bind-key "C-c s" 'org-velocity org-mode-map)))

(use-package org-timer
  :bind (("C-c C-x ;" . org-timer-set-timer)
         ("C-c C-x :" . org-timer-cancel-timer))
  :init (require 'org-timer)
  :config
  (defun org-timer-set-timer (&optional opt) ;; redefine from org-timer.el
    "Prompt for a duration and set a timer.

If `org-timer-default-timer' is not zero, suggest this value as
the default duration for the timer.  If a timer is already set,
prompt the user if she wants to replace it.

Called with a numeric prefix argument, use this numeric value as
the duration of the timer.

Called with a `C-u' prefix arguments, use `org-timer-default-timer'
without prompting the user for a duration.

With two `C-u' prefix arguments, use `org-timer-default-timer'
without prompting the user for a duration and automatically
replace any running timer."
    (interactive "P")
    (let ((minutes (or (and (numberp opt) (number-to-string opt))
                       (and (listp opt) (not (null opt))
                            (number-to-string org-timer-default-timer))
                       (read-from-minibuffer
                        "How many minutes left? "
                        (if (not (eq org-timer-default-timer 0))
                            (number-to-string org-timer-default-timer))))))
      (if (not (string-match "[0-9]+" minutes))
          (org-timer-show-remaining-time)
        (let* ((mins (string-to-number (match-string 0 minutes)))
               (secs (* mins 60))
               (hl (cond
                    ((string-match "Org Agenda" (buffer-name))
                     (let* ((marker (or (get-text-property (point) 'org-marker)
                                        (org-agenda-error)))
                            (hdmarker (or (get-text-property (point) 'org-hd-marker)
                                          marker))
                            (pos (marker-position marker)))
                       (with-current-buffer (marker-buffer marker)
                         (widen)
                         (goto-char pos)
                         (org-show-entry)
                         (or (ignore-errors (org-get-heading))
                             (concat "File:" (file-name-nondirectory (buffer-file-name)))))))
                    ((derived-mode-p 'org-mode)
                     (or (ignore-errors (org-get-heading))
                         (concat "File:" (file-name-nondirectory (buffer-file-name)))))
                    (t (read-from-minibuffer "Task: " nil nil nil nil "Countdown task"))))
               timer-set)
          (if (or (and org-timer-current-timer
                       (or (equal opt '(16))
                           (y-or-n-p "Replace current timer? ")))
                  (not org-timer-current-timer))
              (progn
                (require 'org-clock)
                (when org-timer-current-timer
                  (cancel-timer org-timer-current-timer))
                (setq org-timer-current-timer
                      (run-with-timer
                       secs nil `(lambda ()
                                   (setq org-timer-current-timer nil)
                                   (org-notify ,(format "%s: time out" hl) ,org-clock-sound)
                                   (setq org-timer-timer-is-countdown nil)
                                   (org-timer-set-mode-line 'off)
                                   (run-hooks 'org-timer-done-hook))))
                (run-hooks 'org-timer-set-hook)
                (setq org-timer-timer-is-countdown t
                      org-timer-start-time
                      (time-add (current-time) (seconds-to-time (* mins 60))))
                (org-timer-set-mode-line 'on))
            (message "No timer set")))))))

;; add support for automatic org-files commits
(defvar my-org-commit-timer
  (run-at-time (format-time-string "%H:59" (current-time)) 3600 'org-save-all-org-buffers)
  "Org commit timer.")

(defface my-org-bold
  '((t (:weight bold :inherit font-lock-variable-name-face)))
  "The face used to highlight pair overlays.")

(defface my-org-italic
  '((t (:slant italic :inherit font-lock-variable-name-face)))
  "The face used to highlight pair overlays.")

(defface my-org-code
  '((t (:family "Consolas" :inherit font-lock-constant-face)))
  "The face used to highlight pair overlays.")

(defface my-org-math
  '((t (:foreground "burlywood")))
  "Face used to highlight math.")

(font-lock-add-keywords 'org-mode
                        `(("[[:space:]]+\\(\\$[^[:space:]].*?\\$\\)[^[:word:]]+"
                           1 'my-org-math)
                          ;; TODO: nefunguje ak je {... \n ...}
                          ("\\(?:^\\| \\)\\({.*?}\\)\\(?:$\\| \\|\\s.\\)"
                           1 (progn (put-text-property
                                     (match-beginning 1)
                                     (match-end 1)
                                     'face
                                     'shadow)
                                    (backward-char 1)
                                    nil))))

(bind-keys :map org-mode-map
  ("TAB" . smart-tab)
  ("C-e" . my-end-of-code-or-line)
  ("C-a" . my-back-to-indentation-or-beginning)
  ("C-c C-x r" . org-clock-remove-overlays)
  ;; TODO lepsia mapa pre "toggle prikazy?"
  ("C-c C-x L" . org-toggle-link-display)
  ("C-c R" . org-remove-occur-highlights)

  ("C-x n t" . my-org-narrow-to-top-heading)
  ("C-x n P" . my-org-narrow-to-project)
  ("C-x n N" . my-org-narrow-to-subtree)
  ("C-x n W" . my-org-widen)

  ("C-c M-`" . org-mark-ring-goto)

  ("C-c C-S-n" . my-org-add-sibling)
  ("C-c C-n" . outline-next-visible-heading))

(defun my-org-open-at-point (&optional arg)
  "Just like `org-open-at-point', but open link in this window."
  (interactive "P")
  (if (equal arg '(16))
      (org-open-at-point arg)
    (let ((current-prefix-argument nil))
      (if arg
          (org-open-at-point '(4))
        (let ((org-link-frame-setup (acons 'file 'find-file org-link-frame-setup)))
          (org-open-at-point '(4)))))))
(bind-key "C-c C-o" 'my-org-open-at-point org-mode-map)
(bind-key "C-c C-=" 'org-open-at-point org-mode-map)

(defun my-goto-current-clocked-task ()
  (interactive)
  (org-goto-marker-or-bmk org-clock-marker))
(bind-key "<f1> <f10>" 'my-goto-current-clocked-task)

(defun my-org-metacontrolreturn ()
  "Execute `org-meta-return' followed by `org-meta-right'.
This usually makes new item indented one level deeper."
  (interactive)
  (org-meta-return)
  (org-metaright))
(bind-key "<C-M-return>" 'my-org-metacontrolreturn)

;; Do I even need this?
(use-package org-table
  :defer t
  :config
  (progn
    ;; org/orgtbl bindings
    (defvar my-org-table-map)
    (define-prefix-command 'my-org-table-map)
    (bind-key "C-c t" 'my-org-table-map org-mode-map)
    (bind-key "C-c t" 'my-org-table-map orgtbl-mode-map)
    (bind-key "C-c t s" 'org-table-sort-lines org-mode-map)
    (bind-key "C-c t s" 'org-table-sort-lines orgtbl-mode-map)
    (defvar my-org-table-insert-map)
    (define-prefix-command 'my-org-table-insert-map)
    (bind-key "C-c t i" 'my-org-table-insert-map org-mode-map)
    (bind-key "C-c t i" 'my-org-table-insert-map orgtbl-mode-map)
    (bind-key "C-c t i i" 'orgtbl-insert-radio-table orgtbl-mode-map)
    (defvar my-org-table-delete-map)
    (define-prefix-command 'my-org-table-delete-map)
    (bind-key "C-c t d" 'my-org-table-delete-map org-mode-map)
    (bind-key "C-c t d" 'my-org-table-delete-map orgtbl-mode-map)

    (let ((bindings '(("C-c t i c" org-table-insert-column)
                      ("C-c t i r" org-table-insert-row)
                      ("C-c t d c" org-table-delete-column)
                      ("C-c t d r" org-table-kill-row)))
          (n 1000))
      (dolist (b bindings)
        (define-key org-mode-map (kbd (car b)) (cadr b))
        (org-defkey orgtbl-mode-map (kbd (car b)) (orgtbl-make-binding (cadr b) n (kbd (car b))))
        (setq n (1+ n))))))

(defun my-org-select-cell ()
  "Select the cell in org table the point is in."
  (interactive)
  (let ((b (save-excursion
             (re-search-forward "|")
             (backward-char 1)
             (skip-chars-backward " ")
             (point)))
        (e (save-excursion
             (re-search-backward "|")
             (forward-char 1)
             (skip-chars-forward " ")
             (point))))
    (push-mark b t t)
    (goto-char e)))
(bind-key "C-c t" 'my-org-select-cell org-mode-map)

(defun my-markdown-to-org-link (b e)
  (interactive "r")
  (goto-char b)
  (sp-down-sexp)
  (let ((desc (sp-get (sp--next-thing-selection 0)
                (buffer-substring-no-properties :beg :end)))
        (link (progn
                (sp-beginning-of-sexp 2)
                (sp-get (sp--next-thing-selection 0)
                  (buffer-substring-no-properties :beg :end)))))
    (delete-region b e)
    (insert (format "[[%s][%s]]" link desc))))

(defun my-org-make-numbered-list (beg end)
  (interactive "r")
  (string-rectangle beg end "- ")
  (beginning-of-line)
  (org-call-with-arg 'org-cycle-list-bullet 'previous)
  (org-call-with-arg 'org-cycle-list-bullet 'previous))
(bind-key "C-c 1" 'my-org-make-numbered-list org-mode-map)

;; custom filter bindings
(defvar my-org-filter-map)
(define-prefix-command 'my-org-filter-map)
(bind-key "C-c F" 'my-org-filter-map org-mode-map)

(defmacro my-org-custom-filter (tag key)
  (let ((filter-name (intern (concat "my-org-" (symbol-name tag) "-filter")))
        (filter-name-no-done (intern (concat "my-org-" (symbol-name tag) "-no-done-filter")))
        (filter-name-next (intern (concat "my-org-" (symbol-name tag) "-next-filter")))
        (filter-string (concat "+" (upcase (symbol-name tag))))
        (filter-string-no-done (concat "+" (upcase (symbol-name tag)) "-TODO=\"DONE\""))
        (filter-string-next (concat "+" (upcase (symbol-name tag)) "+TODO=\"NEXT\"")))
    `(progn
      (defun ,filter-name ()
        (interactive)
        (org-match-sparse-tree nil ,filter-string))
      (defun ,filter-name-no-done ()
        (interactive)
        (org-match-sparse-tree nil ,filter-string-no-done))
      (defun ,filter-name-next ()
        (interactive)
        (org-match-sparse-tree nil ,filter-string-next))
      (bind-key ,(concat "C-c F " key) ',filter-name org-mode-map)
      (bind-key ,(concat "C-c F " (upcase key)) ',filter-name-no-done org-mode-map)
      (bind-key ,(concat "C-c F M-" key) ',filter-name-next org-mode-map))))

(my-org-custom-filter books "b")
(my-org-custom-filter mov "m")

(load "files/org-project")

;;;_. Narrowing

(defun my-org-narrow-to-top-heading ()
  "Narrow to the top-most tree containing point."
  (interactive)
  (save-excursion
    (ignore-errors (while (outline-up-heading 1)))
    (org-narrow-to-subtree)))

;; abstract these three into a macro, maybe?
(defun my-org--narrow-to-subtree ()
  "Narrow to subtree at point and also set agenda restriction."
  (widen)
  (org-narrow-to-subtree)
  (save-restriction
    (org-agenda-set-restriction-lock)))

(defun my-org-narrow-to-subtree ()
  "Narrow to subtree at point and also set agenda restriction.

If invoked from agenda, narrow to the subtree of the task at
point and rebuild the agenda view."
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (org-get-at-bol 'org-hd-marker)
          (my-org--narrow-to-subtree))
        (when org-agenda-sticky
          (org-agenda-redo)))
    (my-org--narrow-to-subtree)))

(defun my-org--narrow-to-parent ()
  "Narrow to parent of subtree at point and also set agenda restriction."
  (widen)
  (save-excursion
    (org-up-heading-safe)
    (my-org--narrow-to-subtree)))

(defun my-org-narrow-to-parent ()
  "Narrow to parent of subtree at point and also set agenda restriction.

If invoked from agenda, narrow to the parent of the task at point
and rebuild the agenda view."
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (org-get-at-bol 'org-hd-marker)
          (my-org--narrow-to-parent))
        (when org-agenda-sticky
          (org-agenda-redo)))
    (my-org--narrow-to-parent)))

(defun my-org--narrow-to-project ()
  "Narrow to project at point and also set agenda restriction."
  (widen)
  (save-excursion
    (my-org-find-project-task)
    (my-org--narrow-to-subtree)))

(defun my-org-narrow-to-project ()
  "Narrow to project at point and also set agenda restriction.

If invoked from agenda, narrow to the project of the task at
point and rebuild the agenda view."
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (org-get-at-bol 'org-hd-marker)
          (my-org--narrow-to-project))
        (when org-agenda-sticky
          (org-agenda-redo)))
    (my-org--narrow-to-project)))

(defun my-org-widen ()
  "Remove agenda restrictions or widen the buffer."
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-agenda-remove-restriction-lock)
        (when org-agenda-sticky
          (org-agenda-redo)))
    (widen)
    (org-agenda-remove-restriction-lock)))

;;;_. Capture
;; Capture templates for: TODO tasks, Notes, appointments, phone calls, and org-protocol
(setq org-capture-templates
      `(("t" "todo" entry (file "~/org/refile.org")
         "* TODO %?\n%U\n" :clock-keep t)
        ,@(let ((targets '(("e" "todo-emacs" "emacs" "Emacs config")
                           ("g" "todo-general" "me" "General")
                           ("l" "todo-linux" "me" "Linux")
                           ("h" "todo-home" "home" "General"))))
            (--map
             `(,(nth 0 it) ,(nth 1 it) entry (file+headline ,(concat "~/org/" (nth 2 it) ".org") ,(nth 3 it))
               "* TODO %?\n%U\n" :clock-keep t)
             targets))
        ("w" "todo-logio" entry (file "~/logio/logio.org")
         "* TODO %?\n%U\n" :clock-keep t)
        ("b" "bookmark" entry (file+function "~/org/bookmarks.org" my-org-handle-bookmark)
         "* %:description\n- %:link\n%(if (not (equal %:initial \"\"))
                                        (concat \"- \" %:initial) \"\")")
        ("c" "contact" entry (file "~/org/contacts.org")
         "* %(org-contacts-template-name)
    :PROPERTIES:
    :EMAIL: %(org-contacts-template-email)
    :END:")))

(defun my-org-handle-bookmark ()
  (let ((link (caar org-stored-links)))
    (cond
     ;; add handlers for various categories here
     (t
      (goto-char (point-max))
      (newline)))))

;; Remove empty LOGBOOK drawers on clock out
(defun my-org-remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-hook 'org-clock-out-hook 'my-org-remove-empty-drawer-on-clock-out 'append)

;;;; Refile settings
;; Exclude DONE state tasks from refile targets
(defun my-org-verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

;; TODO KEYWORDS SETTINGS
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)" "MOVE(m@)")
        (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "STOP(s@/!)")
        (sequence "IDEA(i)" "|")))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "IndianRed1" :weight bold)
              ("NEXT" :foreground "RoyalBlue" :weight bold)
              ("DONE" :foreground "LimeGreen" :weight bold)
              ("WAIT" :foreground "orange" :weight bold)
              ("HOLD" :foreground "orange" :weight bold)
              ("STOP" :foreground "LimeGreen" :weight bold)
              ("IDEA" :foreground "pink" :weight bold))))

(setq org-todo-state-tags-triggers
      (quote (("STOP" ("STOP" . t))
              ("WAIT" ("WAIT" . t))
              ("HOLD" ("HOLD" . t))
              (done ("WAIT") ("HOLD"))
              ("TODO" ("WAIT") ("STOP") ("HOLD"))
              ("IDEA" ("WAIT") ("STOP") ("HOLD"))
              ("NEXT" ("WAIT") ("STOP") ("HOLD"))
              ("DONE" ("WAIT") ("STOP") ("HOLD")))))

;; Tags shortcuts
(setq org-tag-alist (quote ((:startgroup)
                            ("Errand" . ?e)
                            ("School" . ?s)
                            ("Home" . ?h)
                            (:endgroup)
                            (:startgroup)
                            ("DE" . ?D)
                            ("IT" . ?I)
                            ("LA" . ?L)
                            ("RU" . ?R)
                            ("FR" . ?F)
                            ("ES" . ?E)
                            ("IL" . ?H)
                            ("SA" . ?S)
                            ("PL" . ?P)
                            ("JP" . ?J)
                            (:endgroup)
                            ("Reading" . ?r)
                            ("CS" . ?c)
                            ("FP" . ?f)
                            ("Langs" . ?l))))

(defun my-org-compare-closed-entries (a b)
  "Compare two agenda entries A and B based on CLOSED time."
  (let ((closed-a (org-time-string-to-time (org-entry-get (get-text-property 1 'org-marker a) "CLOSED")))
        (closed-b (org-time-string-to-time (org-entry-get (get-text-property 1 'org-marker b) "CLOSED"))))
    (cond
     ((equal closed-a closed-b) nil)
     ((time-less-p closed-a closed-b) -1)
     (t +1))))

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)

(defun my-org-clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.

Skips capture tasks, standalone tasks, projects, and subprojects.

Switch projects and subprojects from NEXT back to TODO"
  (when (not (bound-and-true-p org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (my-org-is-task-p)
           (not (my-org-is-standalone-task-p))
           (not (org-is-habit-p)))
      "NEXT")
     ;; Handles the case when we clock in into a project.  We want
     ;; projects to always have TODO keyword
     ((and (member (org-get-todo-state) (list "NEXT"))
           (my-org-is-project-p))
      "TODO"))))

(defun my-org-export-read-books-do-export (buf)
  "Buf is the buffer into which the export is written."
  (goto-char (point-min))
  (forward-line)
  (let ((index 0))
    (with-current-buffer buf
      (insert "|-+-+-+-+-+-|\n")
      (insert "| Index | Language | Title | Published | Author | Original Title |\n")
      (insert "|-+-+-+-+-+-|\n"))
    (while (and (= 0 (forward-line))
                (not (eobp)))
      (cl-incf index)
      (org-with-point-at (org-get-at-bol 'org-hd-marker)
        (let* ((element (cadr (org-element-at-point)))
               (title (plist-get element :title))
               (author (plist-get element :AUTHOR))
               (published (plist-get element :PUBLISHED))
               (original-title (plist-get element :ORIGINAL_TITLE))
               (language (cdr (assoc (let* ((tags (org-get-tags-at))
                                            (language-tag
                                             (car (-intersection tags '("DE" "IT" "LA" "RU" "FR" "ES" "IL" "SA" "PL" "JP"))))
                                            (language-prop (plist-get element :LANGUAGE)))
                                       (or language-prop language-tag "EN"))
                                     '(("DE" . "German")
                                       ("IT" . "Italian")
                                       ("LA" . "Latin")
                                       ("RU" . "Russian")
                                       ("FR" . "French")
                                       ("ES" . "Spanish")
                                       ("IL" . "Hebrew")
                                       ("SA" . "Sanskrit")
                                       ("PL" . "Polish")
                                       ("JP" . "Japanese")
                                       ("CS" . "Czech")
                                       ("SK" . "Slovak")
                                       ("EN" . "English"))))))
          (with-current-buffer buf
            ;; num, lan, title, published, author, orig. title
            (insert (format "| %d. | %s | %s | %s | %s | %s |\n"
                            index
                            language
                            title
                            published
                            author
                            (or original-title "")))))))
    (with-current-buffer buf
      (insert "|-+-+-+-+-+-|\n")
      (save-excursion
        (forward-line -1)
        (org-table-align)))))

;; TODO: scope agenda-files to "me" and "books"
(defun my-org-export-read-books ()
  (interactive)
  (save-window-excursion
    (unwind-protect
        (let ((buf (get-buffer-create "*org-books-export*"))
              (my-org-show-media-closed-since
               (apply 'encode-time (org-parse-time-string "2014-01-01")))
              (org-agenda-sticky nil))
          (with-current-buffer buf
            (erase-buffer)
            (org-mode))
          (org-agenda nil "fdb")
          (my-org-export-read-books-do-export buf)
          (org-agenda nil "fb")
          (with-current-buffer buf
            (insert "\n* Reading\n\n"))
          (my-org-export-read-books-do-export buf)
          (with-current-buffer buf
            (org-export-to-file 'html "~/books.html"))
          (copy-file "~/books.html" "/fuco@dasnet.cz:/home/fuco/books.html" t))
      (kill-buffer "*org-books-export*"))))


;; navigation & header manipulation

(defun my-org-next-parent-sibling ()
  (condition-case err
      (progn
        (outline-up-heading 1)
        (outline-get-next-sibling))
    (error
     (goto-char (point-max)))))

;; TODO: breaks if we're adding the headline as last headline in the
;; file
(defun my-org-add-sibling (&optional arg)
  "Add new sibling depending on context.

If the point is inside a header and:

- if next header is deeper, skip headers until point is at the same
level and add a sibling there.  With prefix argument
\\[universal-argument], add a sibling before the next header that
is less deep than current.

- if the next header is the same depth, skip until the first less
deep header and add a sibling before it.  With prefix argument
\\[universal-argument], add a sibling after the current one.

- if the next header is less deep than the current one, insert a
sibling before the next header."
  (interactive "P")
  (org-back-to-heading)
  (let ((cdepth (plist-get (cadr (org-element-at-point)) :level))
        (ndepth (save-excursion
                  (outline-next-heading)
                  (plist-get (cadr (org-element-at-point)) :level))))
    (cond
     ((and (< cdepth ndepth) arg)
      (my-org-next-parent-sibling))
     ((< cdepth ndepth)
      (outline-get-next-sibling))
     ((and (= cdepth ndepth) arg)
      (outline-next-heading))
     ((= cdepth ndepth)
      (my-org-next-parent-sibling))
     ((> cdepth ndepth)
      (outline-next-heading)))
    (if (= (point) (point-max)) (newline) (open-line 1))
    (insert (make-string cdepth ?*) " ")))


;; better org-agenda s/m UI
;; TODO: move to defun-macros
(defmacro my-with-each-line (&rest body)
  (declare (indent 0)
           (debug (body)))
  `(save-excursion
     (goto-char (point-min))
     ,@body
     (while (= (forward-line) 0)
       ,@body)))


;; tags
(defun my-org-add-tags-at (tags)
  "Add TAGS to current entry."
  (let* ((ctags (org-get-local-tags-at))
         (tags (-union tags ctags)))
    (org-set-tags-to tags)))

(defun my-org-remove-tags-at (tags)
  "Remove TAGS from current entry."
  (let* ((ctags (org-get-local-tags-at))
         (tags (-difference ctags tags)))
    (org-set-tags-to tags)))

(defvar my-org-tag-ontology '(("jedi" "starwars")
                              ("startrek" "scifi")
                              ("starwars" "war" "scifi"))
  "An alist specifying an \"implication\" relation between tags.

The `car' is a tag and the `cdr' is a list of its hypernym tags.

For example, (\"starwars\" \"scifi\" \"space\") means that each
headline tagged with either \"starwars\" tag automatically
implies tags \"scifi\" and \"space\".  You can read this as:

\"starwars\" implies \"scifi\" and \"space\".

The relation is transitive.  If we have another implication
relation (\"jedi\" \"starwars\"), any headline tagged with
\"jedi\" will imply tags \"starwars\", \"scifi\" and \"space\".")

;; TODO: use the better linear time algorithm (like the "depth of tree from array")
(defun my-org-resolve-ontology (tags)
  "Return TAGS with all the parent tags according to current ontology."
  (--fix
   (-uniq
    (--mapcat
     (cons it (--map (org-add-prop-inherited (copy-sequence it))
                     (cdr (assoc it my-org-tag-ontology))))
     it))
   tags))

;; we also had to patch org-scan-tags: tags-alist has to be modified
;; at the correct level to add "included" tags
;; -(setq tags (org-split-string tags ":")
;; +(setq tags (my-org-resolve-ontology (org-split-string tags ":"))
(defadvice org-get-tags-at (around add-tags-ontology activate)
  ad-do-it
  (unless (ad-get-arg 1)
    (setq ad-return-value (my-org-resolve-ontology ad-return-value))))


;; more org macros
(defmacro my-org-with-children (&rest body)
  "Execute BODY with point at the beginning of each child of current node."
  (declare (indent 0)
           (debug (body)))
  `(let ((start-level (org-current-level)))
     (while (and (outline-next-heading)
                 (> (org-current-level) start-level))
       (when (= (org-current-level) (1+ start-level))
         ,@body))))

(defmacro my-org-with-descendants (&rest body)
  "Execute BODY with point at the beginning of each descendant of current node."
  (declare (indent 0)
           (debug (body)))
  `(let ((start-level (org-current-level)))
     (while (and (outline-next-heading)
                 (> (org-current-level) start-level))
       ,@body)))


;; subtree manipulation
;; See: org-scan-tags
(defun my-org-copy-trees (query target-buffer)
  "Copy all headlines matching QUERY to TARGET-BUFFER."
  (let* ((todo-only nil)
         (matcher (org-make-tags-matcher query))
         (kill-ring kill-ring))
    (save-restriction
      (widen)
      (org-map-region
       (lambda ()
         (let ((tags-list (org-get-tags-at))
               (todo (org-get-todo-state)))
           (when (eval (cdr matcher))
             (org-copy-subtree)
             (with-current-buffer target-buffer
               (goto-char (point-max))
               (unless (looking-back "^")
                 (newline))
               (yank)))))
       (point-min) (point-max)))))

(defun my-org-delete-trees (query)
  "Delete all headlines matching QUERY."
  (let* ((todo-only nil)
         (matcher (org-make-tags-matcher query))
         (kill-ring kill-ring)
         (size 0))
    (save-restriction
      (widen)
      (while (/= size (buffer-size))
        (setq size (buffer-size))
        (org-map-region
         (lambda ()
           (let ((tags-list (org-get-tags-at))
                 (todo (org-get-todo-state)))
             (when (eval (cdr matcher))
               (org-cut-subtree))))
         (point-min) (point-max))))))

(defun my-org-move-trees (query target-buffer)
  "Move all headlines matching QUERY to TARGET-BUFFER."
  (my-org-copy-trees query target-buffer)
  (my-org-delete-trees query))
