;; -*- lexical-binding: t -*-

;; What to do after update:
;; - check if `org-agenda-format-item' properly renders timestamps with hour-ranges
;;   This piece of code needs to be augumented
;;   (setq s0 (match-string 0 ts)
;;                     srp (and stamp (match-end 3))
;;                     s1 (match-string (if plain 1 2) ts)
;;                     s2 (or (match-string (if plain 8 (if srp 4 6)) ts)
;;                            ;; FUCO: test if s1 is a range
;;                            (save-match-data
;;                              (when (string-match "[012][0-9]:[0-5][0-9]--\\([012][0-9]:[0-5][0-9]\\)" s1)
;;                                (match-string 1 s1)))))
;;
;; - in org-capture, replace (error "Abort") with (user-error "Abort"),
;;   this fixes the annoying crap q/C-g does in template selection buffer

;; Reading
;; - http://doc.norang.ca/org-mode.html

;; Remember to put :LOGGING: DONE(!) property on habit tasks. That way
;; we can "cancel" them for today by setting todo state to STOP, but
;; they wont log the state change into logbook so as not to mess up
;; the graph

(require 'org)

(require 'hydra)

(use-package orgba :straight (:repo "git@github.com:Fuco1/orgba.git"))
(use-package org-make-toc :straight t)

(use-package org-id
  :config
  (require 'org-element)
  (org-link-set-parameters "id" :store #'org-id-store-link))

(use-package org-inlinetask)

(use-package elfeed-link)

(use-package org-cliplink :straight t)

(use-package org-clock
  :commands (
             org-clock-in
             org-clock-out
             org-clock-goto
             ))

(use-package org-toggl
  :after org-clock
  :straight (org-toggl :repo "git@github.com:Fuco1/org-toggl.git")
  :custom
  (org-toggl-inherit-toggl-properties t)
  :config
  (toggl-get-projects)
  (org-toggl-integration-mode))

;; TODO: replace with proper package
(load (f-join (f-parent (f-this-file)) "org-project"))

(use-package org-depend
  :config
  (progn
    (defun my-org-block-next-change-on-dependency (_change-plist)
      "If the blocker is not `done', do not allow switch to `todo'."
      (-if-let* ((blocker-id (org-entry-get (point) "BLOCKER"))
                 (blocker (org-id-find blocker-id)))
          (org-with-point-at blocker
            (if (member (org-get-todo-state) (cons 'done org-done-keywords)) t
              (setq org-block-entry-blocking (org-get-heading))
              nil))
        t))
    (add-hook 'org-blocker-hook 'my-org-block-next-change-on-dependency)

    (defun my-org-add-blocker ()
      "Add a BLOCKER property to current task."
      (interactive)
      (let* ((org-refile-targets '((nil :maxlevel . 9)))
             id)
        (-when-let (target (org-refile-get-location "Blocker"))
          (org-with-point-at (-last-item target)
            (setq id (org-id-get-create)))
          ;; TODO: make the BLOCKER property multivalued
          (org-set-property "BLOCKER" id))))

    (defun my-org-goto-blocker ()
      "Goto blocker of this task."
      (interactive)
      (-when-let (blocker (org-entry-get (point) "BLOCKER"))
        (goto-char (org-find-entry-with-id blocker))))

    (defun my-org-add-trigger (target state)
      "Add a TRIGGER property to current task."
      (interactive
       (list (let ((org-refile-targets '((nil :maxlevel . 9))))
               (org-refile-get-location "Trigger task"))
             (completing-read "to state: " (-concat org-done-keywords org-not-done-keywords))))
      (let* ((id (org-with-point-at (-last-item target)
                   (org-id-get-create))))
        (org-set-property "TRIGGER" (format "%s(%s)" id state))))

    (defun my-org-goto-trigger ()
      "Goto blocker of this task."
      (interactive)
      (-when-let (trigger (org-entry-get (point) "TRIGGER"))
        (goto-char (org-find-entry-with-id (car (split-string trigger "(" t))))))

    (bind-key "C-x C-d"
              (defhydra org-depend-hydra (:color blue)
                "Org dependencies management."
                ("b" my-org-add-blocker "Add blocker")
                ("g" my-org-goto-blocker "Goto blocker")
                ("t" my-org-add-trigger "Add trigger")
                ("v" my-org-goto-trigger "Goto trigger"))
              org-mode-map)))

(use-package org-drill
  :commands org-drill
  :bind (:map org-mode-map
         ("H-d" . org-drill)
         ("H-r" . org-drill-resume)
         ("H-a" . org-drill-again))
  :config
  (progn
    (use-package pcase)

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
          (my-org-make-numbered-list start end))))

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
           (org-drill-hide-subheadings-if 'org-drill-entry-p)))))

    (defun my-org-drill-remove-empty-notes-examples ()
      "Remove empty subtrees."
      (let ((kill-ring kill-ring))
        (org-map-entries
         (lambda ()
           (when (and (org-entry-empty-p)
                      (> (org-outline-level) 1))
             (org-cut-subtree))))))))

;; To fix the stickyness issue with incorrectly named buffers (just
;; (m) instead of (m/keys:query)):
;; in org-tags-view, after "catch 'exit", put
;; (unless match
;;   (setq match (org-completing-read-no-i
;;                "Match: " 'org-tags-completion-function nil nil nil
;;                'org-tags-history)))
(add-hook 'org-agenda-finalize-hook 'my-org-agenda-remove-duplicate-habits)
(use-package org-agenda
  :defer t
  :config
  (use-package org-timeline
    :straight (:repo "git@github.com:Fuco1/org-timeline.git")
    :config
    (progn
      (add-hook 'org-agenda-finalize-hook 'org-timeline-insert-timeline :append)))

  (use-package org-super-agenda
    :straight t
    :config
    (progn
      (org-super-agenda-mode 1)))

  ;; Custom agenda command definitions
  (defvar my-org-show-media-closed-since (apply 'encode-time (org-parse-time-string "1980-01-01"))
    "Time since which we show the closed media")

  (defvar my-org-show-media-closed-until (apply 'encode-time (org-parse-time-string "9999-99-99"))
    "Time until which we show the closed media")

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
                                          ;; TODO: for some reason here we get a bug "Bad timestamp"
                                          (-when-let* ((ts (org-entry-get (point) "CLOSED"))
                                                       (closed-at (org-time-string-to-time ts)))
                                            (when (or (time-less-p closed-at
                                                                   my-org-show-media-closed-since)
                                                      (time-less-p my-org-show-media-closed-until
                                                                   closed-at))
                                              next-headline)))))))
         args)))

  (defconst my-project-files '(
                               "~/org/emacs.org"
                               "~/org/me.org"
                               "~/org/saleschamp.org"
                               "~/org/work.org"
                               ))

  (defconst my-custom-agenda-sections
    `((refile . (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-agenda-files '("~/org/refile.org"))
                       (org-tags-match-list-sublevels nil))))
      (super-agenda . (agenda ""
                              ((org-super-agenda-groups
                                '(
                                  (:name "MIT"
                                   :tag "#mit"
                                   :order 1)
                                  (:name "Weekly"
                                   :tag "#weekly"
                                   :order 1)
                                  (:name "Monthly"
                                   :tag "#monthly"
                                   :order 1)
                                  (:name "Habits"
                                   :and (:habit t
                                         :not (:time-grid))
                                   :order 11)
                                  (:name "Deadlines"
                                   :and (:deadline future)
                                   :order 20)
                                  (:name "Scheduled"
                                   :and (:scheduled past
                                         :not (:habit)
                                         :not (:time-grid)
                                         :not (:tag "piano"))
                                   :order 30)
                                  (:name "Scheduled - piano practice"
                                   :and (:scheduled past
                                         :tag "piano"
                                         :not (:habit)
                                         :not (:time-grid))
                                   :order 35)
                                  )))))
      (bugs-emacs . (tags-todo "bug/!-NEXT"
                               ((org-agenda-overriding-header "Bugs (in emacs projects)")
                                (org-agenda-files '("~/org/emacs.org"))
                                (org-tags-match-list-sublevels nil))))
      (stuck-projects . (tags-todo "-STOP-HOLD/!-WAIT"
                                   ((org-agenda-overriding-header "Stuck Projects")
                                    (org-agenda-files ',my-project-files)
                                    (org-agenda-skip-function 'my-org-skip-non-stuck-projects))))
      (next-tasks . (tags-todo "-WAIT-HOLD-STOP-BOOKS-BUG/!NEXT"
                               ((org-agenda-overriding-header "Next Tasks")
                                (org-agenda-skip-function 'my-org-skip-projects-and-habits-and-single-tasks)
                                (org-agenda-todo-ignore-scheduled t)
                                (org-agenda-todo-ignore-deadlines t)
                                (org-agenda-todo-ignore-with-date t)
                                (org-tags-match-list-sublevels t)
                                (org-agenda-sorting-strategy '(priority-down todo-state-down effort-up category-keep)))))
      (tasks . (tags-todo "-REFILE-STOP-BOOKS-MOV-download-readlater-watchlater/!-HOLD-WAIT-IDEA-CONF"
                          ((org-agenda-overriding-header "Tasks")
                           (org-agenda-skip-function 'my-org-skip-project-tasks-maybe)
                           (org-agenda-todo-ignore-scheduled t)
                           (org-agenda-todo-ignore-deadlines t)
                           (org-agenda-todo-ignore-with-date t)
                           (org-agenda-sorting-strategy '(priority-down category-keep)))))
      (tasks-to-discuss . (tags-todo "/!CONF"
                                     ((org-agenda-overriding-header "Tasks to discuss")
                                      (org-agenda-todo-ignore-scheduled t)
                                      (org-agenda-todo-ignore-deadlines t)
                                      (org-agenda-todo-ignore-with-date t)
                                      (org-agenda-sorting-strategy '(priority-down category-keep)))))
      (waiting-tasks . (tags-todo "-STOP/!+WAIT"
                                  ((org-agenda-overriding-header "Waiting Tasks")
                                   (org-agenda-skip-function 'my-org-skip-projects)
                                   (org-tags-match-list-sublevels nil)
                                   (org-agenda-todo-ignore-scheduled 'future)
                                   (org-agenda-todo-ignore-deadlines 'future))))
      ;; Active projects and projects that wait on something
      ;; Things we are working on
      ;; TODO: should show immediate children tasks if narrowed
      (projects . (tags-todo "-HOLD-STOP-GENERAL/!"
                             ((org-agenda-overriding-header (if (my-org-restricted-p)
                                                                "Subprojects (and children tasks)"
                                                              "Projects"))
                              (org-agenda-files ',my-project-files)
                              (org-agenda-skip-function 'my-org-skip-non-projects)
                              (org-tags-match-list-sublevels 'indented)
                              (org-agenda-sorting-strategy '(priority-down category-keep)))))
      ;; Projects/tasks on HOLD: projects that are not cancelled, but we don't want to work on them now
      (hold . (tags-todo "-STOP/!+HOLD"
                         ((org-agenda-overriding-header "Postponed Projects and Tasks")
                          (org-tags-match-list-sublevels nil)
                          (org-agenda-todo-ignore-scheduled 'future)
                          (org-agenda-todo-ignore-deadlines 'future)))))
    "An alist mapping keywords to custom agenda sections")

  (setq org-agenda-custom-commands
        `((" " "Quick Agenda"
           (,(cdr (assoc 'super-agenda my-custom-agenda-sections))
            ,(cdr (assoc 'refile my-custom-agenda-sections))
            ,(cdr (assoc 'next-tasks my-custom-agenda-sections))
            ,(cdr (assoc 'tasks my-custom-agenda-sections))
            ,(cdr (assoc 'tasks-to-discuss my-custom-agenda-sections))))
          ("A" "Full Agenda"
           (,(cdr (assoc 'super-agenda my-custom-agenda-sections))
            ,(cdr (assoc 'refile my-custom-agenda-sections))
            ,(cdr (assoc 'bugs-emacs my-custom-agenda-sections))
            ,(cdr (assoc 'stuck-projects my-custom-agenda-sections))
            ,(cdr (assoc 'next-tasks my-custom-agenda-sections))
            ,(cdr (assoc 'tasks my-custom-agenda-sections))
            ,(cdr (assoc 'tasks-to-discuss my-custom-agenda-sections))
            ,(cdr (assoc 'waiting-tasks my-custom-agenda-sections))
            ,(cdr (assoc 'projects my-custom-agenda-sections))
            ,(cdr (assoc 'hold my-custom-agenda-sections))))
          ("t" "Todo" tags "-HOLD-STOP-BOOKS-download/!+NEXT|+TODO"
           ((org-agenda-skip-function 'my-org-skip-projects)
            (org-agenda-todo-ignore-scheduled t)
            (org-agenda-todo-ignore-deadlines t)
            (org-agenda-todo-ignore-with-date t)))
          ("e" "Emacs config"
           ((todo "TODO|NEXT"
                  ((org-agenda-overriding-header "Emacs config")
                   (org-agenda-files '("~/.emacs.d"
                                       "~/.emacs.d/files"
                                       "~/.emacs.d/site-lisp"))))
            (tags "to_publish"
                  ((org-agenda-overriding-header "Available for publication")
                   (org-use-tag-inheritance nil)
                   (org-agenda-files '("~/.emacs.d"
                                       "~/.emacs.d/files"
                                       "~/.emacs.d/site-lisp"))))
            (tags "published"
                  ((org-agenda-overriding-header "Published")
                   (org-use-tag-inheritance nil)
                   (org-agenda-files '("~/.emacs.d"
                                       "~/.emacs.d/files"
                                       "~/.emacs.d/site-lisp"))))))
          ,@(my-org-agenda-filter "f" "Media filter" '("b" . "BOOKS") '("m" . "MOV"))
          ("k" . "Knowledge-base operations")
          ("ks" "Knowledge-base search" search nil
           ((org-agenda-files '("~/data/documents/kb.org"))))
          ("km" "Knowledge-base tag match" tags nil
           ((org-agenda-files '("~/data/documents/kb.org"))))
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
           ((tags "+readlater-BOOKS/-DONE"
                  ((org-agenda-overriding-header "Read later")))
            (tags "+Reading+BOOKS-readlater-tosort/-DONE-NEXT"
                  ((org-agenda-overriding-header "To read")
                   (org-agenda-files '("~/org/reading.org"))))
            (tags "+Reading+tosort"
                  ((org-agenda-overriding-header "Sort")
                   (org-agenda-files '("~/org/reading.org"))))
            (tags "+Reading/DONE"
                  ((org-agenda-overriding-header "Finished")
                   (org-agenda-files '("~/org/reading.org"))))))))

  (defun my-org-agenda-remove-duplicate-habits ()
    "Remove duplicated habits from agenda.

If a task is scheduled multiple times for different days and is
overdue and a habit it is inserted multiple times."
    (save-excursion
      (let ((limit (save-excursion
                     (goto-char (point-min))
                     (while (and (eq (org-get-at-bol 'org-agenda-type) 'agenda)
                                 (= (forward-line 1) 0)))
                     (point-marker)))
            (visited nil))
        (goto-char (point-min))
        (while (< (point) limit)
          (let ((p (--when-let (org-get-at-bol 'org-marker)
                     (org-with-point-at it
                       (save-excursion
                         (ignore-errors
                           (org-back-to-heading t)
                           (point-marker)))))))
            (if (and p
                     (equal (org-entry-get p "STYLE") "habit")
                     (not (--when-let (org-get-at-bol 'extra)
                            (string-match-p "^Clocked" it))))
                (if (member p visited)
                    (delete-region (point-at-bol) (1+ (point-at-eol)))
                  (push p visited)
                  (forward-line 1))
              (forward-line 1))))
        (--each visited (set-marker it nil)))))

  (add-hook 'org-agenda-finalize-hook 'my-org-agenda-remove-duplicate-habits)
  (add-hook 'org-agenda-finalize-hook 'my-org-agenda-remove-duplicate-habits)

  (defun org-agenda-time-limit (time)
    "Call `org-agenda' with media timestamp limited."
    (interactive "sTimestamp: ")
    (let ((my-org-show-media-closed-since
           (apply 'encode-time (org-parse-time-string time))))
      (org-agenda)))

  ;; View
  ;; TODO: replace with orgba-agenda-is-task-p
  (defun my-org-agenda-is-task-p ()
    "Return non-nil if line at point is a task."
    (org-get-at-bol 'org-marker))

  (defun my-org-agenda-remove-empty-lists ()
    (let ((headers '("Tasks to Refile"
                     "Bugs (in emacs projects)"
                     "Stuck Projects"
                     "Next Tasks"
                     "Tasks"
                     "Tasks to discuss"
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
  ;; TODO: add to orgba as interactive
  (defun my-org-agenda-open-at-point (&optional arg)
    "Open the first link after the headline under point."
    (interactive "P")
    (org-with-point-at (org-get-at-bol 'org-hd-marker)
      (org-next-link)
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
      (setq-local org-global-tags-completion-table
                  (-uniq (-map 'downcase
                               (-concat (my-org--get-agenda-tags)
                                        (-filter 'stringp (-map 'car org-tag-alist)))))))
    (org-agenda-filter-by-tag-refine strip char))

  (defun my-org-agenda-filter-by-tag (strip &optional char narrow)
    "Just like `org-agenda-filter-by-tag' but with tags from
current agenda view added to `org-tag-alist'."
    (interactive "P")
    (unless (local-variable-p 'org-global-tags-completion-table (current-buffer))
      (setq-local org-global-tags-completion-table
                  (-uniq (-map 'downcase
                               (-concat (my-org--get-agenda-tags)
                                        (-filter 'stringp (-map 'car org-tag-alist)))))))
    (org-agenda-filter-by-tag strip char narrow))

  (defun my-org-agenda-clockreport (from to)
    "Standalone clockreport"
    (interactive (list (org-read-date nil nil nil "From: " nil (format-time-string "%Y-01-01"))
                       (org-read-date nil nil nil "To: ")))
    (let ((org-hide-emphasis-markers nil)
          (org-agenda-files (org-agenda-files nil 'ifmode))
          ;; the above line is to ensure the restricted range!
          (p (copy-sequence org-agenda-clockreport-parameter-plist))
          tbl)
      (setq p (org-plist-delete p :block))
      (setq p (plist-put p :tstart from))
      (setq p (plist-put p :tend to))
      (setq p (plist-put p :scope 'agenda))
      (setq tbl (apply 'org-clock-get-clocktable p))
      (with-current-buffer (get-buffer-create (format "*clockreport %s--%s*" from to))
        (erase-buffer)
        (insert tbl)
        (goto-char (point-min))
        (pop-to-buffer (current-buffer)))))

  (bind-keys :map org-agenda-mode-map
    ("C-c r" . my-org-agenda-clockreport)
    ("C-c g" . org-clock-budget-report)
    ("C-n" . org-agenda-next-item)
    ("C-p" . org-agenda-previous-item)
    ("P" . my-org-narrow-to-project)
    ("U" . my-org-narrow-to-parent)
    ("N" . my-org-narrow-to-subtree)
    ("W" . my-org-widen)
    ("/" . my-org-agenda-filter-by-tag)
    ("\\" . my-org-agenda-filter-by-tag-refine)
    ("o" . my-org-agenda-open-at-point)))

(use-package org-notmuch)

(use-package org-contacts)

(use-package ox-publish
  :init
  (autoload
    'my-org-publish
    "~/.emacs.d/files/org-blog.el"
    "Publish current subtree." t)
  :bind (:map org-mode-map
         ("C-c P" . my-org-publish) )
  :config
  (use-package ox-extra
    :config
    (ox-extras-activate '(ignore-headlines))))

(use-package org-timer
  :bind (("C-c C-x ;" . org-timer-set-timer)
         ("C-c C-x :" . org-timer-cancel-timer)))

(use-package org-tree-slide
  :commands org-tree-slide-mode
  :init
  (progn
    (bind-key "C-c C-x s" 'org-tree-slide-mode org-mode-map))
  :config
  (progn
    (defun my-org-hide-block-metadata-lines ()
      "Hide metadata lines for org blocks."
      (ov-clear 'my-org-hide-block-metadata)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (while (re-search-forward org-block-regexp nil t)
            (save-excursion
              (let ((beg (match-beginning 1))
                    (end (match-end 0)))
                (goto-char beg)
                (ov (line-beginning-position) (1+ (line-end-position)) 'invisible t 'my-org-hide-block-metadata t)
                (goto-char end)
                (ov (line-beginning-position) (line-end-position) 'invisible t 'my-org-hide-block-metadata t)))))))

    (defun my-org-hide-name-metadata-lines ()
      "Hide #+NAME: lines."
      (ov-clear 'my-org-hide-name-metadata)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (while (re-search-forward "^#\\+NAME:" nil t)
            (ov (line-beginning-position) (1+ (line-end-position)) 'invisible t 'my-org-hide-name-metadata t)))))

    (defun my-org-tree-slide-init ()
      "Init hook for org-tree-slide-mode."
      (my-org-hide-block-metadata-lines)
      (my-org-hide-name-metadata-lines))

    (add-hook 'org-tree-slide-play-hook 'my-org-tree-slide-init)

    (defun my-org-tree-slide-stop ()
      "Hook run when presentation is stopped."
      (ov-clear 'my-org-hide-name-metadata)
      (ov-clear 'my-org-hide-block-metadata))

    (add-hook 'org-tree-slide-stop-hook 'my-org-tree-slide-stop)

    (bind-keys :map org-tree-slide-mode-map
      ("<right>" . org-tree-slide-move-next-tree)
      ("<left>" . org-tree-slide-move-previous-tree)
      ("<up>" . org-tree-slide-content)
      ("<down>" . org-tree-slide-mode))))

(use-package ox-latex
  :defer t
  :config
  (setq org-latex-listings t)

  (defadvice org-latex-export-to-latex (before set-environment activate)
    (my-auto-tex-parameters)
    (my-auto-tex-cmd))

  ;; Originally taken from Bruno Tavernier: http://thread.gmane.org/gmane.emacs.orgmode/31150/focus=31432
  ;; but adapted to use latexmk 4.20 or higher.
  (defun my-auto-tex-cmd ()
    "When exporting from .org with latex, automatically run latex,
     pdflatex, or xelatex as appropriate, using latexmk."
    (let (texcmd)
      ;; default command: oldstyle latex via dvi
      (setq texcmd "latexmk -dvi -pdfps -quiet %f")
      ;; pdflatex -> .pdf
      (if (string-match "LATEX_CMD: pdflatex" (buffer-string))
          (setq texcmd "latexmk -pdf -quiet %f"))
      ;; xelatex -> .pdf
      (if (string-match "LATEX_CMD: xelatex" (buffer-string))
          (setq texcmd "latexmk -pdflatex=xelatex -pdf -quiet %f"))
      ;; LaTeX compilation command
      (setq org-latex-to-pdf-process (list texcmd))))

  ;; Specify default packages to be included in every tex file, whether pdflatex or xelatex
  (setq org-latex-packages-alist
        '(("" "graphicx" t)
          ("" "longtable" nil)
          ("" "float" nil)))

  (defun my-auto-tex-parameters ()
    "Automatically select the tex packages to include."
    ;; default packages for ordinary latex or pdflatex export
    (setq org-latex-default-packages-alist
          '(("AUTO" "inputenc" t)
            ("T1"   "fontenc"   t)
            (""     "fixltx2e"  nil)
            (""     "wrapfig"   nil)
            (""     "soul"      t)
            (""     "textcomp"  t)
            (""     "marvosym"  t)
            (""     "wasysym"   t)
            (""     "latexsym"  t)
            (""     "amssymb"   t)
            (""     "hyperref"  nil)))

    ;; Packages to include when xelatex is used
    (if (string-match "LATEX_CMD: xelatex" (buffer-string))
        (setq org-latex-default-packages-alist
              '(("" "fontspec" t)
                ("" "xunicode" t)
                ("" "url" t)
                ("" "rotating" t)
                ("" "babel" t)
                ("babel" "csquotes" t)
                ("" "soul" t)
                ("xetex" "hyperref" nil)
                )))

    (if (string-match "LATEX_CMD: xelatex" (buffer-string))
        (setq org-latex-classes
              (cons '("article"
                      "\\documentclass[11pt,article,oneside]{memoir}"
                      ("\\section{%s}" . "\\section*{%s}")
                      ("\\subsection{%s}" . "\\subsection*{%s}")
                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                      ("\\paragraph{%s}" . "\\paragraph*{%s}")
                      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                    org-latex-classes)))))

(defun my-org-fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(add-hook 'org-babel-after-execute-hook 'my-org-fix-inline-images)

(use-package ox-html
  :defer t
  :config
  (progn
    (push '(code . "<kbd>%s</kbd>") org-html-text-markup-alist)))

(use-package org-habit
  :defer t
  :config
  (progn
    (defadvice org-get-repeat (around check-property-for-repeat activate)
      "Fix the problem with org-get-repeat where it only checks
the first scheduled timestamp for the repeater.  If we have
multiple scheduled timestamps, this might not be the repeat
interval we want (eg. we are setting weekly repeat schedule for
each day of the week).  Here we allow the value to be overriden
by a REPEAT property."
      (-if-let (repeat (org-entry-get (point) "REPEAT"))
          (setq ad-return-value repeat)
        ad-do-it))))

(use-package org-clock-budget
  :straight (org-clock-budget :repo "git@github.com:Fuco1/org-clock-budget.git"))

;; add support for automatic org-files commits
(defvar my-org-commit-timer
  (run-at-time (format-time-string "%H:59" (current-time)) 3600 'org-save-all-org-buffers)
  "Org commit timer.")

(defun my-org-auto-time-range-update ()
  "Automatically update time ranges and clock in org mode."
  (when (and (eq major-mode 'org-mode)
             (not (use-region-p)))
    (shut-up
     (ignore-errors
       (org-evaluate-time-range)))))
(add-hook 'post-command-hook 'my-org-auto-time-range-update)

;; Reminder setup
; Erase all reminders and rebuilt reminders for today from the agenda
(defun my-org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt)
  (appt-activate t))

; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'my-org-agenda-to-appt 'append)

; This is at the end of my .emacs - so appointments are set up when Emacs starts
(my-org-agenda-to-appt)

; Activate appointments so we get notifications
(appt-activate t)

; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'my-org-agenda-to-appt)

(org-add-link-type "my-orgdict" 'my-org-dictionary-follow)
(defun my-org-dictionary-follow (search)
  "Follow the org-dict link.

The link is composed of two parts separated by |.

The first part is a headline construct which contains optional
number of stars denoting depth followed by a space followed by a
regexp to compare against the header.

The second part is a regexp to search in the buffer."
  (-let (((header regexp) (split-string search "|" t))
         (hit nil))
    (save-excursion
      (goto-char (point-min))
      (while (not hit)
        (if (re-search-forward regexp nil t)
            (-let (((&plist :title title :level level) (my-org-header-at))
                   (depth (save-match-data
                            (when (string-match "\\`\\(\\*+\\) " header)
                              (length (match-string 1 header)))))
                   (clean-header (save-match-data
                                   (string-match "\\`\\(\\*+ +\\)?\\(.*\\)" header)
                                   (match-string 2 header))))
              (when (and (or (not depth) (= depth level))
                         (string-match-p clean-header title))
                (setq hit (point))))
          (setq hit t))))
    (when (numberp hit)
      (push-mark)
      (goto-char hit))))

(defun my-org-emphasis-regexp (open close)
  "Generate a regexp matching text enclosed with OPEN and CLOSE.
1 matches the entire body including the delimiters
2 matches the opening delimiter
3 matches the body sans delimiters
4 matches the closing delimiter"
  (concat
   "[[:space:]]"
   "\\("
     "\\(" (regexp-quote open) "\\)" ;; match the opening delimiter
     "\\([^" close "]*?\\)" ;; match the body
     "\\(" (regexp-quote close) "\\)" ;; match the closing delimiter
   "\\)"
   "[^[:word:]]"))

;; TODO: replace with orgba-in-any-block-p
(defun my-org-in-block-p ()
  "Non-nil when point belongs to any org block."
  (save-match-data
    (let ((case-fold-search t)
          (lim-up (save-excursion (outline-previous-heading)))
          (lim-down (save-excursion (outline-next-heading))))
      (org-between-regexps-p "^[ \t]*#\\+begin_" "^[ \t]*#\\+end_" lim-up lim-down))))

(defun my-org-emphasis-fontifier (face)
  "Generate code to fontify custom emphasis."
  (unless (save-match-data
            (or (org-in-block-p '("SRC" "EXAMPLE"))
                (eq (car (org-element-at-point)) 'fixed-width)))
    (when org-hide-emphasis-markers
      (add-text-properties (match-beginning 2) (match-end 2) '(invisible org-link))
      (add-text-properties (match-beginning 4) (match-end 4) '(invisible org-link)))
    (backward-char 1)
    face))

(defface my-org-inline-src-src
  '((t (:background "#212526")))
  "")

(defface my-org-inline-src-lang
  '((t (:inherit org-block-begin-line)))
  "")

(defface my-org-inline-src-header
  '((t (:inherit org-block-begin-line)))
  "")

(defface my-org-inline-src-code
  '((t (:background "#212526")))
  "")

(defun my-org-match-inline-src-block (limit)
  (catch 'done
    (let (header-args
          code)
      (while (re-search-forward
              (rx (group "src_")
                  (group (+ alnum)))
              limit t)
        (when (looking-at-p (regexp-quote "["))
          (setq header-args
                (cons (point)
                      (progn (forward-sexp) (point)))))
        (when (looking-at-p (regexp-quote "{"))
          (setq code
                (cons (point)
                      (progn (forward-sexp) (point))))
          (when org-hide-emphasis-markers
            (add-text-properties
             (match-beginning 1)
             (match-end 1)
             '(invisible org-link)))
          (add-face-text-property
           (match-beginning 1)
           (match-end 1)
           'my-org-inline-src-src)
          (add-face-text-property
           (match-beginning 2)
           (match-end 2)
           'my-org-inline-src-lang)
          (when header-args
            (add-face-text-property
             (car header-args)
             (cdr header-args)
             'my-org-inline-src-header))
          (org-src-font-lock-fontify-block (match-string 2) (car code) (cdr code))
          (throw 'done (point)))))))

(defun my-org-fontify-list-marker ()
  "Fontify the list marker at the beginning of line but not in source blocks.

Also fontify the space in front to make sure nested lists are
properly aligned."
  (unless (org-in-block-p '("SRC" "EXAMPLE"))
    'org-list-dt))

(font-lock-add-keywords 'org-mode
                        `((,(my-org-emphasis-regexp "$" "$") 1 (funcall 'my-org-emphasis-fontifier 'markup-math))
                          (,(my-org-emphasis-regexp "{" "}") 1 (funcall 'my-org-emphasis-fontifier 'shadow))
                          ;; Fontify list markers
                          ("^[ \t]*\\([-+]\\|[0-9]+[).]\\) " 0 (funcall 'my-org-fontify-list-marker))
                          ;; Fontify hashtags
                          ("\\s-\\(#[^ \n]+\\)" 1 'font-lock-keyword-face prepend))
                        'append)

(bind-keys :map org-mode-map
  ("TAB" . smart-tab)
  ("RET" . my-org-return)
  ("<tab>" . smart-tab)
  ("C-e" . my-end-of-code-or-line)
  ("C-a" . my-back-to-indentation-or-beginning)
  ("C-c C-x r" . org-clock-remove-overlays)
  ;; TODO lepsia mapa pre "toggle prikazy?"
  ("C-c C-x L" . org-toggle-link-display)
  ("C-c C-x w" . org-cut-subtree)
  ("C-c C-x y" . org-paste-subtree)
  ("C-c k" . my-insert-key-in-org)
  ("C-c S" . org-table-sort-lines)

  ("C-x n t" . my-org-narrow-to-top-heading)
  ("C-x n P" . my-org-narrow-to-project)
  ("C-x n N" . my-org-narrow-to-subtree)
  ("C-x n W" . my-org-widen)

  ("C-c C-=" . org-open-at-point)
  ("M-'" . my-org-open-at-point)
  ("C-M-'" . org-mark-ring-goto)

  ("C-c C-S-n" . my-org-add-sibling)
  ("C-c C-n" . outline-next-visible-heading)
  ("C-c s" . helm-org-in-buffer-search)
  ("A-d" . helm-org-in-buffer-search))
(unbind-key "C-'" org-mode-map)

(defun my-insert-key-in-org (key)
  "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
  (interactive "kType key sequence: ")
  (let* ((orgp (derived-mode-p 'org-mode))
         (tag (if orgp "~%s~" "<kbd>%s</kbd>")))
    (if (null (equal key "\C-m"))
        (insert
         (format tag (help-key-description key nil)))
      ;; If you just hit RET.
      (insert (format tag ""))
      (forward-char (if orgp -1 -6)))))

(defun my-org-open-at-point (&rest _ignored)
  "Just like `org-open-at-point', but open link in this window.

If there is no link on point, call `org-next-link'."
  (interactive)
  (let ((current-prefix-argument nil)
        (org-link-frame-setup (acons 'file 'find-file org-link-frame-setup)))
    (condition-case err
        (org-open-at-point '(4))
      (user-error
       (progn
         (unless (stringp (org-next-link))
           (my-org-open-at-point)))))))

(defun my-goto-current-clocked-task ()
  (interactive)
  (org-goto-marker-or-bmk org-clock-marker))

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

(defun my-org-global-skip-function ()
  "Global skip function for all agenda views"
  (when (member "folder" (ignore-errors (org-get-tags)))
    (save-excursion
      (or (outline-next-heading)
          (point-max)))))

(defun my-org-field-empty-p ()
  "Return non-nil if the org table field at point is empty."
  (equal "" (s-trim (save-excursion (org-table-get-field)))))

;; TODO: replace with orgba-table-select-cell
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
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list
                  (save-excursion (outline-previous-heading) (forward-line) (point))
                  (save-excursion (outline-next-heading) (forward-line -1) (point)))))
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

;;;_. Narrowing

;; TODO: replace with orgba-narrow-to-top-heading
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
(defun my-notmuch-show-get-name ()
  (with-current-buffer (org-capture-get :original-buffer)
    (car (mail-extract-address-components (notmuch-show-get-from)))))

(defun my-notmuch-show-get-email ()
  (with-current-buffer (org-capture-get :original-buffer)
    (cadr (mail-extract-address-components (notmuch-show-get-from)))))

(defun my-org-note-find-position ()
  (let ((file (file-name-sans-extension (f-filename buffer-file-name))))
    (pop-to-buffer (find-file "notes.org"))
    (widen)
    (goto-char (point-min))
    (when (re-search-forward (concat "^\\* +" file) nil t)
      (beginning-of-line))))

(defvar my-org-node-is-note-capture nil)
(defvar my-org-node-last-id nil)

(defun my-org-note-get-template ()
  "Create template for org inline note."
  (-let ((id (org-id-get-create))
         (text (and (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))))
         (context (-let [(type (&plist :contents-begin :contents-end))
                         (org-element-at-point)]
                    (when (eq type 'paragraph)
                      (buffer-substring-no-properties contents-begin contents-end)))))
    (unless text
      (error "Unable to create note to no content"))
    (setq my-org-node-is-note-capture (cons (region-beginning) (region-end)))
    (let ((template (format "* <note-placeholder>
:PROPERTIES:
:CREATED: %%u
:END:
- pingback: [[id:%s][%s]]
%s

%%?
"
                            id (replace-regexp-in-string "\n" " " text)
                            (format ":CONTEXT:
- context: %%a
%s:END:" (if context
             (format "  #+BEGIN_QUOTE
%s  #+END_QUOTE
" context)
           "")))))
      (with-temp-buffer
        (pop-to-buffer (current-buffer))
        (org-mode)
        (insert template)
        (goto-char (point-min))
        (org-font-lock-ensure)
        (search-forward "pingback: ")
        (unless org-descriptive-links
          (org-toggle-link-display))
        (org-fill-paragraph)
        (buffer-string)))))

(defun my-org-note-get-id-create ()
  (save-excursion
    (org-back-to-heading t)
    (when (search-forward "<note-placeholder>" (line-end-position) t)
      (let ((id (org-id-get-create)))
        (org-back-to-heading t)
        (search-forward "<note-placeholder>" (line-end-position) t)
        (replace-match id)
        (setq my-org-node-last-id id)))))

(defun my-org-note-create-link-to-note ()
  (save-excursion
    (when (and my-org-node-is-note-capture
               (not org-note-abort))
      (-let* (((beg . end) my-org-node-is-note-capture)
              (text (delete-and-extract-region beg end)))
        (goto-char beg)
        (insert (format "[[id:%s][%s]]" my-org-node-last-id text)))))
  (setq my-org-node-is-note-capture nil))

(add-hook 'org-capture-mode-hook 'my-org-note-get-id-create)
(add-hook 'org-capture-after-finalize-hook 'my-org-note-create-link-to-note)

(setq org-capture-templates
      `(("r" "Todo" entry (file "~/org/refile.org")
         "* TODO %?\n%U\n%a\n" :clock-keep t)
        ("p" "Pomodoro interruption" entry (file "~/org/refile.org")
         "* TODO %? :pomodoro:\n%U\n%a\n" :clock-keep t)
        ("i" "Interruption" entry (file "~/org/interruptions.org")
         "* %? :interruption:\n%U\n%a\n" :clock-in t :clock-resume t)
        ("t" "Todo entries")
        ,@(let ((targets '(("te" "todo-emacs" "emacs" "Emacs config")
                           ("tg" "todo-general" "me" "General")
                           ("tl" "todo-linux" "me" "Linux")
                           ("th" "todo-home" "home" "General"))))
            (--map
             `(,(nth 0 it) ,(nth 1 it) entry (file+headline ,(concat "~/org/" (nth 2 it) ".org") ,(nth 3 it))
               "* TODO %?\n%U\n" :clock-keep t)
             targets))
        ("w" "Work")
        ("wl" "Work - Logio" entry (file+olp "~/org/work.org" "Logio")
         "* TODO %? :logio:
%^{HELPDESK}p%U
 - [ ] zadane v HD
 - [ ] zadany cas v HD
" :clock-keep t
         )
        ("l" "Read later" entry (file "~/org/refile.org")
         "* %?%:description :readlater:
:PROPERTIES:
:SOURCE: %:link
:END:
%(when (< 2 (length \"%:initial\")) \"- %:initial\n\")
%(unless (string-blank-p \"%:elfeed-entry-link\") \"- web link: %:elfeed-entry-link \n\")")
        ("j" "Journals")
        ("jj" "Journal" entry (file+datetree "~/data/documents/journal.org.gpg") "* %<%H:%M:%S> %?" :clock-keep t :kill-buffer t :empty-lines-after 1)
        ("jo" "Journal - trading" entry (file+datetree "~/org/inv.org") "* %<%H:%M:%S> %?" :clock-keep t)
        ("n" "Note" entry (function my-org-note-find-position)
         (function my-org-note-get-template))
        ("b" "Bookmark" entry (file+function "~/org/bookmarks.org" my-org-handle-bookmark)
         "* %:description\n- %:link\n")
        ("c" "Contact" entry (file "~/org/contacts.org")
         "* %(my-notmuch-show-get-name)
    :PROPERTIES:
    :EMAIL: %(my-notmuch-show-get-email)
    :END:")))

(defun my-org-handle-bookmark ()
  (let ((link (caar org-stored-links)))
    (cond
     ;; add handlers for various categories here
     (t
      (goto-char (point-max))
      (newline)))))

(defun my-org-remove-empty-drawer-on-clock-out ()
  "Remove empty LOGBOOK drawers on clock out."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (org-back-to-heading t)
      (org-narrow-to-subtree)
      (catch 'done
        (while (re-search-forward org-drawer-regexp nil t)
          (when (equal (match-string 1) "LOGBOOK")
            (beginning-of-line)
            (org-remove-empty-drawer-at (point))
            (throw 'done t)))))))

(add-hook 'org-clock-out-hook 'my-org-remove-empty-drawer-on-clock-out 'append)

;;;; Refile settings
;; Exclude DONE state tasks from refile targets
(defun my-org-verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

;; TODO KEYWORDS SETTINGS
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "BILL(b)" "|" "DONE(d!)" "MOVE(m@)" "FAIL(f@)")
        (sequence "WAIT(w@/@)" "HOLD(h@/@)" "CONF(c@/@)""|" "STOP(s@/@)")
        (sequence "|" "IDEA(i)")))

;; TODO: move faces to theme
(setq org-todo-keyword-faces
      '(
        ("TODO" my-org-keyword-todo)
        ("NEXT" my-org-keyword-next)
        ("BILL" my-org-keyword-bill)
        ("DONE" my-org-keyword-done)
        ("MOVE" my-org-keyword-move)
        ("FAIL" my-org-keyword-fail)
        ("WAIT" my-org-keyword-wait)
        ("HOLD" my-org-keyword-hold)
        ("CONF" my-org-keyword-conf)
        ("STOP" my-org-keyword-stop)
        ("IDEA" my-org-keyword-idea)
        ))

(setq org-todo-state-tags-triggers
      (quote (
              ("TODO" ("WAIT") ("STOP") ("HOLD"))
              ("NEXT" ("WAIT") ("STOP") ("HOLD") ("readlater") ("watchlater"))
              (done ("WAIT") ("STOP") ("HOLD")
                    ("readlater") ("watchlater")
                    ("#mit") ("#weekly") ("#monthly"))
              ("WAIT" ("WAIT" . t))
              ("HOLD" ("HOLD" . t))
              ("STOP" ("WAIT") ("STOP" . t) ("HOLD"))
              ("IDEA" ("WAIT") ("STOP") ("HOLD")))))

;; Tags shortcuts
(setq org-tag-alist (quote (("readlater" . ?r)
                            ("fantasy" . ?f)
                            ("scifi" . ?s)
                            ("finance" . ?i)
                            ("physics" . ?p)
                            ("math" . ?m))))

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
  (goto-char (next-single-property-change (point) 'todo-state))
  (forward-line -1)
  (let ((index 0))
    (with-current-buffer buf
      (insert "|-+-+-+-+-+-|\n")
      (insert "| Index | Language | Title | Published | Author | Original Title |\n")
      (insert "|-+-+-+-+-+-|\n"))
    (while (and (= 0 (forward-line))
                (not (eobp)))
      (org-with-point-at (org-get-at-bol 'org-hd-marker)
        (let* ((element (cadr (org-element-at-point)))
               (title (plist-get element :title))
               (author (plist-get element :AUTHOR))
               (published (plist-get element :PUBLISHED))
               (original-title (plist-get element :ORIGINAL_TITLE))
               (tags (org-get-tags-at))
               (language (cdr (assoc (let* ((language-tag
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
          (unless (member "noexport" tags)
            (cl-incf index)
            (with-current-buffer buf
              ;; num, lan, title, published, author, orig. title
              (insert (format "| %d. | %s | %s | %s | %s | %s |\n"
                              index
                              language
                              title
                              published
                              author
                              (or original-title ""))))))))
    (with-current-buffer buf
      (insert "|-+-+-+-+-+-|\n")
      (save-excursion
        (forward-line -1)
        (org-table-align)))))

(defun my-org-get-option (name)
  (let ((re (org-make-options-regexp (list name))))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (when (re-search-forward re nil t)
          (org-match-string-no-properties 2))))))

(defun my-org-export-read-books ()
  (interactive)
  (let ((org-agenda-files (list "~/org/me.org" "~/org/reading.org")))
    (save-window-excursion
      (unwind-protect
          (let ((years-to-export (nreverse (number-sequence 2014 2017)))
                (buf (get-buffer-create "*org-books-export*"))
                (my-org-show-media-closed-since
                 (apply 'encode-time (org-parse-time-string "2014-01-01")))
                (org-agenda-sticky nil))
            (with-current-buffer buf
              (erase-buffer)
              (org-mode)
              (insert "* Done\n"))
            (--each years-to-export
              (with-current-buffer buf
                (insert (format "** %d\n" it)))
              (let ((my-org-show-media-closed-since
                     (apply 'encode-time (org-parse-time-string (format "%d-01-01" it))))
                    (my-org-show-media-closed-until
                     (apply 'encode-time (org-parse-time-string (format "%d-01-01" (1+ it))))))
                (org-agenda nil "fdb")
                (my-org-export-read-books-do-export buf)
                (with-current-buffer buf
                  (insert "\n\n"))))
            (org-agenda nil "fb")
            (with-current-buffer buf
              (insert "\n* Reading\n\n"))
            (my-org-export-read-books-do-export buf)
            (with-current-buffer buf
              (org-export-to-file 'html "~/books.html")))
        (kill-buffer "*org-books-export*")))))


;; navigation & header manipulation
;; TODO: replace with orgba-next-parent-sibling
(defun my-org-next-parent-sibling ()
  (condition-case err
      (progn
        (outline-up-heading 1)
        (outline-get-next-sibling))
    (error
     (goto-char (point-max)))))

;; TODO: replace with orgba-top-parent
(defun my-org-top-parent ()
  "Go to the top parent of current heading."
  (interactive)
  (while (org-up-heading-safe)))

(defun my-org-show-parent-context ()
  "Show the entire tree of the top-most parent."
  (interactive)
  (save-excursion
    (my-org-top-parent)
    (org-show-subtree)
    (save-restriction
      (widen)
      (org-narrow-to-subtree)
      (org-cycle-hide-drawers t))))

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
  (let* ((cdepth (plist-get (cadr (org-element-at-point)) :level))
         (ndepth (or (save-excursion
                       (outline-next-heading)
                       (plist-get (cadr (org-element-at-point)) :level))
                     cdepth)))
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

;; TODO: replace with orgba-heading-at
(defun my-org-header-at (&optional point)
  "Return the header element at POINT."
  (setq point (or point (point)))
  (save-excursion
    (org-back-to-heading)
    (-let [(_ header) (org-element-at-point)]
      header)))


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

;;; Sanskrit formatting
(defun my-format-sanskrit-lines (&optional no-latin)
  (let ((w1 (s-trim (thing-at-point 'line)))
        (w2 (progn
              (forward-line)
              (s-trim (thing-at-point 'line))))
        (w3 (progn
              (forward-line)
              (s-trim (thing-at-point 'line))))
        (w4 (progn
              (forward-line)
              (s-trim (thing-at-point 'line))))
        (w5 (progn
              (forward-line)
              (s-trim (thing-at-point 'line))))
        (w6 (progn
              (forward-line)
              (s-trim (thing-at-point 'line)))))
    (forward-line -5)
    (kill-line 6)
    (if (not no-latin)
        (insert (format "| %s | %s | %s |\n| %s | %s | %s |\n|-+-+-|\n" w1 w3 w5 w2 w4 w6))
      (insert (format "| %s | %s | %s |\n|-+-+-|\n" w1 w3 w5 )))))

(defun my-format-sanskrit-lines-no-latin ()
  (let ((w1 (s-trim (thing-at-point 'line)))
        (w2 (progn
              (forward-line)
              (s-trim (thing-at-point 'line))))
        (w3 (progn
              (forward-line)
              (s-trim (thing-at-point 'line)))))
    (forward-line -2)
    (kill-line 3)
    (insert (format "| %s | %s | %s |\n|-+-+-|\n" w1 w2 w3))))

(defun my-format-sanskrit-noun ()
  (interactive)
  (insert "|-+-+-|\n")
  (let ((kill-ring kill-ring))
    (--dotimes 8 (my-format-sanskrit-lines))))

(defun my-format-sanskrit-noun-delete-latin ()
  (interactive)
  (insert "|-+-+-|\n")
  (let ((kill-ring kill-ring))
    (--dotimes 8 (my-format-sanskrit-lines :no-latin))))

(defun my-format-sanskrit-verb ()
  (interactive)
  (insert "|-+-+-|\n")
  (let ((kill-ring kill-ring))
    (--dotimes 3 (my-format-sanskrit-lines))))

(defun my-format-sanskrit-verb-no-latin ()
  (interactive)
  (insert "|-+-+-|\n")
  (let ((kill-ring kill-ring))
    (--dotimes 3 (my-format-sanskrit-lines-no-latin))))

(defun my-gleeo-to-org-timestamps ()
  "Convert Gleeo export to org timestamps."
  (interactive)
  (reverse-region (point-min) (point-max))
  (my-with-each-line
    (delete-region (point) (search-forward "," nil t 4))
    (search-forward "," nil t)
    (replace-match " ")
    (delete-region (1- (search-forward "," nil t)) (line-end-position))
    (beginning-of-line)
    (let ((from (delete-and-extract-region
                 (point) (1- (search-forward " " nil t 2))))
          (to (progn
                (delete-char 1)
                (delete-and-extract-region (point) (line-end-position)))))
      (insert
       "CLOCK: "
       (format-time-string "[%Y-%m-%d %a %H:%M]"
                           (org-read-date nil 'totime from))
       "--"
       (format-time-string "[%Y-%m-%d %a %H:%M]"
                           (org-read-date nil 'totime to))))))

(defadvice org-archive-subtree (around fix-hierarchy activate)
  (let* ((fix-archive-p (and (not current-prefix-arg)
                             (not (use-region-p))))
         (afile (org-extract-archive-file (org-get-local-archive-location)))
         (buffer (or (find-buffer-visiting afile) (find-file-noselect afile))))
    ad-do-it
    (when fix-archive-p
      (with-current-buffer buffer
        (goto-char (point-max))
        (while (org-up-heading-safe))
        (let* ((olpath (org-entry-get (point) "ARCHIVE_OLPATH"))
               (path (and olpath (split-string olpath "/")))
               (level 1)
               tree-text)
          (when olpath
            (org-mark-subtree)
            (setq tree-text (buffer-substring (region-beginning) (region-end)))
            (let (this-command) (org-cut-subtree))
            (goto-char (point-min))
            (save-restriction
              (widen)
              (-each path
                (lambda (heading)
                  (if (re-search-forward
                       (rx-to-string
                        `(: bol (repeat ,level "*") (1+ " ") ,heading)) nil t)
                      (org-narrow-to-subtree)
                    (goto-char (point-max))
                    (unless (looking-at "^")
                      (insert "\n"))
                    (insert (make-string level ?*)
                            " "
                            heading
                            "\n"))
                  (cl-incf level)))
              (widen)
              (org-end-of-subtree t t)
              (org-paste-subtree level tree-text))))))))

(defun my-org-table-to-ledger-quotes (ticker)
  "Convert an org table with date/price quote to ledger format.

PSE quotes can be downloadad as csv files at
https://www.pse.cz/udaje-o-trhu/akcie/"
  (let ((data (org-table-to-lisp)))
    (-map
     (-lambda ((d p))
       (-let (((day mon year) (-map 'string-to-number (split-string d "\\."))))
         (format "P %d/%02d/%02d 00:00:00 %s %s Kc"
                 year mon day ticker p)))
     data)))

(defun my-org-clear-current-line-and-return ()
  "Clear the current line and call `org-return'."
  (beginning-of-line)
  (setf (buffer-substring
         (line-beginning-position)
         (line-end-position))
        "")
  (org-return))

;; Based on scimax/org-return
(defun my-org-return (&optional ignore)
  "Add new list item, heading or table row with RET.
A double return on an empty element deletes it.
Use a prefix arg to get regular RET. "
  (interactive "P")
  (if ignore (org-return)
    (let* ((context (org-element-context))
           (syntax-type (car context)))
      (cond
       ((eq 'line-break syntax-type)
        (org-return-indent))
       ;; Open links like usual
       ((eq 'link syntax-type)
        (org-open-at-point-global))
       ;; It doesn't make sense to add headings in
       ;; inline tasks. Thanks Anders Johansson!
       ((and (fboundp 'org-inlinetask-in-task-p)
             (org-inlinetask-in-task-p))
        (org-return))
       ;; add checkboxes
       ((and (not (bolp))
             (org-at-item-checkbox-p))
        (org-insert-todo-heading nil))
       ;; lists end with two blank lines, so we need
       ;; to make sure we are also not at the
       ;; beginning of a line to avoid a loop where
       ;; a new entry gets created with only one
       ;; blank line.
       ((and (not (bolp)) (org-in-item-p))
        (if (or (org-element-property
                 :contents-begin context)
                (and (eq (car context) 'verbatim)
                     (org-element-property
                      :contents-begin
                       (org-element-property
                        :parent context))))
            (org-insert-item)
          (delete-region (line-beginning-position)
                         (line-end-position))
          (org-return)))
       ((and (not (bolp)) (org-at-heading-p))
        (if (not
             (string=
              ""
              (org-element-property :title context)))
            (org-insert-heading-respect-content)
          (delete-region (line-beginning-position)
                         (line-end-position))))
       ((org-at-table-p)
        (let ((current-line
               (save-excursion
                 (save-restriction
                   (widen)
                   (narrow-to-region
                    (line-beginning-position)
                    (line-end-position))
                   (car (org-table-to-lisp))))))
          (if (or (eq current-line 'hline)
                  (--any? (not (string= "" it))
                          current-line))
              (org-return)
            ;; empty row
            (delete-region (line-beginning-position)
                           (line-end-position))
            (org-return))))
       (t
        (org-return))))))

(defun my-org-calculate-invoice ()
  "Update invoicing information from invoice hours."
  (interactive)
  (let ((current-rate nil))
    (save-excursion
      (orgba-top-parent)
      (org-map-tree
       (lambda ()
         (--when-let (org-entry-get (point) "HOURLY_RATE")
           (setq current-rate (string-to-number it)))
         (when current-rate
           (--when-let (org-entry-get (point) "INVOICE_HOURS")
             (org-entry-put
              (point) "INVOICE_VALUE"
              (format "%d" (* current-rate
                              (/ (org-duration-to-minutes it) 60)))))))))))

(provide 'org-defs)
