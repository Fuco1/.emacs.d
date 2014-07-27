;; some code here is adopted from http://doc.norang.ca/org-mode.html

(defvar my-org-ignore-task-in-project-by-tag
  '("BOOKS")
  "Tasks with these tags should be ignored when determining if a
task is a subtask in a project.")

(defun my-org-restricted-p ()
  "Return non-nil if org is restricted to a subtree."
  (marker-buffer org-agenda-restrict-begin))

(defun my-org-entry-is-task-p ()
  "Return non-nil if header at point has any keyword."
  (member (org-get-todo-state) org-todo-keywords-1))

(defun my-org-is-project-p ()
  "Any task with a todo keyword subtask.

Done subtasks and subtask tagged with a tag in
`my-org-ignore-task-in-project-by-tag' are skipped."
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          has-subtask)
      (when (and (org-entry-is-todo-p) (not (org-is-habit-p)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (and (org-entry-is-todo-p)
                       (not (--any? (member it my-org-ignore-task-in-project-by-tag) (org-get-tags-at))))
              (setq has-subtask t)))
          has-subtask)))))

(defun my-org-is-subproject-p ()
  "Any task which is a subtask of another project"
  (let (is-subproject)
    (when (org-entry-is-todo-p)
      (save-excursion
        (while (and (not is-subproject) (org-up-heading-safe))
          (when (org-entry-is-todo-p)
            (setq is-subproject t)))))
    is-subproject))

(defun my-org-find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (my-org-entry-is-task-p)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun my-org-is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
    (save-excursion
      (my-org-find-project-task)
      (/= (point) task))))

(defun my-org-is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          has-subtask)
      (when (my-org-entry-is-task-p)
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (my-org-entry-is-task-p)
              (setq has-subtask t)))))
      (not has-subtask))))

(defun my-org-skip-stuck-projects ()
  "Skip trees that are stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (my-org-is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 has-next)
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAIT" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next nil next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun my-org-skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (my-org-is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAIT" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next next-headline nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun my-org-skip-non-projects ()
  "Skip trees that are not projects"
  (if (save-excursion (my-org-skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((my-org-is-project-p) nil)
           (t subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun my-org-skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (when (or (org-is-habit-p)
                (my-org-is-project-p)
                (and (my-org-is-task-p)
                     (not (my-org-is-project-subtree-p))))
        next-headline))))

(defun my-org-skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (my-org-restricted-p)))
      (cond
       ((my-org-is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (my-org-is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (my-org-is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun my-org-update-parent-tasks-todo ()
  "Visit each parent task and change NEXT states to TODO"
  (save-excursion
    (let ((mystate (nth 2 (org-heading-components)))
          moved-up)
      (cond
       ((equal mystate "NEXT")
        (save-excursion
          (while (and (setq moved-up (org-up-heading-safe)) (not (my-org-entry-is-task-p))))
          (when (and moved-up
                     (member (org-get-todo-state) (list "NEXT" "DONE")))
            (org-todo "TODO"))))
       ((equal mystate "TODO")
        (save-excursion
          (while (and (setq moved-up (org-up-heading-safe)) (not (my-org-entry-is-task-p))))
          (when (and moved-up
                     (member (org-get-todo-state) (list "DONE")))
            (org-todo "TODO"))))
       ((equal mystate "DONE")
        (save-excursion
          (while (and (setq moved-up (org-up-heading-safe)) (not (my-org-entry-is-task-p))))
          (when moved-up
            (let (all-done)
              (save-excursion
                (org-goto-first-child)
                (setq all-done (if (my-org-entry-is-task-p) (org-entry-is-done-p) t))
                (while (and all-done (org-goto-sibling))
                  (when (my-org-entry-is-task-p)
                    (setq all-done (org-entry-is-done-p)))))
              (when all-done
                (org-todo "DONE"))))))))))

(defun my-org-update-siblings-tasks-todo ()
  (save-excursion
    (let ((mystate (nth 2 (org-heading-components))))
      (cond
       ((equal mystate "NEXT")
        (let ((pos (save-excursion (org-back-to-heading 'invisible-ok) (point))))
          (when (org-up-heading-safe)
            (org-goto-first-child)
            (do ((has-sibling t (org-goto-sibling)))
                ((not has-sibling))
              (when (and (member (org-get-todo-state) (list "NEXT"))
                         (/= (point) pos))
                (let ((org-after-todo-state-change-hook))
                  (org-todo "TODO")))))))))))


(add-hook 'org-after-todo-state-change-hook 'my-org-update-parent-tasks-todo 'append)
(add-hook 'org-after-todo-state-change-hook 'my-org-update-siblings-tasks-todo 'append)
