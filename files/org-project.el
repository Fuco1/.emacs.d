;;; org-package.el --- Additional code for managing packages

;;; Commentary:

;; some code here is adopted from http://doc.norang.ca/org-mode.html

;;; TODO:

;; - napisat testy
;; - optimalizovat
;; my-org-is-project-p                               827         0.6357351890  0.0007687245
;; my-org-skip-project-tasks-maybe                   239         0.4149120869  0.0017360338
;; my-org-skip-non-stuck-projects                    300         0.3088462969  0.0010294876
;; my-org-skip-non-projects                          249         0.2266012680  0.0009100452
;; my-org-is-project-subtree-p                       225         0.1833827     0.0008150342
;; my-org-find-project-task                          225         0.1768276280  0.0007859005
;; my-org-next-heading-pos                           551         0.0546931169  9.926...e-05
;; my-org-skip-projects-and-habits-and-single-tasks  27          0.049606745   0.0018372868
;; my-org-entry-is-task-p                            434         0.0284485230  6.554...e-05
;; my-org-skip-stuck-projects                        11          0.0235584430  0.0021416766
;; my-org-is-task-p                                  27          0.0051818140  0.0001919190
;; my-org-skip-projects                              1           0.000391605   0.000391605
;; my-org-restricted-p                               240         0.0001750009  7.291...e-07

;;; Code:

(require 'org)
(require 'org-agenda)
(require 'org-habit)
(require 'cl-lib)

(require 'dash)

(defvar my-org-ignore-task-in-project-by-tag
  '("BOOKS")
  "Tags to ignore under projects.

Tasks with these tags should be ignored when determining if a
task is a subtask in a project.")

;; TODO: replace with orgba-next-heading
(defun my-org-next-heading ()
  "Go to next heading or end of file if at the last heading."
  (or (outline-next-heading) (goto-char (point-max))))

(defun my-org-next-heading-pos ()
  "Return position of next heading or end of file if at the last heading."
  (or (save-excursion (outline-next-heading)) (point-max)))

;; TODO: replace with orgba-restricted-p
(defun my-org-restricted-p ()
  "Return non-nil if org is restricted to a subtree."
  (marker-buffer org-agenda-restrict-begin))

(defun my-org-entry-is-task-p ()
  "Return non-nil if header at point has any keyword."
  (member (org-get-todo-state) org-todo-keywords-1))

(defun my-org-is-standalone-task-p ()
  "Any task which is not a project and is not a subtask in a project."
  (and (not (my-org-is-project-p))
       (= (save-excursion (my-org-find-project-task))
          ;; TODO: make a version of this where we don't have to pass
          ;; an argument.  We always want all headings when doing
          ;; "logic".  Also, make it return point
          (save-excursion (org-back-to-heading 'invisible-ok) (point)))))

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
  "Any task which is a subtask of another project."
  (let (is-subproject)
    (when (org-entry-is-todo-p)
      (save-excursion
        (while (and (not is-subproject) (org-up-heading-safe))
          (when (org-entry-is-todo-p)
            (setq is-subproject t)))))
    is-subproject))

(defun my-org-find-project-task ()
  "Move point to the parent (project) task if any."
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
  "Any task with a todo keyword and no subtask."
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

(defun my-org-skip-projects ()
  "Skip trees that are projects."
  (save-restriction
    (widen)
    (when (my-org-is-project-p) (my-org-next-heading-pos))))

(defun my-org-skip-non-projects ()
  "Skip trees that are not projects."
  (save-restriction
    (widen)
    (unless (my-org-is-project-p) (my-org-next-heading-pos))))

(defun my-org-stuck-project-p ()
  "Test if the tree at point is stuck project.

A stuck project has subtasks but no next task."
  (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
         has-action)
    (save-excursion
      (forward-line 1)
      (while (and (not has-action)
                  (< (point) subtree-end)
                  (re-search-forward "^\\*+ \\(NEXT\\|WAIT\\) " subtree-end t))
        (setq has-action t)))
    (not has-action)))

(defun my-org-skip-stuck-projects ()
  "Skip trees that are stuck projects."
  (save-restriction
    (widen)
    (if (my-org-is-project-p)
        (when (my-org-stuck-project-p)
          (my-org-next-heading-pos))
      nil)))

(defun my-org-skip-non-stuck-projects ()
  "Skip trees that are not stuck projects."
  (save-restriction
    (widen)
    (if (my-org-is-project-p)
        (unless (my-org-stuck-project-p)
          (my-org-next-heading-pos))
      ;; not a project at all, go to next headline
      (my-org-next-heading-pos))))

(defun my-org-skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks."
  (save-restriction
    (widen)
    (when (or (org-is-habit-p)
              (my-org-is-project-p)
              (and (my-org-is-task-p)
                   (not (my-org-is-project-subtree-p))))
      (my-org-next-heading-pos))))

(defun my-org-skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.

When restricted to a project, skip project and sub project tasks,
habits, NEXT tasks, and loose tasks.  When not restricted, skip
project and sub-project tasks, habits, and project related
tasks."
  (save-restriction
    (widen)
    (let* ((limit-to-project (my-org-restricted-p)))
      (cond
       ((my-org-is-project-p)
        (my-org-next-heading-pos))
       ((org-is-habit-p)
        (save-excursion (org-end-of-subtree t)))
       ((and (not limit-to-project)
             (my-org-is-project-subtree-p))
        (save-excursion (org-end-of-subtree t)))
       ((and limit-to-project
             (my-org-is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        (save-excursion (org-end-of-subtree t)))
       (t
        nil)))))

(defun my-org-update-parent-tasks-todo ()
  "Visit each parent task and change NEXT states to TODO."
  (save-excursion
    (let ((mystate (nth 2 (org-heading-components)))
          moved-up)
      (cond
       ((equal mystate "NEXT")
        (save-excursion
          (while (and (setq moved-up (org-up-heading-safe)) (not (my-org-entry-is-task-p))))
          (when moved-up
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
                (let ((org-inhibit-logging t)) (org-todo "DONE")))))))))))

(defun my-org-update-siblings-tasks-todo ()
  "Update sibling tasks on todo state change."
  (save-excursion
    (let ((mystate (nth 2 (org-heading-components))))
      (cond
       ((equal mystate "NEXT")
        (let ((pos (save-excursion (org-back-to-heading 'invisible-ok) (point))))
          (when (org-up-heading-safe)
            (org-goto-first-child)
            (cl-do
                ((has-sibling t (org-goto-sibling)))
                ((not has-sibling))
              (when (and (member (org-get-todo-state) (list "NEXT"))
                         (/= (point) pos))
                (let ((org-after-todo-state-change-hook))
                  (org-todo "TODO")))))))))))


(add-hook 'org-after-todo-state-change-hook 'my-org-update-parent-tasks-todo 'append)
(add-hook 'org-after-todo-state-change-hook 'my-org-update-siblings-tasks-todo 'append)

(provide 'org-project)
;;; org-project.el ends here
