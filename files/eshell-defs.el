;;;; git config
(defun my--get-git-branches (&optional all)
  (let ((branches (-map 'car (magit-list-interesting-refs))))
    (if all
        branches
      (--filter (not (string-match-p "/" it)) branches))))

(defun pcomplete/eshell-mode/gc ()
  (pcomplete-here (my--get-git-branches)))

(defun pcomplete/eshell-mode/git ()
  (pcomplete-here '("add"
                    "bisect"
                    "branch"
                    "checkout"
                    "clone"
                    "commit"
                    "diff"
                    "fetch"
                    "grep"
                    "init"
                    "log"
                    "merge"
                    "mv"
                    "pull"
                    "push"
                    "rebase"
                    "remote"
                    "reset"
                    "rm"
                    "show"
                    "status"
                    "tag"))
  (pcomplete-here
   (let ((last-cmd (nth (1- pcomplete-last) pcomplete-args)))
     (cond
      ((equal "checkout" last-cmd)
       (my--get-git-branches))
      ((equal "add" last-cmd)
       (pcomplete-entries))
      ((equal "merge" last-cmd)
       (my--get-git-branches t))))))

;;;; hg config
(defun pcomplete/eshell-mode/hg ()
  (pcomplete-here '("add"
                    "annotate"
                    "clone"
                    "commit"
                    "diff"
                    "export"
                    "forget"
                    "init"
                    "log"
                    "merge"
                    "pull"
                    "push"
                    "remove"
                    "serve"
                    "status"
                    "summary"
                    "update")))
