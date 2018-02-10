(defun my-eshell-send-input ()
  "Same as `eshell-send-input' but with some extra handling.

If the point is at a PHP trace line:

    in <file name>:(<line>)

pop to that buffer if it exists."
  (interactive)
  (cond
   ((save-excursion
      (back-to-indentation)
      (looking-at "in \\(.+\\)(\\([0-9]+\\))"))
    (let* ((file (match-string 1))
           (line (match-string 2))
           (possible-buffers
            (--filter (string-match-p
                       file
                       (or (buffer-file-name it) ""))
                      (buffer-list))))
      (when (= 1 (length possible-buffers))
        (pop-to-buffer (car possible-buffers))
        (goto-line (string-to-int line)))))
   (t (call-interactively 'eshell-send-input))))

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

(provide 'eshell-defs)
