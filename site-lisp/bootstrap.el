;; -*- lexical-binding: t -*-

(require 'dash)

(defun my-create-cache ()
  "Create .cache directories based on saved custom settings and a
list of always-create directories."
  (let ((buffer (find-file-noselect "files/emacs-custom.el"))
        (expr nil)
        (cache-dirs))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (catch 'done
            (while (setq expr (read (current-buffer)))
              (when (eq (car expr) 'custom-set-variables)
                (-each (cdr expr)
                  (-lambda ((_quote (name value)))
                    (and (stringp value)
                         (string-match-p "\\.emacs\\.d/\\.cache" value)
                         (push value cache-dirs))))
                (throw 'done t)))))))
    (let* ((directories-config (-map 'file-name-directory cache-dirs))
           (directories-always (list
                                "~/.emacs.d/.cache/backups/"
                                ))
           (directories (-uniq
                         (-map 'expand-file-name
                               (-concat directories-config directories-always)))))
      (-each directories
        (lambda (dir)
          (unless (file-exists-p dir)
            (message "Creating directory: %s" dir)
            (mkdir dir :parents)))))))
