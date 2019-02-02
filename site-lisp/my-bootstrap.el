;; -*- lexical-binding: t -*-

(require 'dash)
(require 'f)
(require 's)

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
                  (-lambda ((_quote (_name value)))
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

;; TODO: we could probably use paths relative to this file instead of
;; the prefix format stuff
(defun my-setup-load-path ()
  "Add extra entries to `load-path', such as paths to dev
  versions of packages."
  (let ((prefix (if (getenv "CI") "/build/Fuco1" "")))
    (add-to-list 'load-path "/home/matus/dev/c++/ledger/lisp")
    (add-to-list 'load-path (format "~%s/.emacs.d/site-lisp/" prefix))
    (add-to-list 'load-path (format "~%s/.emacs.d/files/" prefix))
    (add-to-list 'load-path (format "~%s/.emacs.d/site-lisp/special/" prefix))
    (mapc (apply-partially 'add-to-list 'load-path) (f-directories (format "~%s/.emacs.d/projects" prefix)))
    (mapc (apply-partially 'add-to-list 'load-path) (f-directories (format "~%s/.emacs.d/dev/" prefix)))
    (mapc (lambda (dir) (load (concat dir "/" (f-base dir) "-autoloads.el") t t))
          (f-directories (format "~%s/.emacs.d/dev/" prefix)))))

(provide 'my-bootstrap)
