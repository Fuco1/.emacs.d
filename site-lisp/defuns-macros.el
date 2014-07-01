(defmacro save-kill-ring (&rest body)
  "Save the kill ring and execute the BODY.  After BODY is
executed, the kill-ring value is restored to the state before
BODY was executed"
  (declare (indent 0))
  ;; initialize new binding for the ,@body.  It starts with the global
  ;; original value.  After ,@body is executed, the new binding is
  ;; thrown away!
  `(let ((kill-ring kill-ring))
     ,@body))

(defmacro save-buffer-list (&rest forms)
  "Execute FORMS while preserving the list of opened buffers.
After the FORMS are executed, close all buffers that were not
opened before.  This does not re-open closed buffers as that
might often be impossible."
  (declare (indent 0))
  `(let ((old-buffer-list (buffer-list)))
     ,@forms
     (mapc (lambda (buffer)
             (when (member buffer old-buffer-list)
               (kill-buffer buffer)))
           (buffer-list))))

;; this is still broken :/
(defmacro with-files-in-dir (directory &rest forms)
  (declare (indent 1))
  `(save-buffer-list
     (save-excursion
       (mapc (lambda (file)
               (find-file (concat ,directory "/" file))
               ,@forms)
             (directory-files ,directory)))))

(defmacro with-lines-narrow (&rest forms)
  (declare (indent 0))
  `(while (not (eobp))
     (let ((beg (point-at-bol))
           (end (point-at-eol)))
       (save-excursion
         (save-restriction
           (narrow-to-region beg end)
           ,@forms)))
     (forward-line)))

(defmacro with-every-line (&rest forms)
  "Execute FORMS on each line following point to the end of file."
  (declare (indent 0))
  `(progn
     (beginning-of-line)
     (while (not (eobp))
       ,@forms
       (forward-line))))

(defmacro fix-reset-after-each (&rest forms)
  (declare (indent 0))
  `(progn
     ,@(apply 'append (mapcar (lambda (form) (list '(goto-char (point-min)) form)) forms))))

(defvar my-macro-names
  '(
    "save-buffer-list"
    "save-kill-ring"
    "with-files-in-dir"
    "with-lines-narrow"
    "with-every-line"
    "fix-reset-after-each"
    "bind-keys"
    ))

(font-lock-add-keywords 'emacs-lisp-mode `((,(concat "(\\<"
                                                     (regexp-opt my-macro-names 'paren)
                                                     "\\>")
                                            1 font-lock-keyword-face)) 'append)
