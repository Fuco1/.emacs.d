(defun create-scratch-buffer (mode)
  "Create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive (list (if current-prefix-arg
                         (intern
                          (ido-completing-read
                           "Major mode to set: "
                           (let (r)
                             (mapatoms
                              (lambda (x)
                                (when (s-suffix? "-mode" (symbol-name x))
                                  (push x r))))
                             (mapcar 'symbol-name r))))
                       'emacs-lisp-mode)))
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (call-interactively mode)))

(defun create-scratch-buffer-current-mode ()
  "Create a new scratch buffer to work in and set its mode to current `major-mode'."
  (interactive)
  (create-scratch-buffer major-mode))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-restriction
    (widen)
    (narrow-to-defun)
    (indent-buffer)))

(defun my-kill-this-buffer ()
  "Kill the current buffer.
Warn me if I want to kill a scratch buffer."
  (interactive)
  (if (--any? (string-match-p it (buffer-name)) '("\\*scratch"
                                                  "\\*Messages\\*"))
      (when (y-or-n-p "Do you want really to kill this buffer?")
        (kill-buffer (current-buffer)))
    (kill-buffer (current-buffer))))

(defvar my-inhibit-buffer-cleanup nil)

;; TODO: add some abstraction for the exceptions
(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (unless my-inhibit-buffer-cleanup
    (let ((inhibit-read-only t))
      (when (or (called-interactively-p)
                (and (not (memq major-mode '(snippet-mode)))
                     (or (not (s-matches? "/var/www/html/" default-directory))
                         (s-matches? "orders-refactor" default-directory))
                     (not (s-matches? "dev/org-mode" default-directory))))
        (delete-trailing-whitespace)
        (unless (memq major-mode '(makefile-gmake-mode
                                   makefile-mode
                                   text-mode
                                   fundamental-mode))
          (untabify-buffer)))
      (set-buffer-file-coding-system 'utf-8))))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-buffer))

(defun my-create-directory-on-save (&optional _)
  (when buffer-file-name
    (let ((dir (file-name-directory buffer-file-name)))
      (when (and (not (file-exists-p dir))
                 (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
        (make-directory dir t)))))

(defun my-fix-multibyte-encoding ()
  "Fix the display of octal chars instead of decoded text."
  (set-buffer-multibyte nil)
  (set-buffer-multibyte t))

(defun my-kill-pp-eval-expression-window ()
  (interactive)
  (--when-let (--first (equal (buffer-name (window-buffer it)) "*Pp Eval Output*") (window-list))
    (delete-window it)))

(defun my-add-font-lock-face (beg end face)
  "Add face to region."
  (interactive (list
                (region-beginning)
                (region-end)
                (read-face-name "Face")))
  (put-text-property beg end 'font-lock-face face))
