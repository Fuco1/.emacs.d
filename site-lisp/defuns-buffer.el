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

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (unless my-inhibit-buffer-cleanup
    (let ((inhibit-read-only t))
      (unless (memq major-mode '(makefile-gmake-mode
                                 text-mode
                                 fundamental-mode))
        (untabify-buffer))
      (unless (memq major-mode '(snippet-mode))
        (delete-trailing-whitespace))
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

(defun my-switch-to-scratch (buffer)
  "Switch to a scratch buffer."
  (interactive (list (car
                      (split-string
                       (ido-completing-read
                        "Scratch buffer: "
                        (->> (buffer-list)
                          (-map 'buffer-name)
                          (--filter (string-match-p "\\*scratch.*" it))
                          (--map (concat it
                                         " ["
                                         (with-current-buffer (get-buffer-create it)
                                           (symbol-name major-mode))
                                         "]")))
                        nil t) " "))))
  (switch-to-buffer buffer))

(defun my-fix-multibyte-encoding ()
  "Fix the display of octal chars instead of decoded text."
  (set-buffer-multibyte nil)
  (set-buffer-multibyte t))

(defun my-kill-pp-eval-expression-window ()
  (interactive)
  (--when-let (--first (equal (buffer-name (window-buffer it)) "*Pp Eval Output*") (window-list))
    (delete-window it)))
