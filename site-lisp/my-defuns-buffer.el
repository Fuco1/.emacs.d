;;; my-defuns-buffer.el --- Buffer related functions

;;; Commentary:

;; Buffer related functions

;;; Code:

(require 'pcase)
(require 'dash)
(require 's)

(defun my-scratch-autodetect-mode (beg end pre-change)
  "Detect major mode when text is first inserted into the buffer."
  (when (and (= 0 pre-change)
             (= beg (point-min)))
    (set-auto-mode)
    (pcase major-mode
      (`json-mode
       (json-mode-beautify)))))

;;;###autoload
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
        bufname
        (root (expand-file-name ".cache/" user-emacs-directory)))
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (make-directory root t)
    (write-file (concat root "/" bufname))
    (call-interactively mode)
    (add-hook 'after-change-functions 'my-scratch-autodetect-mode 'append 'local)))

;;;###autoload
(defun create-scratch-buffer-current-mode ()
  "Create a new scratch buffer to work in and set its mode to current `major-mode'."
  (interactive)
  (create-scratch-buffer major-mode))

;;;###autoload
(defun untabify-buffer ()
  (interactive)
  (my-untabify-indent (point-min) (point-max)))

(defun my-untabify-indent (start end)
  "Convert all indentation tabs in region to multiple spaces, preserving columns."
  (interactive "r")
  (let ((c (current-column)))
    (save-excursion
      (save-restriction
        (narrow-to-region (point-min) end)
        (goto-char start)
        (while (re-search-forward "^ *\t" nil t)
          (forward-char -1)
          (let ((tab-beg (point))
                (indent-tabs-mode nil)
                column)
            (skip-chars-forward "\t")
            (setq column (current-column))
            (delete-region tab-beg (point))
            (indent-to column))
          (beginning-of-line))))
    (move-to-column c)))

;;;###autoload
(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

;;;###autoload
(defun indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-restriction
    (widen)
    (narrow-to-defun)
    (indent-buffer)))

;;;###autoload
(defun my-kill-this-buffer ()
  "Kill the current buffer.
Warn me if I want to kill a scratch buffer."
  (interactive)
  (if (--any? (string-match-p it (buffer-name)) '(
                                                  "\\*Messages\\*"
                                                  ))
      (when (y-or-n-p "Do you want really to kill this buffer?")
        (kill-buffer (current-buffer)))
    (kill-buffer (current-buffer))))

(defun my-copy-buffer-filename-as-kill ()
  "Copy current buffer's filename to `kill-ring'."
  (interactive)
  (kill-new buffer-file-name))

(defun my-open-buffer-xdg ()
  "Open the current buffer's file with xdg-open(1)."
  (interactive)
  (--if-let (or (buffer-file-name)
                (ignore-errors (dired-get-file-for-visit)))
      (start-process "" nil "xdg-open" it)
    (user-error "No file to open")))

(define-minor-mode my-inhibit-buffer-cleanup-mode
  "If enabled, do not clean up whitespace on buffer save."
  :init-value nil)

;; TODO: add some abstraction for the exceptions
;;;###autoload
(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (unless my-inhibit-buffer-cleanup-mode
    (let ((inhibit-read-only t))
      (when (or (called-interactively-p)
                (and (not (memq major-mode '(snippet-mode)))
                     (or (not (s-matches? "/var/www/html/" default-directory))
                         (s-matches? "orders-refactor" default-directory))
                     (not (s-matches? "dev/org-mode" default-directory))))
        (delete-trailing-whitespace)
        (unless (or (memq major-mode '(makefile-gmake-mode
                                       makefile-mode
                                       text-mode
                                       fundamental-mode))
                    indent-tabs-mode)
          (untabify-buffer)))
      (set-buffer-file-coding-system 'utf-8))))

;;;###autoload
(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-buffer))

;;;###autoload
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

;;;###autoload
(defun my-kill-pp-eval-expression-window ()
  (interactive)
  (--when-let (--first (equal (buffer-name (window-buffer it)) "*Pp Eval Output*") (window-list))
    (delete-window it)))

;;;###autoload
(defun my-add-font-lock-face (beg end face)
  "Add face to region."
  (interactive (list
                (region-beginning)
                (region-end)
                (read-face-name "Face")))
  (put-text-property beg end 'font-lock-face face))

(provide 'my-defuns-buffer)
;;; my-defuns-buffer.el ends here
