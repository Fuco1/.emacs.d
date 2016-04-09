;;; global.el --- Global settings which don't fit anywhere else.

;;; Commentary:

;; Catch-it-all bag of global configuration.

;;; Code:

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Small fringes
(set-fringe-mode (cons 8 1))

;; Trailing whitespace is unnecessary
(add-hook 'before-save-hook 'cleanup-buffer-safe)
(add-hook 'before-save-hook 'my-create-directory-on-save)

;; UTF
(setq locale-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(if (eq system-type 'windows-nt)
    (progn
      (set-selection-coding-system 'utf-16le-dos)
      (set-terminal-coding-system 'cp1250))
  (set-selection-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8))
(prefer-coding-system 'utf-8)

;; cua-selection-mode for all the goodies ;)
(cua-selection-mode t)

(setq edebug-inhibit-emacs-lisp-mode-bindings t)

(defun my-try-smerge ()
  "Try to turn on `smerge-mode'."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))
(add-hook 'find-file-hook 'my-try-smerge t)

(defadvice display-message-or-buffer (before ansi-color activate)
  "Process ANSI color codes in shell output."
  (let ((buf (ad-get-arg 0)))
    (when (and (bufferp buf)
               (string= (buffer-name buf) "*Shell Command Output*"))
      (require 'ansi-color)
      (with-current-buffer buf
        (ansi-color-apply-on-region (point-min) (point-max))))))

;;; global.el ends here
