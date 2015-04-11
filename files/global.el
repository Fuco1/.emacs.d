;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Small fringes
(set-fringe-mode '(1 . 1))

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

;; dir-local variables
(dir-locals-set-class-variables
 'pw-orders-refactor
 '((nil
    (my-svn-branch . "/fw/branches-devel/mg-orders-refactor/")
    (my-svn-trunk . "/fw/trunk/")
    (my-pw-root . "/var/www/html/devel/mg/orders-refactor")
    (my-pw-test-uuid . "test"))))

(dir-locals-set-directory-class
 "/scp:speedy:/var/www/html/devel/mg/orders-refactor/" 'pw-orders-refactor)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some text-mode settings

(setq-default cursor-type 'box)
(dolist (hook '(
                LaTeX-mode-hook
                org-mode-hook
                markdown-mode-hook
                gnus-article-mode-hook
                textile-mode-hook
                ))
  (add-hook hook 'my-init-text-based-modes))

;; Use variable width font faces in current buffer
(defun my-buffer-face-mode-variable ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (variable-pitch-mode))

;; Use monospaced font faces in current buffer
(defun my-buffer-face-mode-fixed ()
  "Sets a fixed width (monospace) font in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Consolas" :height 100))
  (buffer-face-mode))

(defun my-init-text-based-modes ()
  (my-buffer-face-mode-variable)
  (setq cursor-type 'bar)
  (turn-on-visual-line-mode)
  (smartparens-mode 1))

(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)

(defun my-try-smerge ()
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
