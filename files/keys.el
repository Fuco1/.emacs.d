;;; keys.el --- Global key bindigns

;;; Commentary:

;; Global keybindings.

;;; Code:

(require 'use-package)

;; We do not want to quit emacs, ever.
(unbind-key "C-x C-c")

;;; stupid terminal key sequence remapping
(define-key key-translation-map [escape] [?\e])
(define-key input-decode-map [?\C-\[] [(control left_bracket)])
(define-key function-key-map [escape] nil)
(define-key function-key-map [?\e] nil)

(bind-key "<delete>" 'god-local-mode)

;; unfortunately C-. can't be represented by anything customize allows
;; for this variable.
(setq smerge-command-prefix (kbd "C-. m"))

(bind-key "C-. /" 'my-insert-date-iso)

;; Url & Browsing
(bind-key "C-c C-w" 'browse-url-at-point)
(bind-key "C-c w" 'browse-url)


(bind-key "M-=" 'count-words)

;; refresh-like
(bind-key "M-<f5>" '(lambda () (interactive) (load-file (buffer-file-name))))

;; Indenting and alignment
(bind-key "C-<f8>" 'indent-buffer)
(bind-key "C-<tab>" 'indent-defun)

(bind-key "C-x C-a" 'my-switch-buffer-LRU)

;; buffer cleanup
(bind-key "C-c u" 'cleanup-buffer)

;; Window navigation
(windmove-default-keybindings 'meta)

;; Easier buffer killing
(bind-key "M-k" 'my-kill-this-buffer)
(bind-key "C-x C-k" 'kill-buffer-and-window)
(bind-key "C-x C-f" 'ido-find-file)
(bind-key "C-w" 'my-kill-region-or-word)

;; imenu
(bind-key "M-," 'find-function)

;; sexp settings
(bind-key "C-x e" 'my-eval-and-replace)
(bind-key "C-;" 'eval-expression)

;; minibuffer history
(bind-key "C-p" 'previous-history-element minibuffer-local-map)
(bind-key "C-n" 'next-history-element minibuffer-local-map)

(bind-key "C-c V" 'view-clipboard)
(bind-key "C-c =" 'count-matches)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; navigation

;; remove arrows, evil!
(mapc 'global-unset-key [[up] [down] [left] [right]])

;; # move by (forw/back)
;; character
(bind-key "C-f" 'forward-char)
(bind-key "C-b" 'backward-char)
;; word
(bind-key "M-f" 'forward-word)
(bind-key "M-b" 'backward-word)
;; line
(bind-key "C-n" 'next-line)
(bind-key "C-p" 'previous-line)
;; sentence
(bind-key "M-e" 'forward-sentence)
(bind-key "M-a" 'backward-sentence)
;; paragraph
(bind-key "M-[" 'backward-paragraph)
(bind-key "M-]" 'forward-paragraph)
;; screen
(bind-key "C-v" 'scroll-up-command)
(bind-key "M-v" 'scroll-down-command)
;; sexp
(bind-key "C-M-f" 'forward-sexp)
(bind-key "C-M-b" 'backward-sexp)
;; list
(bind-key "C-M-n" 'forward-list)
(bind-key "C-M-p" 'backward-list)
(bind-key "C-c f" (lambda () (interactive) (sp-beginning-of-sexp 2)))
(bind-key "C-c b" (lambda () (interactive) (sp-beginning-of-sexp -2)))

(bind-key "C-c q s" 'query-replace)
(bind-key "C-c q r" 'query-replace-regexp)

;; # move to (beg/end)
;; line
;; swap C-a and M-m, back-to-indentation is much more common
(bind-key "M-m" 'move-beginning-of-line)
(bind-key "C-a" 'my-back-to-indentation-or-beginning)
(bind-key "C-e" 'my-end-of-code-or-line)
;; buffer
(unbind-key "<home>")
(unbind-key "<end>")
(bind-key "M-<" 'beginning-of-buffer)
(bind-key "C-M-," 'beginning-of-buffer)
(bind-key "M->" 'end-of-buffer)
(bind-key "C-M-." 'end-of-buffer)

;; defun
(bind-key "M-p" 'beginning-of-defun)
(bind-key "M-n" 'end-of-defun)

;; # move into
(bind-key "C-M-d" 'down-list)

(bind-key "M-<up>" 'my-move-line-up)
(bind-key "M-<down>" 'my-move-line-down)

;; opening new lines. C-o can be called from any point on the line,
;; ret from the end only
(bind-key "RET" 'my-newline)
(bind-key "C-o" 'my-open-line)
(bind-key "C-S-o" 'my-forward-line-and-indent)
(bind-key "M-j"
          (lambda ()
            (interactive)
            (join-line -1)))

;; deleting stuff
(bind-key "C-<backspace>" 'my-kill-whitespace)
(bind-key "C-c d" 'my-kill-entire-line)

;; up/downcase
(bind-key "M-l" 'my-smart-downcase-word)
(bind-key "M-u" 'my-smart-upcase-word)
(bind-key "M-c" 'my-capitalize-word)

;;;;; multiple cursors
(bind-key "C-c C-S-c" 'mc/edit-lines)
(bind-key "s-." 'mc/mark-next-like-this)
(bind-key "s-," 'mc/mark-previous-like-this)
(bind-key "s-\\" 'mc/mark-more-like-this-extended)
(bind-key "s-/" 'mc/mark-all-like-this-dwim)
(bind-key "H-SPC" 'set-rectangular-region-anchor)

(add-hook 'html-mode-hook (lambda () (bind-key "C-c <deletechar>" 'sgml-delete-tag html-mode-map)))

;; mark commands
(bind-key "C-`" 'my-push-mark-no-activate)
(bind-key "M-`" 'my-jump-to-mark)
(global-set-key [remap exchange-point-and-mark] 'my-exchange-point-and-mark-no-activate)

(bind-keys :prefix "C-h e"
           :prefix-map ctl-h-e-map
           :prefix-docstring "List find/help map"
  ("b" . free-keys)
  ("d" . info-lookup-symbol)
  ("f" . find-face-definition)
  ("i" . info-apropos)
  ("k" . find-function-on-key)
  ("l" . find-library)
  ("v" . find-variable)
  ("V" . apropos-value))
;; help-command seems to be some sort of hack, so we have to
;; `define-key' it instead.
(define-key 'help-command (kbd "C-v") 'find-variable-at-point)
(define-key 'help-command (kbd "C-f") 'find-function-at-point)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)

(bind-key "C-h D" 'describe-personal-keybindings)

(bind-keys :prefix "C-."
           :prefix-map ctl-dot-prefix-map
           :prefix-docstring "Second auxiliary map on C-."
  ("c" . create-scratch-buffer)
  ("C-c" . create-scratch-buffer-current-mode)
  ;; ("k" . browse-kill-ring) ;use helm
  ("z" . my-insert-no-move)
  ("-" . macrostep-expand)
  ("0" . my-kill-pp-eval-expression-window)
  ("s" . my-sprunge)
  ("r" . align-regexp)
  ("C-d" . my-change-identifier-style))

(bind-keys :map ctl-dot-prefix-map
           :prefix "p"
           :prefix-map my-profiler-prefix-map
           :prefix-docstring "Prefix map for profiler functions"
  ("f" . elp-instrument-function)
  ("p" . elp-instrument-package)
  ("r" . elp-results)
  ("R" . elp-restore-all))

(bind-key "C-. e" 'eval-region emacs-lisp-mode-map)

(bind-key "<XF86HomePage>" 'toggle-input-method)
(unbind-key "<insert>")
(bind-key "<insert> <insert>" 'my-toggle-buffer-input-methods)
(bind-keys :prefix "C-. i"
           :prefix-map ctl-dot-i-prefix-map
           :prefix-docstring "Input method map."
  ("m" . set-input-method)
  ("e" . toggle-input-method)
  ("s" . (lambda () "Toggle on slovak-prog-2 input method." (interactive) (set-input-method "slovak-prog-2")))
  ("c" . (lambda () "Toggle on czech input method." (interactive) (set-input-method "czech")))
  ("r" . (lambda () "Toggle on russian-computer input method." (interactive) (set-input-method "russian-computer")))
  ("q" . (lambda () "Toggle on cyrillic-translit input method." (interactive) (set-input-method "cyrillic-translit")))
  ("i" . (lambda () "Toggle on italian-keyboard input method." (interactive) (set-input-method "italian-keyboard")))
  ("d" . (lambda () "Toggle on german input method." (interactive) (set-input-method "german")))
  ("t" . (lambda () "Toggle on TeX input method." (interactive) (set-input-method "TeX")))
  ("l" . (lambda () "Toggle on latin-macrons input method." (interactive) (set-input-method "latin-macrons")))
  ("f" . (lambda () "Toggle on french-keyboard input method." (interactive) (set-input-method "french-keyboard")))
  ("g" . (lambda () "Toggle on greek-mizuochi input method." (interactive) (set-input-method "greek-mizuochi")))
  ("j" . (lambda () "Toggle on japanese input method." (interactive) (set-input-method "japanese")))
  ("h" . (lambda () "Toggle on devanagari-kyoto-harvard input method." (interactive) (set-input-method "devanagari-kyoto-harvard")))
  ("v" . (lambda () "Toggle on devanagari-translit input method." (interactive) (set-input-method "devanagari-translit")))
  ("x" . (lambda () "Toggle on ipa-x-sampa input method." (interactive) (set-input-method "ipa-x-sampa"))))

(bind-key "H-u" 'universal-argument)
(bind-key "H-u" 'universal-argument-more universal-argument-map)
(bind-key "H-0" 'digit-argument)
(bind-key "H-1" 'digit-argument)
(bind-key "H-2" 'digit-argument)
(bind-key "H-3" 'digit-argument)
(bind-key "H-4" 'digit-argument)
(bind-key "H-5" 'digit-argument)
(bind-key "H-6" 'digit-argument)
(bind-key "H-7" 'digit-argument)
(bind-key "H-8" 'digit-argument)
(bind-key "H-9" 'digit-argument)
(bind-key "H--" 'negative-argument)

(bind-keys :prefix "C-c s"
           :prefix-map ctl-c-s-map)

(bind-keys :prefix "C-c C-v"
           :prefix-map ctl-c-ctl-v-map
  ("p" . my-pull-word)
  ("j" . my-join-word))

;; zapping
(bind-key "M-z" 'zap-up-to-char)
(bind-key "M-Z" 'zap-to-char)

(defun my-quoted-insert-and-backward (arg)
  "Just like `quoted-insert' but moves the point before the
inserted character."
  (interactive "*p")
  (save-excursion
    (call-interactively 'quoted-insert)))

(bind-key "C-z" 'repeat)

;; M-s map
(bind-key "M-s RET" 'skeleton-easy-regexp-display-abbrev)
(bind-key "M-s c" 'calendar)

;; M-g map
(bind-key "M-g RET" 'skeleton-display-abbrev)

;; Hydras for the F-keys
(bind-key "<f1>"
          (defhydra f1-hydra (:color blue)
            "F1 hydra: navigation"
            ("<f1>" ibuffer "ibuffer")
            ("<f2>" my-visit-init-file "Visit the user init file" )
            ("<f3>" view-echo-area-messages "Visit messages")
            ("<f4>" ffap "Find file at point")
            ("<f5>" my-find-file-in-home "Find file in the home directory")
            ("<f10>" my-goto-current-clocked-task "Go to current clocked task")))

(bind-key "<f7>"
          (defhydra f7-hydra (:color blue)
            "F7 hydra: grep/find"
            ("<f6>" my-projectile-rgrep "projectile-rgrep")
            ("<f7>" rgrep "rgrep")
            ("<f8>" dired-list-find-file "dired-list-find-file")
            ("<f9>" dired-list-grep "dired-list-grep")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jump to "logical" top/bottom of buffer in listing buffers

(defmacro my-special-buffer-back-to-top (mode &rest forms)
  (declare (indent 1))
  (let ((fname (intern (concat "my-" (symbol-name mode) "-back-to-top")))
        (mode-map (intern (concat (symbol-name mode) "-mode-map")))
        (mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
    `(progn
       (defun ,fname ()
         (interactive)
         (let ((p (point)))
           (goto-char (point-min))
           ,@forms
           (when (= p (point))
             (goto-char (point-min)))))
       (add-hook ',mode-hook (lambda () (define-key ,mode-map [remap beginning-of-buffer] ',fname))))))

(defmacro my-special-buffer-jump-to-bottom (mode &rest forms)
  (declare (indent 1))
  (let ((fname (intern (concat "my-" (symbol-name mode) "-jump-to-bottom")))
        (mode-map (intern (concat (symbol-name mode) "-mode-map")))
        (mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
    `(progn
       (defun ,fname ()
         (interactive)
         (let ((p (point)))
           (goto-char (point-max))
           ,@forms
           (when (= p (point))
             (goto-char (point-max)))))
       (add-hook ',mode-hook (lambda () (define-key ,mode-map [remap end-of-buffer] ',fname))))))

(my-special-buffer-back-to-top dired
  (while (not (ignore-errors (dired-get-filename)))
    (dired-next-line 1)))
(my-special-buffer-jump-to-bottom dired
  (dired-previous-line 1))

(my-special-buffer-back-to-top occur
  (occur-next 1))
(my-special-buffer-jump-to-bottom occur
  (occur-prev 1))

(my-special-buffer-back-to-top ibuffer
  (ibuffer-forward-line 1))
(my-special-buffer-jump-to-bottom ibuffer
  (ibuffer-backward-line 1))

(my-special-buffer-back-to-top vc-dir
  (vc-dir-next-line 1))
(my-special-buffer-jump-to-bottom vc-dir
  (vc-dir-previous-line 1))

(my-special-buffer-back-to-top bs
  (bs-down 2))
(my-special-buffer-jump-to-bottom bs
  (bs-up 1)
  (bs-down 1))

(my-special-buffer-back-to-top recentf-dialog
  (my-recentf-next-file 3))
(my-special-buffer-jump-to-bottom recentf-dialog
  (my-recentf-previous-file 2))

(provide 'keys)
;;; keys.el ends here
