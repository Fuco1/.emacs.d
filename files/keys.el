;;; Global key bindigns
;; TODO: move keys to apropriate packages in vendor.el, only keep the
;; bare minimum here.

;; setting the PC keyboard's various keys to
;; Super or Hyper, or emacs running on Windows.
(setq w32-pass-lwindow-to-system nil
      w32-pass-rwindow-to-system nil
      w32-pass-apps-to-system nil
      w32-lwindow-modifier 'super ; Left Windows key
      w32-rwindow-modifier 'super ; Right Windows key
      w32-apps-modifier 'hyper) ; Menu key

;;; stupid terminal key sequence remapping
(define-key key-translation-map [return] [?\r])
(define-key key-translation-map [?\C-\m] [(control m-key)])
(define-key function-key-map [return] nil)
(define-key function-key-map [?\r] nil)

(define-key key-translation-map [tab] [?\t])
(define-key key-translation-map [?\C-\i] [(control i-key)])
(define-key function-key-map [tab] nil)
(define-key function-key-map [?\t] nil)

(define-key key-translation-map [escape] [?\e])
(define-key input-decode-map [?\C-\[] [(control left_bracket)])
(define-key function-key-map [escape] nil)
(define-key function-key-map [?\e] nil)

;; Url & Browsing
(bind-key "C-c C-w" 'browse-url-at-point)
(bind-key "C-c w" 'browse-url)

;; Find stuff
(bind-key "<f2>" 'occur)
(bind-keys :map occur-mode-map
  ("n" . occur-next)
  ("p" . occur-prev)
  ("o" . occur-mode-display-occurrence))

;; is this safe binding?
(bind-key "C-'" 'repeat)

;; refresh-like
(bind-key "M-<f5>" '(lambda () (interactive) (load-file (buffer-file-name))))
;; TODO: gather all `universal-argument' bindings somewhere on one place
(bind-key "A-u" 'universal-argument)
;; TODO: find better prefix for this, too close to A-c which kills the
;; frame
(bind-keys :prefix "A-v"
           :prefix-map alt-v-prefix-map
           :prefix-docstring "A-v prefix map"
           ("A-m" . my-find-file-same-mode)
           ("A-x" . my-find-file-same-ext)
           ("A-s" . my-find-file-sudo))

;; Indenting and alignment
(bind-key "<f8>" 'align-regexp)
(bind-key "C-<f8>" 'indent-buffer)
(bind-key "C-<tab>" 'indent-defun)

(bind-keys :prefix "<f1>"
           :prefix-map f1-prefix-map
           :prefix-docstring "F1 prefix map (navigation)"
  ("<f2>" . (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
  ("<f3>" . view-echo-area-messages)
  ("<f4>" . ffap)
  ("<f5>" . (lambda () (interactive) (let ((default-directory "~")) (ido-find-file))))
  ("<f12>" . my-switch-to-scratch))

;; ibuffer > list-buffers
(bind-key "C-x C-b" 'ibuffer)
(bind-key "C-<m-key>" 'ido-switch-buffer)

;; buffer cleanup
(bind-key "C-c u" 'cleanup-buffer)

;; Window navigation
(windmove-default-keybindings 'meta)

;; Easier buffer killing
(bind-key "M-k" 'my-kill-this-buffer)
(bind-key "C-x C-k" 'kill-buffer-and-window)

;; imenu
(bind-key "M-," 'find-function)

;; sexp settings
(bind-key "C-x e" 'eval-and-replace)
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
(bind-key "M-{" 'backward-paragraph-select)
(bind-key "M-}" 'forward-paragraph-select)
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
;; active region
(bind-key "M-g a" 'beginning-of-region)
(bind-key "M-g e" 'end-of-region)

;; # move into
(bind-key "C-M-d" 'down-list)

(bind-key "C-c <up>" 'copy-previous-line)
(bind-key "C-c <down>" 'copy-next-line)
(bind-key "M-<up>" 'move-line-up)
(bind-key "M-<down>" 'move-line-down)

;; opening new lines. C-o can be called from any point on the line,
;; ret from the end only
(bind-key "RET" 'my-newline)
(bind-key "C-o" 'my-open-line)
(bind-key "C-S-o" 'forward-line-and-indent)
(bind-key "M-j"
          (lambda ()
            (interactive)
            (join-line -1)))

;; deleting stuff
(bind-key "C-<i-key>" 'backward-kill-word)
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
(bind-key "C-`" 'push-mark-no-activate)
(bind-key "M-`" 'jump-to-mark)
(global-set-key [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

;; customize
(bind-keys :prefix "C-c c"
           :prefix-map ctl-c-c-map
           :prefix-docstring "Customize map"
  ("v" . customize-variable)
  ("f" . customize-face)
  ("g" . customize-group))

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
  ("p f" . elp-instrument-function)
  ("p r" . elp-results)
  ("p R" . elp-restore-all)
  ("c" . create-scratch-buffer)
  ("k" . browse-kill-ring)
  ("m s" . kmacro-set-counter)
  ("m a" . kmacro-add-counter)
  ("m f" . kmacro-set-format)
  ("z" . my-insert-no-move)
  ("-" . macrostep-expand)
  ("0" . my-kill-pp-eval-expression-window))

(bind-key "C-. e" 'eval-region emacs-lisp-mode-map)

(bind-key "<XF86HomePage>" 'toggle-input-method)
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
  ("g" . (lambda () "Toggle on greek-mizuochi input method." (interactive) (set-input-method "greek-mizuochi"))))

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

(bind-keys :prefix "C-c m"
           :prefix-map ctl-c-m-map
           :prefix-docstring "Magit map"
  ("b" . magit-key-mode-popup-branching)
  ("c" . magit-key-mode-popup-committing)
  ("d" . magit-key-mode-popup-dispatch)
  ("f" . magit-key-mode-popup-fetching)
  ("i" . magit-key-mode-popup-diff-options)
  ("l" . magit-key-mode-popup-logging)
  ("m" . magit-key-mode-popup-merging)
  ("p" . magit-key-mode-popup-pushing)
  ("v" . magit-branch-manager)
  ("s" . magit-status))

;; zapping
(bind-key "M-z" 'zap-up-to-char)
(bind-key "M-Z" 'zap-to-char)

(defun my-quoted-insert-and-backward (arg)
  "Just like `quoted-insert' but moves the point before the
inserted character."
  (interactive "*p")
  (save-excursion
    (call-interactively 'quoted-insert)))

(bind-key "C-z" 'my-quoted-insert-and-backward)

;; M-s map
(bind-key "M-s RET" 'skeleton-easy-regexp-display-abbrev)
(bind-key "M-s c" 'calendar)

;; M-g map
(bind-key "M-g RET" 'skeleton-display-abbrev)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jump to "logical" top/bottom of buffer in listing buffers
;; TODO: turn this into a package

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
