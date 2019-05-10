;;; keys.el --- Global key bindigns -*- lexical-binding: t -*-

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

;; Url & Browsing
(bind-key "C-c C-w" 'browse-url-at-point)
(bind-key "C-c w" 'browse-url)

;; refresh-like
(bind-key "M-<f5>" '(lambda () (interactive) (load-file (buffer-file-name))))
(bind-key "C-<f5>" 'revert-buffer)

;; Indenting and alignment
(bind-key "C-<f8>" 'indent-buffer)
(bind-key "C-<tab>" 'indent-defun)

(bind-key "C-x C-a" 'my-switch-buffer-LRU)

;; buffer cleanup
(bind-key "C-c u" 'cleanup-buffer)

;; Window navigation
(windmove-default-keybindings 'meta)
(bind-key "C-c 0" (my-with-preserved-window-config
                   "Delete current window and enter `recursive-edit'."
                   (delete-window)))
(bind-key "C-c 1" (my-with-preserved-window-config
                   "Delete other windows and enter `recursive-edit'."
                   (delete-other-windows)))

;; Easier buffer killing
(bind-key "M-k" 'my-kill-this-buffer)
(bind-key "M-K" 'kill-whole-line)
(bind-key "C-x C-k" 'kill-buffer-and-window)
(bind-key "C-x C-f" 'ido-find-file)
(bind-key "C-w" 'my-kill-region-or-word)
(bind-key [remap kill-ring-save] 'easy-kill)

;; imenu
(bind-key "M-," 'find-function)

;; sexp settings
(bind-key "C-x e" 'my-eval-and-replace)
(bind-key "C-;" 'eval-expression)

;; minibuffer history
(bind-key "C-p" 'previous-history-element minibuffer-local-map)
(bind-key "C-n" 'next-history-element minibuffer-local-map)

(bind-key "C-c =" 'count-matches)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; navigation

;; remove arrows, evil!
(mapc 'global-unset-key [[up] [down] [left] [right]])

;; paragraph
(bind-key "M-[" 'backward-paragraph)
(bind-key "M-]" 'forward-paragraph)

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
(bind-key "M-j" 'my-join-lines)

;; deleting stuff
(bind-key "C-<backspace>" 'my-kill-whitespace)

(bind-key "M-f" 'my-forward-word)
(bind-key "M-b" 'my-backward-word)

;; up/downcase
(bind-key "M-l" 'my-smart-downcase-word)
(bind-key "M-u" 'my-smart-upcase-word)
(bind-key "M-c" 'my-capitalize-word)

;; tabify/untabify

(bind-key "M-T" 'tabify)
(bind-key "M-U" 'untabify)

;; mark commands
(bind-key "C-`" 'my-push-mark-no-activate)
(bind-key "M-`" 'my-jump-to-mark)
(global-set-key [remap exchange-point-and-mark] 'my-exchange-point-and-mark-no-activate)

(bind-key "C-c TAB" 'fold-dwim-toggle)

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
(define-key 'help-command (kbd "C-m") 'sallet-man)

(bind-key "C-h D" 'describe-personal-keybindings)

(bind-keys :prefix "C-."
           :prefix-map ctl-dot-prefix-map
           :prefix-docstring "Second auxiliary map on C-."
  ("c" . create-scratch-buffer)
  ("C-c" . create-scratch-buffer-current-mode)
  ("z" . my-insert-no-move)
  ("-" . macrostep-expand)
  ("0" . my-kill-pp-eval-expression-window)
  ("s" . my-sprunge)
  ("r" . align-regexp)
  ("C-d" . my-change-identifier-style)
  ("/" . my-insert-date-iso))

(bind-keys :map ctl-dot-prefix-map
           :prefix "p"
           :prefix-map my-profiler-prefix-map
           :prefix-docstring "Prefix map for profiler functions"
  ("s" . profiler-start)
  ("e" . profiler-report)
  ("h" . profiler-stop)
  ("f" . elp-instrument-function)
  ("p" . elp-instrument-package)
  ("r" . elp-results)
  ("R" . elp-restore-all))

(bind-keys :prefix "C-. r"
           :prefix-map ctl-dot-repl-prefix-map
           :prefix-docstring "Map for starting repls")

(bind-key "C-. b"
          (defhydra buffer-hydra (:color blue)
            ("o" my-open-buffer-xdg "Open buffer with external application")))

(bind-key "C-. e" 'eval-region emacs-lisp-mode-map)

(bind-key "<XF86HomePage>" 'toggle-input-method)
(unbind-key "<insert>")
(bind-key "<insert> <insert>" 'my-toggle-buffer-input-methods)

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

(bind-key "C-z" 'repeat)

(provide 'keys)
;;; keys.el ends here
