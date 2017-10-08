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
(bind-key "M-j"
          (lambda ()
            (interactive)
            (join-line -1)))

;; deleting stuff
(bind-key "C-<backspace>" 'my-kill-whitespace)

(bind-key "M-f" 'my-forward-word)
(bind-key "M-b" 'my-backward-word)

;; up/downcase
(bind-key "M-l" 'my-smart-downcase-word)
(bind-key "M-u" 'my-smart-upcase-word)
(bind-key "M-c" 'my-capitalize-word)

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

(bind-key "C-. e" 'eval-region emacs-lisp-mode-map)

(bind-key "<XF86HomePage>" 'toggle-input-method)
(unbind-key "<insert>")
(bind-key "<insert> <insert>" 'my-toggle-buffer-input-methods)
(bind-key "C-. i"
          (defhydra input-methods-hydra (:color blue :hint nil)
            "
European           | Indian           | Asian    | Special
-------------------+------------------+----------+-----------------------------
_s_lovak             | devanagari (_h_)   | _j_apanese | _w_orkman
_c_zech              | de_v_anagari-trans |          | ipa-_x_-sampa
german (_d_)         |                  |          | TeX (_t_)
_i_talian            |                  |          |
_f_rench             |                  |          |
_l_atin              |                  |          |
_g_reek              |                  |          |
_r_ussian            |                  |          | set input _m_ethod
cyrillic-trans (_q_) |                  |          | toggle input m_e_thod
"
            ("m" set-input-method)
            ("e" toggle-input-method)
            ("s" (lambda () "Toggle on slovak-prog-2 input method." (interactive) (set-input-method "slovak-prog-2")))
            ("c" (lambda () "Toggle on czech input method." (interactive) (set-input-method "czech")))
            ("r" (lambda () "Toggle on russian-computer input method." (interactive) (set-input-method "russian-computer")))
            ("q" (lambda () "Toggle on cyrillic-translit input method." (interactive) (set-input-method "cyrillic-translit")))
            ("i" (lambda () "Toggle on italian-keyboard input method." (interactive) (set-input-method "italian-keyboard")))
            ("d" (lambda () "Toggle on german input method." (interactive) (set-input-method "german")))
            ("t" (lambda () "Toggle on TeX input method." (interactive) (set-input-method "TeX")))
            ("l" (lambda () "Toggle on latin-macrons input method." (interactive) (set-input-method "latin-macrons")))
            ("f" (lambda () "Toggle on french-keyboard input method." (interactive) (set-input-method "french-keyboard")))
            ("g" (lambda () "Toggle on greek-mizuochi input method." (interactive) (set-input-method "greek-mizuochi")))
            ("j" (lambda () "Toggle on japanese input method." (interactive) (set-input-method "japanese")))
            ("h" (lambda () "Toggle on devanagari-kyoto-harvard input method." (interactive) (set-input-method "devanagari-kyoto-harvard")))
            ("v" (lambda () "Toggle on devanagari-translit input method." (interactive) (set-input-method "devanagari-translit")))
            ("w" (lambda () "Toggle on workman input method." (interactive) (set-input-method "english-workman")))
            ("x" (lambda () "Toggle on ipa-x-sampa input method." (interactive) (set-input-method "ipa-x-sampa")))))

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

;; Hydras for the F-keys
(bind-key "<f1>"
          (defhydra f1-hydra (:color blue)
            "F1 hydra: navigation"
            ("<f1>" ibuffer "ibuffer")
            ("<f2>" my-visit-init-file "Visit the user init file" )
            ("<f3>" view-echo-area-messages "Visit messages")
            ("<f4>" ffap "Find file at point")
            ("<f5>" my-find-file-in-home "Find file in the home directory")
            ("<f8>" projectile-dired "Goto project root")
            ("<f10>" my-goto-current-clocked-task "Go to current clocked task")))

(bind-key "<f7>"
          (defhydra f7-hydra (:color blue)
            "F7 hydra: grep/find"
            ("<f6>" my-projectile-rgrep "projectile-rgrep")
            ("<f7>" rgrep "rgrep")
            ("<f8>" dired-list-find-file "dired-list-find-file")
            ("<f9>" dired-list-grep "dired-list-grep")))

(provide 'keys)
;;; keys.el ends here
