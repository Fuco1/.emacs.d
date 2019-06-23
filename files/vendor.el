;;; vendor.el --- Package configuration

;;; Commentary:
;;; Code:

(require 'use-package)

;; TODO: make this also load the regular file so we never forget
;; loading either that or the tangled org source.
;; TODO: check the timestamps on org file and the tangled result file
;; and re-tangle if necessary
(defun my-load-or-tangle (config-file)
  "Load CONFIG-FILE.

If the file CONFIG-FILE-tangled.el exists, load it.  If not,
first run `org-babel-tangle-file' on CONFIG-FILE.org and then
load the result."
  (let ((source (concat config-file ".org"))
        (result (concat config-file "-tangled.el")))
    (if (file-exists-p result)
        (load result)
      (require 'org)
      (org-babel-tangle-file source)
      (load result))))

(defvar my-vendor-file (f-this-file)
  "Path to this file")

(defun load-relative (module)
  "Load MODULE relative to this file"
  (load (f-join (f-parent my-vendor-file) module)))

(use-package hydra
  :straight t
  :bind (("<f1>" . f1-hydra/body)
         ("<f7>" . f7-hydra/body)
         ("C-. i" . input-methods-hydra/body))
  :config
  (defhydra f1-hydra (:color blue)
    "F1 hydra: navigation"
    ("<f1>" ibuffer "ibuffer")
    ("<f2>" my-visit-init-file "Visit the user init file" )
    ("<f3>" view-echo-area-messages "Visit messages")
    ("<f4>" ffap "Find file at point")
    ("<f5>" my-find-file-in-home "Find file in the home directory")
    ("<f6>" my-find-dependency "Find dependency")
    ("<f8>" projectile-dired "Goto project root")
    ("<f10>" my-goto-current-clocked-task "Go to current clocked task"))

  (defhydra f7-hydra (:color blue)
    "F7 hydra: grep/find"
    ("<f6>" my-projectile-rgrep "projectile-rgrep")
    ("<f7>" rgrep "rgrep")
    ("<f8>" dired-list-find-file "dired-list-find-file")
    ("<f9>" dired-list-grep "dired-list-grep"))

  (defhydra input-methods-hydra (:color blue :hint nil)
    "
European           | Indian           | Asian    | Special
-------------------+------------------+----------+-----------------------------
_s_lovak             | devanagari (_h_)   | _j_apanese | _w_orkman
_c_zech              | de_v_anagari-trans |          | ipa-_x_-sampa
_p_olish             |                  |          | TeX (_t_)
german (_d_)         |                  |          |
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
    ("p" (lambda () "Toggle on polish-slash input method." (interactive) (set-input-method "polish-slash")))
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

;; TODO: required by dired-images, move it there
(use-package eimp
  :straight t
  :after image-mode)

;; TODO: required by dired-tangled/defs, move it there
(use-package make-it-so
  :straight t
  :defer t)

(use-package ag
  :straight t
  :commands (ag ag-regexp ag-files ag-project-dired ag-dired ag-dired-regexp)
  :bind (("<f9>" . f9-hydra/body))
  :config
  (progn
    (defhydra f9-hydra (:color blue :hint nil)
      "
silversearcher: ag(1)

 Files                  Directories
------------------------------------------
 _<f8>_ ag-files          _8_ ag-project-dired
 _<f9>_ ag                _9_ ag-dired
_<f10>_ ag-regexp         _0_ ag-dired-regexp
"
      ("<f8>" ag-files "ag-files")
      ("<f9>" ag "ag")
      ("<f10>" ag-regexp "ag-regexp")
      ("8" ag-project-dired "ag-project-dired")
      ("9" ag-dired "ag-dired")
      ("0" ag-dired-regexp "ag-dired-regexp"))

    (require 'wgrep)
    (require 'wgrep-ag)
    (add-hook 'ag-mode-hook 'wgrep-ag-setup)))

(use-package allout
  :commands allout-mode
  :config
  (progn
    (defun my-allout-init ()
      (unbind-key "M-k" allout-mode-map))

    (add-hook 'allout-mode-hook 'my-allout-init)
    ;; redefine this to make it work with smartparens
    (defun allout-pre-command-business ()
      "Outline `pre-command-hook' function for outline buffers.

Among other things, implements special behavior when the cursor is on the
topic bullet character.

When the cursor is on the bullet character, self-insert
characters are reinterpreted as the corresponding
control-character in the `allout-mode-map-value'.  The
`allout-mode' `post-command-hook' insures that the cursor which
has moved as a result of such reinterpretation is positioned on
the bullet character of the destination topic.

The upshot is that you can get easy, single (ie, unmodified) key
outline maneuvering operations by positioning the cursor on the bullet
char.  When in this mode you can use regular cursor-positioning
command/keystrokes to relocate the cursor off of a bullet character to
return to regular interpretation of self-insert characters."

      (if (not (allout-mode-p))
          nil
        (setq allout-command-counter (1+ allout-command-counter))
        (setq allout-this-command-hid-text nil)
        ;; Do hot-spot navigation.
        (if (and (memq this-command '(sp--self-insert-command self-insert-command))
                 (eq (point)(allout-current-bullet-pos)))
            (allout-hotspot-key-handler))))))

(use-package appt
  :defer t
  :config
  (progn
    (defadvice appt-display-message (around add-notifyd-notification activate)
      (let ((strings (if (listp (ad-get-arg 0)) (ad-get-arg 0) (list (ad-get-arg 0))))
            (mins (if (listp (ad-get-arg 1)) (ad-get-arg 1) (list (ad-get-arg 1)))))
        (-zip-with
         (lambda (s m)
           (start-process "notify-send-appt" nil "notify-send"
                          "-u" "critical" "-c" "appt" "-t" "30000" (format "In %d minutes" m) s))
         strings mins))
      ad-do-it)))

(use-package atomic-chrome
  :straight t
  :custom
  (atomic-chrome-buffer-open-style 'frame)
  (atomic-chrome-url-major-mode-alist
   '(
     ("github" . markdown-mode)
     ("gitlab" . markdown-mode)
     ("mail.google.com" . org-mode)
     ("helpdesk.logio.cz" . textile-mode)
     ("saleschamp\\.atlassian\\.net" . jira-markup-mode)
     ))
  :config
  (atomic-chrome-start-server))

(use-package autobookmarks
  :config
  (autobookmarks-mode 1))

(use-package better-jump
  :bind (("C-\\" . bjump-word-jump)
         ("A-l" . bjump-char-jump-line)
         ("A-;" . bjump-word-jump-paragraph)
         ("s-o" . bjump-window-jump)
         ("A-j" . bjump-window-jump)
         ("A-k" . bjump-window-delete))
  :config
  (progn
    (bind-key "o" 'bjump-info-link-jump Info-mode-map)
    (bind-key "o" 'bjump-help-link-jump help-mode-map)))

(use-package browse-at-remote
  :straight t
  :bind (("C-c g g" . browse-at-remote)))

(use-package cask-mode
  :straight t
  :mode ("Cask\\'" . cask-mode))

(use-package caddyfile-mode
  :straight t
  :config
  (defun my-caddyfile-hook ()
    (setq-local tab-width 4)
    (setq-local indent-tabs-mode nil))
  (add-hook 'caddyfile-mode-hook #'my-caddyfile-hook))

(use-package c-mode
  :defer t
  :config
  (progn
    (bind-keys :map c-mode-map
      ("C-M-x" . compile))
    (defun my-c-mode-setup ()
      (c-set-style "java"))
    (add-hook 'c-mode-hook 'my-c-mode-setup)))

(use-package cc-mode
  :defer t
  :config
  (progn
    (bind-keys :map c++-mode-map
      ("C-M-x" . compile))
    (defun my-c++-mode-setup ()
      (c-set-style "stroustrup"))
    (add-hook 'c++-mode-hook 'my-c++-mode-setup)))

(use-package calc
  :bind ("<f5>" . calc-same-interface)
  :config
  (progn
    (defun my-calc-grab-region-dwim (beg end)
      "Parse the region as a vector of numbers and push it on the Calculator stack.

If the region does not contain spaces, parse it as one
entry (same as passing \\[universal-argument] to
`calc-grab-region').

If no region is active, use word udner point."
      (interactive (if (use-region-p)
                       (list (region-beginning) (region-end))
                     (-let (((beg . end) (bounds-of-thing-at-point 'word)))
                       (list beg end))))
      (if (string-match-p " " (buffer-substring beg end))
          (calc-grab-region beg end nil)
        (calc-grab-region beg end (list 4))))
    (bind-key "g" 'my-calc-grab-region-dwim calc-dispatch-map)

    (fset 'calc-one-minus [?1 return ?- ?n])
    (fset 'calc-vector-normalize [return ?A ?/])
    (bind-keys :map calc-mode-map
      ("C-. m" . calc-one-minus)
      ("C-. n" . calc-vector-normalize)
      ("C-<tab>" . calc-roll-up)
      ("<tab>" . calc-roll-down))
    (setq calc-display-trail nil)))

(use-package calendar
  :bind ("M-s c" . calendar)
  :init
  (use-package slovak-holidays
    :straight t
    :config (slovak-holidays-add))
  (use-package czech-holidays
    :straight t
    :config (czech-holidays-add))
  :config
  (progn
    (defvar my-calendar-current-window nil
      "Window from where the calendar was invoked.")

    (defadvice calendar (before set-my-calendar-current-window activate)
      (setq my-calendar-current-window (selected-window)))

    (defun my-calendar-insert-date ()
      "Insert the date under cursor from calendar to current buffer."
      (interactive)
      (let ((now (calendar-cursor-to-date)))
        (save-selected-window
          (select-window my-calendar-current-window)
          (let ((date (format "%4d-%02d-%02d" (nth 2 now) (nth 0 now) (nth 1 now))))
            (insert date)))
        (calendar-exit)))
    (bind-key "RET" 'my-calendar-insert-date calendar-mode-map)))

(use-package clippy
  :commands clippy-describe-function)

(use-package conf-mode
  :mode (("\\.pwm\\'" . conf-mode)))

(use-package company
  :straight t
  :defer t
  :config
  (add-to-list 'company-backends 'company-omnisharp)

  (bind-key "C-c y" 'company-yasnippet)

  (bind-key "C-f" 'company-complete-selection company-active-map)
  (bind-key "C-n" 'company-select-next company-filter-map)
  (bind-key "C-p" 'company-select-previous company-filter-map)

  (bind-key "C-n" 'company-select-next company-active-map)
  (bind-key "C-p" 'company-select-previous company-active-map))

(use-package company-lsp
  :straight t
  :after lsp-mode)

(use-package company-statistics
  :straight t
  :after company)

(use-package compile
  :bind (:map compilation-mode-map
         ("O" . my-compile-goto-error-other-window)
         ("o" . compile-goto-error))
  :config
  (progn
    (use-package ansi-color)

    (defvar my-get-compile-command nil
      "Command used to compute the `compile-command'.

It is run in the buffer from where `compile' is called.")

    (defun my-compile ()
      "Run `compile' in current buffer.

Call the value of `my-get-compile-command' to generate the
`compile-command' which is then executed."
      (interactive)
      (compile (funcall my-get-compile-command)))

    (defun my-compile-goto-error-other-window (&optional event)
      "Just like `compile-goto-error' but visit in new window."
      (interactive (list last-input-event))
      (compile-goto-error event))

    (defun my-compile-same-window (&rest _)
      (memq this-command (list 'compile-goto-error)))

    (add-to-list 'display-buffer-alist
                 '(my-compile-same-window
                   (display-buffer-reuse-window display-buffer-same-window)
                   (inhibit-same-window . nil)))

    (defun my-compilation-reparse-buffer (&rest _ignored)
      (interactive)
      (compilation--flush-parse (point-min) (point-max))
      (compilation--ensure-parse (point-max)))
    (add-hook 'compilation-finish-functions 'my-compilation-reparse-buffer)

    (defvar my-compile-auto-fold-alist
      `(
        (,(rx (and bol "PHPStan > " (0+ any) eol))
         ,(rx (or (and (group-n 1 bol eol) "\nPHPStan > " (0+ any) eol)
                  (and (group-n 1 bol) "\nTotal time")))
         0 0 1
         "lint")
        (,(rx (and bol "Running \"" (0+ any) "task" eol))
         ,(rx (or (and (group-n 1 eol) "\nRunning \"" (0+ any) "task" eol)
                  (and (group-n 1 bol) "\nDone.")))
         0 0 1
         "shell:tester")
        )
      "Control automatic folding in a `compilation-mode' buffer.

Each element is of the form:

 (ANCHOR-START ANCHOR-END [HEADER-MATCH-GROUP [START-MATCH-GROUP [END-MATCH-GROUP [IGNORE-ANCHOR]]]])

ANCHOR-START is a regular expression which marks the start of the
folded region.

ANCHOR-END is a regular expression which marks the end of the
folded region.

HEADER-MATCH-GROUP is a match group in ANCHOR-START which is used
as the header of the folded region.  If nil a default \"...\"
header is used.

START-MATCH-GROUP (default 0) can be used to specify which of
ANCHOR-START match-group's beginning is used as the beginning of
the folded region.

END-MATCH-GROUP (default 0) can be used to specify which of
ANCHOR-END match-group's end is used as the end of the folded
region.

IGNORE-ANCHOR is a regular expression that matches against the
header or a function which takes the header as an argument.  If
it matches or returns non-nil do not fold this header.")

    (defvar my-compile-auto-fold-header-match-data nil
      "Remember the match data of the last match header.")
    (make-variable-buffer-local 'my-compile-auto-fold-header-match-data)

    ;; TODO: use marker instead of point?
    (defvar my-compile-last-match 1
      "Track the position of last match.

We restart the search from this position instead of using
`compilation-filter-start' because that might have just happened
to be in the middle of a match and we would never find it using a
regexp.

Instead, every time we match a start or end anchor we update this
variable.")
    (make-variable-buffer-local 'my-compile-last-match)

    (defmacro my-with-match-data (match-data &rest body)
      "Use MATCH-DATA in BODY."
      (declare (indent 1))
      `(save-match-data
         (set-match-data ,match-data)
         ,@body))

    (defun my-compile--apply-auto-fold ()
      "Apply rules from `my-compile-auto-fold-alist'.

This function is used in `compilation-filter-hook' and can work
on chunked output.

To keep the state inbetween calls we use `my-compile-last-match'
and `my-compile-auto-fold-header-match-data'."
      (let ((end (point)))
        (-each my-compile-auto-fold-alist
          (-lambda ((anchor-start
                     anchor-end
                     header-match-group
                     start-match-group
                     end-match-group
                     ignore-anchor))
            (save-excursion
              (goto-char my-compile-last-match)
              (catch 'done
                (while (or my-compile-auto-fold-header-match-data
                           (when (re-search-forward anchor-start end t)
                             (setq my-compile-last-match (point))
                             (setq my-compile-auto-fold-header-match-data (match-data))))
                  (if (re-search-forward anchor-end end t)
                      (let ((fold-header
                             (my-with-match-data my-compile-auto-fold-header-match-data
                               (match-string (or header-match-group 0))))
                            (inhibit-read-only t))
                        (unless (and ignore-anchor
                                     (string-match-p ignore-anchor fold-header))
                          (fold-this
                           (my-with-match-data my-compile-auto-fold-header-match-data
                             (match-beginning (or start-match-group 0)))
                           (match-end (or end-match-group 0))
                           fold-header))
                        (goto-char (match-end (or end-match-group 0)))

                        (setq my-compile-last-match (point))
                        (setq my-compile-auto-fold-header-match-data nil))
                    ;; if we have a start or found one, but there is no end,
                    ;; quit the loop and wait for the next invocation
                    (throw 'done t)))))))))

    (defun my-compile-apply-auto-fold ()
      "Apply rules from `my-compile-auto-fold-alist'."
      (interactive)
      (fold-this-unfold-all)
      (my-compile-auto-fold-init)
      (save-excursion
        (goto-char (point-max))
        (my-compile--apply-auto-fold)))

    (defun my-compile-auto-fold-init (&rest _)
      "Reset state at the beginning of compilation."
      (setq-local my-compile-auto-fold-header-match-data nil)
      (setq-local my-compile-last-match 1))

    (add-hook 'compilation-start-hook 'my-compile-auto-fold-init)
    ;; THIS must be called after `my-colorize-compilation-buffer'
    ;; because it can remove "control" characters from output and mess
    ;; with match markers.
    (add-hook 'compilation-filter-hook 'my-compile--apply-auto-fold)

    (defun my-colorize-compilation-buffer ()
      (read-only-mode -1)
      (ansi-color-apply-on-region compilation-filter-start (point))
      (read-only-mode 1))
    (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer)))

(use-package crontab-mode
  :straight t
  :mode "crontab\\'")

(use-package csharp-mode
  :straight t
  :defer t
  :config
  (use-package omnisharp :straight t)
  (progn
    (c-add-style
     "my-C#"
     '("C#"
       (c-offsets-alist
        (arglist-intro         . +)
        (arglist-cont          . 0)
        (arglist-cont-nonempty . +)
        (arglist-close         . 0))))
    ;; TODO: use smart-jump for this
    (bind-key "M-'" 'omnisharp-helm-find-usages csharp-mode-map)
    (bind-key "C-M-'" 'pop-tag-mark csharp-mode-map)
    (bind-key "C-x C-d"
              (defhydra hydra-csharp-refactor (:color blue)
                "Refactor"
                ("v" omnisharp-rename "Rename variable")
                ("u" omnisharp-fix-usings "Fix usings")
                ("d" omnisharp-go-to-definition "Goto definition"))
              csharp-mode-map)


    (defun my-csharp-mode-init ()
      (omnisharp-mode 1)
      (company-mode 1)
      (flycheck-mode 1)
      (c-set-style "my-C#"))
    (add-hook 'csharp-mode-hook 'my-csharp-mode-init)))

(use-package css-mode
  :defer t
  :config
  (progn
    (defun my-css-mode-setup ()
      (rainbow-mode 1))
    (add-hook 'css-mode-hook 'my-css-mode-setup)))

(use-package "cua-base"
  :defer t
  :config
  (progn
    (define-key cua--region-keymap [remap my-emacs-lisp-open-line] 'cua-replace-region)))

(use-package custom
  :init
  (bind-keys :prefix "C-c c"
             :prefix-map ctl-c-c-map
             :prefix-docstring "Customize map")
  :bind (("C-c c v" . customize-variable)
         ("C-c c f" . customize-face)
         ("C-c c g" . customize-group))
  :config
  (progn
    (defun my-custom-jump-to-state ()
      (interactive)
      (goto-char (point-min))
      (re-search-forward "State :")
      (backward-char 7))

    (defun my-custom-jump-to-toggle ()
      (interactive)
      (goto-char (point-min))
      (re-search-forward "Toggle")
      (backward-char 6))

    (defun my-custom-jump-to-last-insert ()
      (interactive)
      (goto-char (point-max))
      (let ((case-fold-search nil)) (re-search-backward "INS")))

    (bind-keys :prefix "C-,"
               :prefix-map custom-custom-map
               :prefix-docstring "Custom custom map."
      ("s" . my-custom-jump-to-state)
      ("t" . my-custom-jump-to-toggle)
      ("i" . my-custom-jump-to-last-insert))))

(use-package diff-mode
  :defer t
  :config
  (progn
    (unbind-key "M-k" diff-mode-map)
    (defun my-diff-mode-init ()
      (highlight-thing-mode)
      (setq buffer-display-table (make-display-table))
      (aset buffer-display-table ?\^M []))
    (add-hook 'diff-mode-hook 'my-diff-mode-init)))

;; see commentary in dired-defs.el
(use-package dired
  :mode ("\\.wdired\\'" . my-virtual-dired-mode)
  :bind (("C-x C-j" . dired-jump))
  :init
  (progn
    (defvar my-virtual-dired-p nil
      "Non-nil if the buffer is virtual dired.")

    ;; TODO: if we have just one line take it to be the "current" dired
    ;; and do not assume it specifies default directory
    ;; TODO: actually, just remove the "top" directory option completely
    (defun my-virtual-dired-mode (&rest _ignore)
      (let ((magit-auto-revert-mode-old
             (bound-and-true-p magit-auto-revert-mode)))
        (unwind-protect
            (save-excursion
              (magit-auto-revert-mode -1)
              (goto-char (point-min))
              (back-to-indentation)
              (let* ((ddir (thing-at-point 'filename))
                     (dired-buffers dired-buffers))
                (set-buffer-modified-p nil)
                (virtual-dired nil)
                (set-buffer-modified-p nil)
                (setq write-contents-functions 'dired-virtual-save-buffer)
                (set (make-local-variable 'my-virtual-dired-p) t))
              (goto-line 2)
              (let ((buffer-name (s-trim (thing-at-point 'line))))
                (dired-virtual-revert)
                (rename-buffer buffer-name)))
          (when (boundp magit-auto-revert-mode)
            (magit-auto-revert-mode magit-auto-revert-mode-old)))))

    (defadvice org-edit-src-code (after run-wdired activate)
      (when (eq major-mode 'dired-mode)
        (my-virtual-dired-mode))))
  :config
  (progn
    (my-load-or-tangle (f-join (f-parent my-vendor-file) "dired-defs"))
    (load-relative "dired-defs")

    ;; overload to fix bullshit
    (defun dired-hack-local-variables () nil)))

(use-package dired-list
  :commands (dired-list-find-file dired-list-grep))

(use-package dired-tagsistant
  :init
  (bind-keys :prefix-map ctl-x-t-map
             :prefix "C-x T"
             :prefix-docstring "C-x T prefix map")
  :bind (("C-x T r" . dired-tagsistant-add-relation)
         ("C-x T +" . dired-tagsistant-some-tags)
         ("C-x T *" . dired-tagsistant-all-tags)
         ("C-x T % +" . dired-tagsistant-some-tags-regexp)
         ("C-x T % *" . dired-tagsistant-all-tags-regexp)))

(use-package docker
  :straight t
  :defer t)

(use-package docker-tramp
  :straight t
  :defer t)

(use-package dockerfile-mode
  :straight t
  :defer t)

(use-package dotenv-mode
  :straight t
  :defer t)

(use-package easy-kill
  :straight t
  :bind ([remap kill-ring-save] . easy-kill))

(use-package ediff
  :bind ("C-. =" . hydra-ediff/body)
  :commands (ediff-buffers
             ediff-buffers3
             ediff-files
             ediff-files
             ediff-files3
             ediff-revision
             ediff-patch-file
             ediff-patch-buffer
             ediff-regions-linewise
             ediff-regions-wordwise)
  :config
  (progn
    ;; TODO: add magit shortcuts as well
    (defhydra hydra-ediff (:color blue :hint nil)
      "
^Buffers           Files           VC                     Ediff regions
----------------------------------------------------------------------
_b_uffers           _f_iles (_=_)       _r_evisions              _l_inewise
_B_uffers (3-way)   _F_iles (3-way)                          _w_ordwise
                  _c_urrent file
"
      ("b" ediff-buffers)
      ("B" ediff-buffers3)
      ("=" ediff-files)
      ("f" ediff-files)
      ("F" ediff-files3)
      ("c" ediff-current-file)
      ("r" ediff-revision)
      ("l" ediff-regions-linewise)
      ("w" ediff-regions-wordwise))
    (defvar my-ediff-before-config nil "Window configuration before ediff.")

    (defun my-ediff-before-setup ()
      "Function to be called before any buffers or window setup for `ediff'."
      (unless (eq this-command 'exit-recursive-edit)
        (setq my-ediff-before-config (current-window-configuration))))

    (defadvice ediff-regions-wordwise (before save-window-config activate)
      "Save window configuration before `ediff-regions-wordwise' is run.
Doing this in the `ediff-before-setup-hook' is too late because
by then the region selection happened which changed the window
config from before."
      (setq my-ediff-before-config (current-window-configuration)))

    (defadvice ediff-regions-linewise (before save-window-config activate)
      "Save window configuration before `ediff-regions-linewise' is run.
Doing this in the `ediff-before-setup-hook' is too late because
by then the region selection happened which changed the window
config from before."
      (setq my-ediff-before-config (current-window-configuration)))

    (defun my-ediff-quit ()
      "Function to be called when `ediff' quits."
      (when my-ediff-before-config
        (set-window-configuration my-ediff-before-config))
      ;; Clean up ediff control buffers
      (->> (buffer-list)
           (-map 'buffer-name)
           (--select (string-match-p "\\*[Ee]diff" it))
           (-map 'kill-buffer)))

    (add-hook 'ediff-before-setup-hook 'my-ediff-before-setup)
    (add-hook 'ediff-quit-hook 'my-ediff-quit)))

(use-package edit-indirect
  :straight t
  :bind (("C-c '" . edit-indirect-region))
  :init
  (defun my-after-indirect-edit-realign (beg end)
    (save-excursion
      (goto-char beg)
      (let ((cc (current-column))
            (end-marker (set-marker (make-marker) end)))
        (while (< (progn
                    (forward-line)
                    (point)) end-marker)
          (line-beginning-position)
          (insert (make-string cc 32)))
        (when indent-tabs-mode
          (tabify beg end-marker))
        (set-marker end-marker nil))))

  (add-hook 'edit-indirect-after-commit-functions 'my-after-indirect-edit-realign)
  :config
  (bind-key "C-x C-s" 'edit-indirect-commit edit-indirect-mode-map))

(use-package editorconfig
  :straight t
  :config (editorconfig-mode 1))

(use-package eldoc
  :commands eldoc-mode
  :diminish eldoc-mode
  :defines eldoc-eval-preferred-function
  :init
  (setq eldoc-eval-preferred-function 'pp-eval-expression))

(use-package eldoc-eval
  :straight t
  :after eldoc
  :config
  (eldoc-in-minibuffer-mode 1))

(use-package elfeed
  :straight t
  :if (member (my-where-am-i) '("home" "brno"))
  :bind (("C-. C-f" . elfeed))
  :custom
  (elfeed-db-directory "~/.emacs.d/elfeed")
  (elfeed-max-connections 5)
  (elfeed-search-face-alist
   (quote
    ((unread elfeed-search-unread-title-face)
     (tumblr font-lock-constant-face))))
  (elfeed-search-title-max-width 90)
  (elfeed-use-curl t)
  :init
  (progn
    ;; run an idle timer from this timer so it won't bother me while editing
    ;; schedule timer, make it run idle timer, make that run this timer again... blerg
    (defvar my-elfeed-update-timer
      (run-with-timer 10 nil 'my-elfeed-update-schedule)
      "`Elfeed' update timer.  This timer periodically starts an
idle timer to do the actual update.")

    (defvar my-elfeed-idle-timer nil
      "Timer performing `elfeed' update.")

    (defun my-elfeed-update-schedule ()
      "Schedule an update."
      (setq my-elfeed-idle-timer
            (run-with-idle-timer 10 nil 'my-elfeed-update)))

    (defun my-elfeed-update ()
      "Run `elfeed' update and schedule a new one in the future."
      (elfeed-update)
      (setq my-elfeed-update-timer
            (run-with-timer 1200 nil 'my-elfeed-update-schedule))))
  :config
  (progn

    (defun my-elfeed-search-print-entry--default (entry)
      "Print ENTRY to the buffer."
      (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
             (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
             (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
             (feed (elfeed-entry-feed entry))
             (author (elfeed-meta entry :author))
             (feed-title
              (when feed
                (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
             (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
             (tags-str (mapconcat
                        (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
                        tags ","))
             (title-width (- (window-width) 10 elfeed-search-trailing-width))
             (title-column (elfeed-format-column
                            title (elfeed-clamp
                                   elfeed-search-title-min-width
                                   title-width
                                   elfeed-search-title-max-width)
                            :left)))
        (insert (propertize date 'face 'elfeed-search-date-face) " ")
        (insert (propertize title-column 'face title-faces 'kbd-help title) " ")
        (when feed-title
          (insert (propertize feed-title 'face 'elfeed-search-feed-face) " "))
        (when tags
          (insert "(" tags-str ")"))
        (when author
          (insert " " author))))

    (setq elfeed-search-print-entry-function
          #'my-elfeed-search-print-entry--default)

    (defvar my-elfeed-unread-count (let ((n 0))
                                     (with-elfeed-db-visit (entry _feed)
                                       (when (memq 'unread (elfeed-entry-tags entry))
                                         (cl-incf n)))
                                     n)
      "Number of unread elfeed feeds.")

    (defun my-elfeed-update-unread-count ()
      (let ((n 0))
        (with-elfeed-db-visit (entry _feed)
          (when (memq 'unread (elfeed-entry-tags entry))
            (cl-incf n)))
        (setq my-elfeed-unread-count n)))
    (add-hook 'elfeed-db-update-hook 'my-elfeed-update-unread-count)

    (defadvice elfeed-tag-1 (before update-unread activate)
      (when (and (not (member 'unread (elfeed-entry-tags (ad-get-arg 0))))
                 (member 'unread (ad-get-args 1)))
        (cl-incf my-elfeed-unread-count)))

    (defadvice elfeed-untag-1 (before update-unread activate)
      (when (and (member 'unread (elfeed-entry-tags (ad-get-arg 0)))
                 (member 'unread (ad-get-args 1)))
        (cl-decf my-elfeed-unread-count)))

    (bind-keys :map elfeed-show-mode-map
      ("M-n" . shr-next-link)
      ("M-p" . shr-previous-link))))

(use-package elpy
  :straight t
  :defer t)

(use-package elxiki
  :defer t
  :commands elxiki-mode
  :config
  (progn
    (bind-keys :map elxiki-mode-map
      ("A-<return>" . elxiki-command)
      ("M-<return>" . elxiki-command-no-filter))))

(use-package elwm
  :bind (("M-o" . elwm-activate-window)
         ("M-O" . elwm-transpose-window)
         ("C-M-o" . elwm-rotate-window)
         ("C-x C-2" . elwm-split-window)
         ("A-P" . elwm-shift-up)
         ("A-:" . elwm-shift-down)
         ("A-L" . elwm-shift-left)
         ("A-\"" . elwm-shift-right)
         ("A-M-p" . elwm-swap-up)
         ("A-M-;" . elwm-swap-down)
         ("A-M-l" . elwm-swap-left)
         ("A-M-'" . elwm-swap-right))
  :init
  (progn
    (add-hook 'dired-mode-hook (lambda () (bind-key "M-o" 'elwm-activate-window dired-mode-map)))
    (add-hook 'diff-mode-hook (lambda () (bind-key "M-o" 'elwm-activate-window diff-mode-map)))))

(use-package emr
  :bind (("M-r" . emr-show-refactor-menu)))

(use-package eshell
  :commands eshell
  :config
  (progn
    ;; https://emacs.stackexchange.com/questions/2107/run-application-in-cwd-on-remote-host-from-within-eshell
    (defadvice eshell-gather-process-output (before absolute-cmd (command args) act)
      (setq command (file-truename command)))

    (defun my-eshell-skip-prompt ()
      (save-match-data
        (let ((eol (line-end-position)))
          (when (and (thing-at-point-looking-at eshell-prompt-regexp)
                     (<= (match-end 0) eol))
            (goto-char (match-end 0))))))

    (defun my-eshell-init ()
      (setq eshell-skip-prompt-function 'my-eshell-skip-prompt)
      (bind-key [remap eshell-send-input] 'my-eshell-send-input eshell-mode-map))
    (add-hook 'eshell-mode-hook 'my-eshell-init)

    (require 'eshell-defs)))

(use-package esh-autosuggest
  :straight t
  :hook (eshell-mode . esh-autosuggest-mode)
  :config
  (el-patch-defun esh-autosuggest--prefix ()
    "Get current eshell input."
    (let* ((input-start (progn
                          (save-excursion
                            (beginning-of-line)
                            (while (not (looking-at-p eshell-prompt-regexp))
                              (forward-line -1))
                            (el-patch-add (re-search-forward eshell-prompt-regexp nil t))
                            (eshell-bol))))
           (prefix
            (string-trim-left
             (buffer-substring-no-properties
              input-start
              (line-end-position)))))
      (if (not (string-empty-p prefix))
          prefix
        'stop))))

(use-package ess
  :straight t
  :defer t
  :config
  (progn
    (use-package ess-r-mode
      :config
      (bind-key "M-'" 'smart-jump-go ess-r-mode-map)
      (bind-key "C-M-'" 'smart-jump-back ess-r-mode-map))

    (put 'flycheck-lintr-linters 'safe-local-variable #'stringp)

    (use-package ess-help)

    (bind-keys
     :map ess-mode-map
     ("_" . self-insert-command)
     :map inferior-ess-mode-map
     ("_" . self-insert-command)
     :map ess-help-mode-map
     ("q" . quit-window))

    (defun my-ess-mode-hook ()
      (font-lock-add-keywords
       nil
       '(
         ("\\b\\(this\\)\\b" 1 font-lock-constant-face)
         ("\\b\\(describe\\)(" 1 font-lock-keyword-face)
         ("\\b\\(it\\)(" 1 font-lock-keyword-face)
         ))
      (smartparens-strict-mode 1))
    (add-hook 'ess-mode-hook 'my-ess-mode-hook)
    (defun my-ess-post-run-hook ()
      (smartparens-strict-mode 1)
      (modify-syntax-entry ?$ ".")
      (ess-execute-screen-options))
    (add-hook 'ess-post-run-hook 'my-ess-post-run-hook)
    (defun my-inferior-ess-init ()
      (smartparens-mode 1))
    (add-hook 'inferior-ess-mode-hook 'my-inferior-ess-init)))

(use-package exec-path-from-shell
  :straight t
  :commands exec-path-from-shell)

(use-package expand-region
  :straight t
  :bind ("s-'" . er/expand-region))

(use-package eyebrowse
  :commands (
             eyebrowse-mode
             eyebrowse-switch-to-window-config-0
             eyebrowse-switch-to-window-config-1
             eyebrowse-switch-to-window-config-2
             eyebrowse-switch-to-window-config-3
             eyebrowse-switch-to-window-config-4
             eyebrowse-switch-to-window-config-5
             eyebrowse-switch-to-window-config-6
             eyebrowse-switch-to-window-config-7
             eyebrowse-switch-to-window-config-8
             eyebrowse-switch-to-window-config-9)
  :config
  (progn

    (defun my-eyebrowse-pop-to-ansi-term ()
      "Switch to slot 1 (ansi-term) or back to last slot."
      (interactive)
      (let ((current-slot (eyebrowse--get 'current-slot)))
        (if (/= 1 current-slot)
            (eyebrowse-switch-to-window-config-1)
          (eyebrowse-last-window-config))))

    (bind-key "C-<f11>" 'my-eyebrowse-pop-to-ansi-term eyebrowse-mode-map)
    (bind-key "C-c `" 'eyebrowse-last-window-config eyebrowse-mode-map)
    (bind-key "C-c 0" 'eyebrowse-switch-to-window-config-0 eyebrowse-mode-map)
    (bind-key "C-c 1" 'eyebrowse-switch-to-window-config-1 eyebrowse-mode-map)
    (bind-key "C-c 2" 'eyebrowse-switch-to-window-config-2 eyebrowse-mode-map)
    (bind-key "C-c 3" 'eyebrowse-switch-to-window-config-3 eyebrowse-mode-map)
    (bind-key "C-c 4" 'eyebrowse-switch-to-window-config-4 eyebrowse-mode-map)
    (bind-key "C-c 5" 'eyebrowse-switch-to-window-config-5 eyebrowse-mode-map)
    (bind-key "C-c 6" 'eyebrowse-switch-to-window-config-6 eyebrowse-mode-map)
    (bind-key "C-c 7" 'eyebrowse-switch-to-window-config-7 eyebrowse-mode-map)
    (bind-key "C-c 8" 'eyebrowse-switch-to-window-config-8 eyebrowse-mode-map)
    (bind-key "C-c 9" 'eyebrowse-switch-to-window-config-9 eyebrowse-mode-map)))

(use-package face-remap
  :bind (("M-V" . variable-pitch-mode)))

(use-package feature-mode
  :straight t
  :defer t)

(use-package fish-completion
  :straight t
  :if (executable-find "fish")
  :after eshell
  :config (global-fish-completion-mode))

(use-package fish-mode
  :straight t
  :defer t)

(use-package gitignore-mode
  :straight t
  :mode ("\\.dockerignore" . 'gitignore-mode))

(use-package gnuplot
  :straight t
  :defer t)

(use-package grep
  :commands rgrep
  :config
  (progn
    (use-package scf-mode)
    (bind-key "s" 'scf-mode grep-mode-map)
    (defun my-grep-mode-init ()
      (scf-mode 1))
    (add-hook 'grep-mode-hook 'my-grep-mode-init)))

(defvar-local flycheck-error-indicators nil)

(use-package find-test
  :straight (:repo "git@github.com:Fuco1/find-test.git")
  :config
  (bind-key "C-c C-t" 'ft-find-test-or-source prog-mode-map)
  (use-package ess-r-mode
    :defer t
    :config (bind-key "C-c C-t" 'ft-find-test-or-source ess-r-mode-map))
  (use-package neon-mode
    :defer t
    :config (bind-key "C-c C-t" 'ft-find-test-or-source neon-mode-map))
  (use-package yaml-mode
    :defer t
    :config (bind-key "C-c C-t" 'ft-find-test-or-source yaml-mode-map))
  (use-package js2-mode
    :defer t
    :config (bind-key "C-c C-t" 'ft-find-test-or-source js2-mode-map)))

(use-package flycheck
  :straight t
  :commands flycheck-mode
  :config
  (progn
    (flycheck-def-option-var flycheck-phpstan-config nil php-phpstan
      "Path to the phpstan configuration for current project.

This is passed to the -l option in phpstan.  It is a good idea is
to use a directory-local variable to specify this per-project."
      :type 'file
      :safe (lambda (x)
              (stringp x)
              (file-exists-p x)))

    (flycheck-def-option-var flycheck-phpstan-config-filename "phpstan.neon" php-phpstan
      "Name of the phpstan configuration file."
      :type 'string
      :safe (lambda (x) (stringp x)))

    (flycheck-def-option-var flycheck-phpstan-autoload nil php-phpstan
      "Path to the phpstan autoload for current project.

This is passed to the -a option in phpstan.  It is a good idea is
to use a directory-local variable to specify this per-project."
      :type 'file
      :safe (lambda (x)
              (stringp x)
              (file-exists-p x)))

    (flycheck-def-option-var flycheck-phpstan-level "3" php-phpstan
      "Strictness level phpstan uses to check the sources.

This is passed to the -c option in phpstan.  A good idea is to
use a directory-local variable to specify this per-project."
      :type 'string
      :safe (lambda (x)
              (and (stringp x)
                   (string-match-p "\\`[0-9]+\\'" x))))

    (defun my-php-phpstan-find-config (filename checker)
      (when (eq checker 'php-phpstan)
        (-first 'file-exists-p
                (--map (format "%s/%s/%s"
                               (my-php-find-project-root)
                               it
                               filename)
                       (list
                        ""
                        "config"
                        "app/config"
                        "tests"
                        "tests/phpstan"
                        "tests/config")))))

    (add-hook 'flycheck-locate-config-file-functions
              'my-php-phpstan-find-config)

    (flycheck-define-checker php-phpstan
      "Checker for PHPStan"
      :command ("phpstan"
                "analyse"
                "--no-progress"
                "--error-format" "raw"
                (option "-l" flycheck-phpstan-level)
                (config-file "-c" flycheck-phpstan-config-filename)
                source-original)
      :error-patterns
      ((error line-start (file-name) ":" line ":" (message)))
      :working-directory my-php-find-project-root
      :modes (php-mode php+-mode))

    (add-to-list 'flycheck-checkers 'php-phpstan 'append)

    (flycheck-add-next-checker 'php '(warning . php-phpstan) 'append)
    (flycheck-add-next-checker 'php-phpmd 'php-phpstan 'append)
    (flycheck-add-next-checker 'php-phpcs 'php-phpstan 'append)

    (setq flycheck-puppet-parser-executable "bundler-puppet")
    (setq flycheck-puppet-lint-executable "bundler-puppet-lint")

    (use-package indicators)

    (defun flycheck-errors-to-indicator-list ()
      (let* ((lines (-uniq (--map (flycheck-error-line it) flycheck-current-errors))))
        (unless (> (length lines) 50)
          (--map (ind-create-indicator-at-line it) lines))))

    (defun flycheck-add-indicators ()
      (setq-local flycheck-error-indicators (flycheck-errors-to-indicator-list))
      (ind-update-event-handler))
    (add-hook 'flycheck-after-syntax-check-hook 'flycheck-add-indicators)

    (defun my-flycheck-init ()
      (indicators-mode t)
      (ind-create-indicator 'point :managed t :face font-lock-builtin-face)
      (add-to-list 'ind-managed-list-relative 'flycheck-error-indicators)
      (flycheck-haskell-setup))
    (add-hook 'flycheck-mode-hook 'my-flycheck-init)))

(use-package flycheck-cask
  :straight t
  :after flycheck
  :init (add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

(use-package flycheck-haskell
  :straight t
  :after flycheck
  :init (add-hook 'haskell-mode-hook #'flycheck-haskell-setup))

(use-package flycheck-ledger
  :straight t
  :after flycheck)

(use-package fold-this
  :straight t
  :bind (("C-c C-v f" . fold-this)))

(use-package fold-dwim
  :straight t
  :defer t)

(use-package free-keys
  :commands free-keys)

(use-package haskell-mode
  :straight t
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.chs\\'" . haskell-mode))
  :config
  (progn
    (require 'haskell-indentation)
    (bind-key "C-c h" 'haskell-hoogle haskell-mode-map)
    (bind-key "C-c C-r" 'my-haskell-reload haskell-mode-map)
    (bind-key "<backspace>" 'sp-backward-delete-char haskell-indentation-mode-map)

    (defun my-haskell-reload (&optional reload)
      (interactive)
      (inferior-haskell-load-file reload)
      (other-window 1))

    (defun my-hs-end-of-defun ()
      (forward-char)
      (re-search-forward "^[[:alpha:]]")
      (backward-char))

    (defun my-hs-beg-of-defun ()
      (re-search-backward "^[[:alpha:]]"))

    (defun my-haskell-init ()
      (setq-local completion-at-point-functions nil)
      ;; If we are in a stack project, change the ghci command
      (when (and (buffer-file-name)
                 (locate-dominating-file (buffer-file-name) "stack.yaml"))
        (-let (((prog args) (s-split-up-to " " haskell-program-name 1)))
          (setq-local haskell-program-name
                      (format "stack exec %s -- %s" prog args))))
      (set (make-local-variable 'end-of-defun-function) 'my-hs-end-of-defun)
      (set (make-local-variable 'beginning-of-defun-function) 'my-hs-beg-of-defun))
    (add-hook 'haskell-mode-hook 'my-haskell-init)

    (defun my-inferior-haskell-init ()
      (smartparens-mode 1))
    (add-hook 'inferior-haskell-mode-hook 'my-inferior-haskell-init)))

(use-package hcl-mode
  :straight t
  :mode ("\\.tf\\'")
  :custom ((hcl-indent-level 4))
  :config

  (flycheck-define-checker tflint
    "Checker for tflint"
    :command ("tflint")
    :error-patterns
    ((error line-start "Error: Failed to load configurations: " (file-name) ":" line "," column "-" column ": " (message)))
    :predicate (lambda ()
                 (equal "tf" (file-name-extension (buffer-file-name))))
    :modes (hcl-mode))

  (add-to-list 'flycheck-checkers 'tflint))

(use-package helm
  :straight
  (helm :repo "git@github.com:emacs-helm/helm.git"
        :fork (:repo "git@github.com:Fuco1/helm.git"))
  :defer t
  :config
  (progn
    (require 'helm-config)
    (autoload 'helm-org-in-buffer-search "helm-org" nil t)
    (bind-keys
     ("<f8>" . helm-occur)
     ("C-x C-b" . helm-buffers-list))
    (bind-keys :map ctl-dot-prefix-map
      ("k" . helm-show-kill-ring))
    (bind-keys :map helm-command-map
      ("o a" . helm-org-agenda-files-headings)
      ("o h" . helm-org-in-buffer-headings))))

(use-package helm-gtags
  :straight t
  :defer t)

(use-package help-mode
  :defer t
  :config
  (progn
    (defun my-help-mode-init ()
      (use-package better-jump)
      (helm-descbinds-mode 1))
    (add-hook 'help-mode-hook 'my-help-mode-init)
    (bind-key "<tab>" 'forward-button help-mode-map)
    (bind-key "<mouse-8>" 'help-go-back help-mode-map)
    (bind-key "<mouse-9>" 'help-go-forward help-mode-map)
    (bind-key "l" 'help-go-back help-mode-map)
    (bind-key "L" 'help-go-forward help-mode-map)))

(use-package helm-descbinds
  :straight t
  :after helm-mode)

(use-package highlight-thing
  :straight t
  :commands highlight-thing-mode)

(use-package htmlize
  :straight t
  :defer t)

(use-package ibuffer
  :commands ibuffer
  :init
  (progn
    ;; startup function
    (defun customize-ibuffer-mode ()
      "Startup function."
      (ibuffer-switch-to-saved-filter-groups "default")
      (-map (-partial 'add-to-list 'ibuffer-hidden-filter-groups)
            '("Tramp" "emacs-elpa" "Org Agenda" "Search" "Dired"))
      (visual-line-mode -1)
      (toggle-truncate-lines 1))
    (add-hook 'ibuffer-mode-hook 'customize-ibuffer-mode))
  :config
  (progn
    (require 'ibuffer-defs)))

(use-package ido
  :commands (
             read-directory-name
             read-file-name
             completing-read
             )
  ;; :bind (("M-." . ido-goto-symbol)) ;; was Find tag
  :init
  (progn
    (setq ido-everywhere t)
    (put 'ido-everywhere 'file (cons read-file-name-function nil))
    (setq read-file-name-function 'ido-read-file-name)
    (put 'ido-everywhere 'buffer (cons read-buffer-function nil))
    (setq read-buffer-function 'ido-read-buffer)

    (setq ido-decorations
          '("
-> " "" "
   " "
   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))
    (setq ido-default-buffer-method 'selected-window)
    (setq ido-enable-flex-matching t)
    (setq ido-enable-last-directory-history nil)
    (setq ido-ignore-buffers '("\\` "))
    (setq ido-max-directory-size 100000)
    (setq ido-mode 'both)
    (setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
    (setq ido-show-dot-for-dired t)
    (setq ido-use-faces t)
    (setq ido-use-virtual-buffers t))
  :config
  (progn
    (load-relative "ido-defs")))

(use-package inf-mongo
  :straight t
  :commands inf-mongo
  :config
  (progn
    (defun my-inf-mongo-init ()
      (smartparens-mode 1))
    (add-hook 'inf-mongo-mode-hook 'my-inf-mongo-init)))

(use-package info
  :defer t
  :config
  (defun my-info-mode-init ()
    (variable-pitch-mode 1)
    (use-package better-jump))
  (add-hook 'Info-mode-hook 'my-info-mode-init))

(use-package "isearch"
  :bind (("C-s" . isearch-forward-regexp)
         ("C-r" . isearch-backward-regexp))
  :config
  (progn
    (load-relative "isearch-defs")))

(use-package ispell
  :bind (("<f10>" . ispell-word)
         ("C-<f10>" . flyspell-mode))
  :init
  (progn
    ;; TODO: add a hydra to pick dictionary
    ;; slovniky sa mozu stiahnut aj ako balicky myspell-*
    (setq ispell-local-dictionary-alist
          '((nil
             "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-p" "/home/matus/.hunspell/personal.en") nil iso-8859-1)
            ("deutsch"
             "[[:alpha:]ÄÖÜéäöüß]"
             "[^[:alpha:]ÄÖÜéäöüß]"
             "[']"
             t ("-d" "de_DE" "-p" "/home/matus/.hunspell/personal.de") nil utf-8)
            ("francais"
             "[[:alpha:]ÀÂÇÈÉÊËÎÏÔÙÛÜàâçèéêëîïôùûü]"
             "[^[:alpha:]ÀÂÇÈÉÊËÎÏÔÙÛÜàâçèéêëîïôùûü]"
             "[-']"
             t ("-d" "fr_FR" "-p" "/home/matus/.hunspell/personal.fr") nil utf-8)
            ("italiano"
             "[[:alpha:]ÀÂÇÈÉÊËÎÏÔÙÛÜàâçèéêëîïôùûü]"
             "[^[:alpha:]ÀÂÇÈÉÊËÎÏÔÙÛÜàâçèéêëîïôùûü]"
             "[']"
             t ("-d" "it_IT" "-p" "/home/matus/.hunspell/personal.it") nil utf-8)
            ("latin"
             "[[:alpha:]]"
             "[^[:alpha:]]"
             ""
             t ("-d" "la_LA" "-p" "/home/matus/.hunspell/personal.la") nil utf-8)
            ("russian"
             "[[:alpha:]]"
             "[^[:alpha:]]"
             ""
             nil ("-d" "ru_RU" "-p" "/home/matus/.hunspell/personal.ru") nil utf-8)
            ("slovak"
             "[[:alpha:]áäčďéíĺľňóôŕšťúýžÁÄČĎÉÍĹĽŇÓÔŔŠŤÚÝŽ]"
             "[^[:alpha:]áäčďéíĺľňóôŕšťúýžÁÄČĎÉÍĹĽŇÓÔŔŠŤÚÝŽ]"
             ""
             t ("-d" "sk_SK" "-p" "/home/matus/.hunspell/personal.sk") nil utf-8)
            ("czech"
             "[[:alpha:]áčďéěíňóřšťúůýžÁČĎÉĚÍŇÓŘŠŤÚŮÝŽ]"
             "[^[:alpha:]áčďéěíňóřšťúůýžÁČĎÉĚÍŇÓŘŠŤÚŮÝŽ]"
             ""
             t ("-d" "cs_CZ" "-p" "/home/matus/.hunspell/personal.cs") nil utf-8)))))

(use-package jira-markup-mode
  :straight
  (jira-markup-mode :repo "git@github.com:mnuessler/jira-markup-mode.git"
                    :fork (:repo "git@github.com:Fuco1/jira-markup-mode.git")))

(use-package js2-mode
  :straight t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.js\\.snap\\'" . js2-mode))
  :config
  (progn
    (use-package js2-refactor :straight t)
    (use-package rjsx-mode :straight t)
    (use-package mocha :straight t)
    (bind-key "C-c C-m"
              (defhydra js2-refactor-hydra (:color blue :hint nil)
                "
 ^Functions^                    ^Variables^               ^Buffer^                      ^sexp^               ^Debugging^
 ------------------------------------------------------------------------------------------------------------------------------
 [_lp_] Localize Parameter      [_ev_] Extract variable   [_wi_] Wrap buffer in IIFE    [_k_]  js2 kill      [_lt_] log this
 [_ef_] Extract function        [_iv_] Inline variable    [_ig_] Inject global in IIFE  [_ss_] split string  [_dt_] debug this
 [_ip_] Introduce parameter     [_rv_] Rename variable    [_ee_] Expand node at point   [_sl_] forward slurp [_st_] JSON.stringify this
 [_em_] Extract method          [_vt_] Var to this        [_cc_] Contract node at point [_ba_] forward barf
 [_ao_] Arguments to object     [_sv_] Split var decl.    [_uw_] unwrap
 [_tf_] Toggle fun exp and decl [_ag_] Add var to globals
 [_ta_] Toggle fun expr and =>  [_ti_] Ternary to if
 [_q_]  quit"
                ("ee" js2r-expand-node-at-point)
                ("cc" js2r-contract-node-at-point)
                ("ef" js2r-extract-function)
                ("em" js2r-extract-method)
                ("tf" js2r-toggle-function-expression-and-declaration)
                ("ta" js2r-toggle-arrow-function-and-expression)
                ("ip" js2r-introduce-parameter)
                ("lp" js2r-localize-parameter)
                ("wi" js2r-wrap-buffer-in-iife)
                ("ig" js2r-inject-global-in-iife)
                ("ag" js2r-add-to-globals-annotation)
                ("ev" js2r-extract-var)
                ("iv" js2r-inline-var)
                ("rv" js2r-rename-var)
                ("vt" js2r-var-to-this)
                ("ao" js2r-arguments-to-object)
                ("ti" js2r-ternary-to-if)
                ("sv" js2r-split-var-declaration)
                ("ss" js2r-split-string)
                ("uw" js2r-unwrap)
                ("lt" js2r-log-this)
                ("st" js2r-json-stringify-this)
                ("dt" js2r-debug-this)
                ("sl" js2r-forward-slurp)
                ("ba" js2r-forward-barf)
                ("k" js2r-kill)
                ("q" nil))
              js2-mode-map)
    (use-package compile-eslint)

    (defun js2r-json-stringify-this ()
      "Wrap the expression at point with JSON.stringify."
      (interactive)
      (let* ((expr (js2-node-at-point))
             (beg (js2-node-abs-pos expr))
             (end (+ beg (js2-node-len expr)))
             (expr (delete-and-extract-region beg end)))
        (insert (format "JSON.stringify(%s, null, 4)" expr))))

    (defun my-nodejs-buffer-to-string (beg end)
      (interactive "r")
      (shell-command
       (format "node --eval 'console.log(Buffer.from(%s).toString())'"
               (buffer-substring-no-properties beg end))))

    (bind-key "C-c C-c" 'mocha-test-file js2-mode-map)
    (bind-key "M-'" 'dumb-jump-go js2-mode-map)
    (bind-key "C-M-'" 'dumb-jump-back js2-mode-map)
    (bind-key "M-." 'sallet-imenu js2-mode-map)
    (bind-key "M-j" 'my-join-lines js2-mode-map)

    ;; TODO: move to smart-jump
    (defun my-flow-minor-jump-to-definition-then-dumb-jump ()
      "Try to call `flow-minor-jump-to-definition' and if it fails use `dumb-jump-go'."
      (interactive)
      (when (equal (flow-minor-jump-to-definition) "Not found")
        (dumb-jump-go)))

    (use-package flow-js2-mode
      :straight (:repo "git@github.com:Fuco1/flow-js2-mode.git"))

    (defun my-eslint-fix ()
      "Fix the current buffer with eslint."
      (when (and flycheck-javascript-eslint-executable
                 (buffer-file-name)
                 (--any? (eq (flycheck-error-checker it) 'javascript-eslint) flycheck-current-errors))
        (message "Fixing buffer: eslint --fix %s" (buffer-file-name))
        (call-process
         flycheck-javascript-eslint-executable
         nil "*eslint errors*" nil "--fix" (buffer-file-name))
        (revert-buffer t t t)))

    (defun my-js2-mode-init ()
      (nvm-use-for-buffer)
      (add-hook 'after-save-hook 'my-eslint-fix nil 'local)
      (-when-let (root (locate-dominating-file default-directory "node_modules"))
        (setq-local flycheck-javascript-eslint-executable (concat root "/node_modules/.bin/eslint"))
        (setq-local flycheck-javascript-flow-executable (concat root "/node_modules/.bin/flow"))
        (setq-local flycheck-javascript-flow-coverage-executable (concat root "/node_modules/.bin/flow")))
      (when (and buffer-file-name
                 (string-match-p "\\.spec\\.js\\'" buffer-file-name))
        (mocha-toggle-imenu-function))
      (js2-refactor-mode 1)
      (when (fboundp 'flow-js2-mode)
        (flow-js2-mode 1))
      (when (flow-minor-configured-p)
        (flow-minor-mode 1)))
    (add-hook 'js2-mode-hook 'my-js2-mode-init)))

(use-package flycheck-flow
  :straight t
  :after (js2-mode flycheck)
  :config
  (el-patch-defun flycheck-flow--predicate ()
    "Shall we run the checker?"
    (and
     buffer-file-name
     (file-exists-p buffer-file-name)
     (locate-dominating-file buffer-file-name ".flowconfig")
     (el-patch-remove (flycheck-flow-tag-present-p))))
  (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))

(use-package flow-minor-mode
  :straight t
  :after js2-mode
  :config
  ;; TODO: move to smart-jump
  (bind-key "M-." nil flow-minor-mode-map)
  (bind-key "M-," nil flow-minor-mode-map)
  (bind-key "M-'" 'my-flow-minor-jump-to-definition-then-dumb-jump flow-minor-mode-map)
  (bind-key "C-M-'" 'xref-pop-marker-stack flow-minor-mode-map))

(use-package json-mode
  :defer t
  :bind (:map json-mode-map
         ("C-c C-c" . my-json-mode-run-jq)
         ("C-c C-j" . my-json-jsonify)
         ("C-c C-i" . jq-interactively)
         ("C-c C-o" . my-json-copy-as-org)
         ("C-c C-m" . my-json-minify))
  :config
  (add-to-list 'magic-mode-alist `(,(rx buffer-start (? "[") "{\"") . json-mode))

  (defun my-check-string-json-p (&optional string)
    "Check if STRING represents valid json object.

If no STRING is specified, try to read the string at point."
    (setq string (or string (save-excursion
                              (goto-char (point-min))
                              (ignore-errors (read (current-buffer))))))
    (and (stringp string)
         (json-read-from-string string)))

  (defun my-read-json-and-json-mode (&optional string)
    "Read json from STRING and insert it into current buffer.

If no STRING is specified, try to read the string at point and
replace it with the resulting json object."
    (let ((replace nil))
      (setq string (or string (prog1 (save-excursion
                                       (goto-char (point-min))
                                       (ignore-errors (read (current-buffer))))
                                (setq replace t))))
      (when replace
        (goto-char (point-min))
        (delete-region (point) (save-excursion (forward-sexp) (point))))
      (insert (json-encode (json-read-from-string string)))
      (json-mode)))

  (add-to-list 'magic-fallback-mode-alist '(my-check-string-json-p . my-read-json-and-json-mode))

  (defvar my-json-mode-run-jq-history nil)
  (defun my-json-mode-run-jq (query &optional arg)
    "Run jq(1) on current buffer.

With prefix argument \\[universal-argument] replace the buffer
with the result of running jq(1)."
    (interactive (list
                  (read-string (format "jq query%s "
                                       (if (car my-json-mode-run-jq-history)
                                           (format " [default: %s]:" (car my-json-mode-run-jq-history))
                                         ":"))
                               nil 'my-json-mode-run-jq-history
                               my-json-mode-run-jq-history)
                  current-prefix-arg))
    (shell-command-on-region
     (point-min) (point-max)
     (concat "jq " (shell-quote-argument query))
     (when arg (current-buffer))
     (when arg t))
    (when arg
      (json-mode-beautify)))

  (defun my-json-to-org (string)
    "Convert json to org mode string."
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (let ((data (json-read)))
        (when (> (length data) 0)
          (erase-buffer)
          (insert "| " (mapconcat (lambda (x) (format "%s" (car x))) (aref data 0) " | ") " |\n")
          (insert "|-")
          (mapc
           (lambda (row)
             (insert "| " (mapconcat (lambda (x) (format "%s" (cdr x))) row " | ") " |\n"))
           data)
          (goto-char (point-min))
          (org-table-align)))
      (buffer-string)))

  (defun my-org-table-to-json (data)
    "Convert org table to JSON.

First line specifies the keys."
    (-let* (((header . data) data))
      (setq data (--drop-while (eq 'hline it) data))
      (json-encode (--map (-zip header it) data))))

  (defun my-json-copy-as-org ()
    "Copy current json data as org table.

This assumes that the data is an array of homogenous items."
    (interactive)
    (kill-new (my-json-to-org (buffer-string))))

  (defun my-json-org-as-json ()
    "Copy current org table as json data.

This assumes that the data is an array of homogenous items."
    (interactive)
    (kill-new (my-org-table-to-json (org-table-to-lisp))))

  (defun my-json-jsonify (beg end)
    "Turn javascript object literal into JSON.

Takes active region or the entire buffer"
    (interactive (list
                  (or (and (use-region-p) (region-beginning)) (point-min))
                  (or (and (use-region-p) (region-end)) (point-max))))
    (shell-command-on-region beg end "jsonify" nil t))

  (defun my-json-minify ()
    "Minify the json at point."
    (interactive)
    (goto-char (point-min))
    (let* ((p (point))
           (json (json-read)))
      (delete-region p (point))
      (save-excursion
        (insert (json-encode json))))))

(use-package jump-char
  :bind (("M-m" . jump-char-forward)))

(use-package keyadvice
  :defer t
  :init (progn (load "~/.emacs.d/projects/keyadvice.el/autoloads.el")))

(use-package keyfreq
  :straight t
  :bind ("C-. C-k" . keyfreq-show)
  :config
  (progn
    ;; hack to make nicer format in keyfreq-show
    (defun keyfreq-format-list (list &optional func)
      "Returns formatted string with command usage statistics.

The LIST is the `keyfreq-table' converted to a list using the `keyfreq-list'.

If FUNC is nil each line contains number of times command was
called and the command; if it is t percentage usage is added in
the middle; if it is 'raw each line will contain number an
command separated by single line (with no formatting) otherwise
FUNC must be a function returning a string which will be called
for each entry with three arguments: number of times command was
called, percentage usage and the command."
      (let* ((sum (car list)))
        (mapconcat
         (cond
          ((not func) (lambda (e) (format "%7d  %s\n" (cdr e) (car e))))
          ((equal func t)
           (lambda (e) (format "%7d  %6.2f%% %10s %s\n"
                               (cdr e) (/ (* 1e2 (cdr e)) sum) (key-description (where-is-internal (car e) nil t)) (car e))))
          ((equal func 'raw) (lambda (e) (format "%d %s\n" (cdr e) (car e))))
          (t (lambda (e) (funcall func (cdr e) (/ (* 1e2 (cdr e)) sum) (car e)))))
         (cdr list) "")))))

(use-package ledger-mode
  :mode ("\\.ledger\\'" . ledger-mode)
  :config
  (progn

    (defun my-ledger-accouts-list ()
      "Return all ledger accounts in ~/org/ledger.ledger."
      (with-temp-buffer
        (insert-file-contents "~/org/ledger.ledger")
        (ledger-accounts-list-in-buffer)))

    (defun my-ledger-payees-list ()
      "Return all ledger payees in ~/org/ledger.ledger."
      (with-temp-buffer
        (insert-file-contents "~/org/ledger.ledger")
        (ledger-payees-in-buffer)))

    (el-patch-defun ledger-read-transaction ()
      "Read the text of a transaction, which is at least the current date."
      (el-patch-swap
        (let ((reference-date (or ledger-add-transaction-last-date (current-time))))
          (read-string
           "Transaction: "
           ;; Pre-fill year and month, but not day: this assumes DD is the last format arg.
           (ledger-format-date reference-date)
           'ledger-minibuffer-history))
        (ledger-read-date "New transaction")))

    (defun my-format-airbank-to-ledger (account payee)
      (interactive
       (list (completing-read "Account: " (my-ledger-accouts-list))
             (completing-read "Payee: " (my-ledger-payees-list))))
      (save-excursion
        (forward-line 1)
        (transpose-lines 1))
      (-let* ((start (point))
              (date-raw (buffer-substring
                         (line-beginning-position)
                         (+ 10 (line-beginning-position))))
              ((d m y) (split-string date-raw "\\."))
              (date-ledger (format "%s/%s/%s" y m d)))
        (forward-line)
        (replace-regexp "\\," "." nil (line-beginning-position) (line-end-position))
        (replace-regexp "CZK" "Kc" nil (line-beginning-position) (line-end-position))
        (replace-regexp "-" "" nil (line-beginning-position) (line-end-position))
        (let ((amount (buffer-substring (line-beginning-position) (line-end-position))))
          (forward-line 2)
          (delete-region start (point))
          (insert (format "%s * %s
    %s   %s
    Assets:Checking:Air Bank  -%s

" date-ledger payee account amount amount)))))))

(use-package lisp-mode
  :defer t
  :init
  (progn
    (require 'elsa-font-lock)
    (use-package flycheck-elsa)

    (add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))
    (defun my-emacs-lisp-init ()
      (require 'my-lisp-mode-defs "~/.emacs.d/files/lisp-mode-defs")
      (add-hook 'my-newline-hook 'my-emacs-lisp-open-line nil :local)
      (set-input-method "english-prog")
      (eldoc-mode 1)
      (buttercup-minor-mode 1)
      (elsa-setup-font-lock)
      (add-to-list 'imenu-generic-expression
                   '("Ert tests" "\\(^(ert-deftest +\\)\\(\\_<.+\\_>\\)" 2))
      (setq completion-at-point-functions nil))
    (add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-init)))

(use-package lsp-mode
  :straight t
  :after php-mode
  :custom
  (lsp-enable-completion-at-point nil)
  (lsp-prefer-flymake nil)
  :config
  (use-package lsp-ui
    :straight t
    :custom
    (lsp-ui-sideline-enable nil)
    (lsp-ui-peek-enable nil)
    (lsp-ui-imenu-enable nil)
    (lsp-ui-flycheck-enable nil)

    (lsp-ui-doc-enable t)
    (lsp-ui-doc-use-webkit t)
    :config
    (el-patch-defun lsp-ui-flycheck-enable (_)
      "Enable flycheck integration for the current buffer."
      (el-patch-wrap 2 0
        (when lsp-ui-flycheck-enable
          (when lsp-ui-flycheck-live-reporting
            (setq-local flycheck-check-syntax-automatically nil))
          (setq-local flycheck-checker 'lsp-ui)
          (lsp-ui-flycheck-add-mode major-mode)
          (add-to-list 'flycheck-checkers 'lsp-ui)
          (add-hook 'lsp-after-diagnostics-hook 'lsp-ui-flycheck--report nil t)))))

  (require 'lsp-clients)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package magit
  ;; :straight t
  :init
  (bind-keys :prefix "C-c m"
             :prefix-map ctl-c-m-map
             :prefix-docstring "Magit map")
  :bind (("C-c m b" . magit-blame-addition)
         ("C-c m d" . magit-dispatch-popup)
         ("C-c m f" . magit-fetch-popup)
         ("C-c m i" . magit-diff-popup)
         ("C-c m l" . magit-log-popup)
         ("C-c m m" . magit-log-all)
         ("C-c m v" . magit-show-refs-popup)
         ("C-c m s" . magit-status)
         ("C-c m c" . magit-find-git-config-file)
         ("C-c m C-f" . magit-find-file)
         )
  :config
  (use-package gitattributes-mode :straight t)
  (magit-auto-revert-mode 1)
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (bind-key "<tab>" 'magit-section-toggle magit-mode-map)
  (require 'flyspell))

(use-package malabar-mode
  :mode "\\.java\\'"
  :disabled t
  :config
  (progn
    (semantic-mode 1)

    (defun my-maven-root ()
      "Find maven root for this project."
      (f--traverse-upwards (f-exists? (f-expand "pom.xml" it))))

    (defun my-maven-test-root ()
      "Find maven test root for this project."
      (concat (my-maven-root) "/src/test/java/"))

    (defun my-maven-src-root ()
      "Find maven source root for this project."
      (concat (my-maven-root) "/src/main/java/"))

    ;; Helpers for templates
    (defun my-maven-test-package ()
      "Return the package of a test file."
      (let ((root (my-maven-test-root)))
        (s-replace "/" "." (s-chop-prefix root (f-parent (buffer-file-name))))))

    (defun my-maven-src-package ()
      "Return the package of a test file."
      (let ((root (my-maven-src-root)))
        (s-replace "/" "." (s-chop-prefix root (f-parent (buffer-file-name))))))

    (defun my-maven-class ()
      "Return the class name of a test file."
      (f-no-ext (buffer-name)))

    ;; Better test support
    (defun my-malabar--visit-corresponding-test-or-source (&optional other-window)
      (let ((test-file (malabar-find-corresponding-test)))
        (if (equal (buffer-file-name) test-file)
            (let ((root (my-maven-src-root))
                  (pkg-path (s-replace "\\." "/" (my-maven-test-package)))
                  (class (s-chop-suffix "Test" (my-maven-class))))
              (when other-window
                (other-window 1))
              (find-file (concat root pkg-path "/" class ".java")))
          (when other-window
            (other-window 1))
          (find-file test-file)
          (when (= (buffer-size) 0)
            (insert "testclass")
            (yas-expand)))))

    (defun my-malabar-visit-corresponding-test-or-source ()
      "Visit the test file.

If it is empty, expand the default template `testclass' there.

If in the test file, visit source."
      (interactive)
      (my-malabar--visit-corresponding-test-or-source))

    (defun my-malabar-visit-corresponding-test-or-source-other-window ()
      "Visit the test file in other window.

If it is empty, expand the default template `testclass' there.

If in the test file, visit source."
      (interactive)
      (my-malabar--visit-corresponding-test-or-source :other-window))

    (defun my-maven-init-project ()
      "Initialize the maven project structure in current directory."
      (interactive)
      (mapc 'f-mkdir '("src"
                       "src/main"
                       "src/main/java"
                       "src/main/resources"
                       "src/main/config"
                       "src/test"
                       "src/test/java"
                       "src/test/resources"
                       "target"))
      (unless (f-exists? "pom.xml")
        (with-temp-file "pom.xml"
          (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<project xmlns=\"http://maven.apache.org/POM/4.0.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd\">
  <modelVersion>4.0.0</modelVersion>
  <groupId>me</groupId>
  <artifactId>" (f-filename default-directory) "</artifactId>
  <version>1</version>
  <packaging>jar</packaging>
  <dependencies>
    <dependency>
       <groupId>junit</groupId>
       <artifactId>junit</artifactId>
       <version>4.10</version>
       <scope>test</scope>
    </dependency>
  </dependencies>
  <properties>
    <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
    <maven.compiler.source>1.7</maven.compiler.source>
    <maven.compiler.target>1.7</maven.compiler.target>
  </properties>
</project>
")))
      (find-file "pom.xml")
      (goto-char (point-min))
      (search-forward "groupId>"))

    (defun my-malabar-setup ()
      "Malabar setup."
      (bind-keys :map (current-local-map)
        ("C-c C-j C-t" . my-malabar-visit-corresponding-test-or-source)
        ("C-c C-j 4 C-t" . my-malabar-visit-corresponding-test-or-source-other-window))
      (abbrev-mode -1))
    (add-hook 'malabar-mode-hook 'my-malabar-setup)))

(use-package markdown-mode
  :straight t
  :mode ("\\.md$" . gfm-mode)
  :config
  (use-package markdown-toc :straight t)
  (progn
    (defun my-markdown-init ()
      (setq-local
       which-key-inhibit-regexps
       (list
        "C-c C-s"
        "C-c C-c"
        ))
      (modify-syntax-entry ?\" "$\""))
    (add-hook 'gfm-mode-hook 'my-markdown-init)

    (defun my-markdown-generate-anchors ()
      "Add anchors above each header.  If an anchor is present,
delete it and re-insert new one."
      (interactive)
      (let (m)
        (while (re-search-forward "^\\(#+\\) \\(.*\\)" nil t)
          (setq m (match-string 2))
          (beginning-of-line)
          (previous-line)
          (if (looking-at "<a")
              (delete-region (point) (line-end-position))
            (newline))
          (insert (concat
                   "<a name=\""
                   (replace-regexp-in-string " " "-" (downcase m))
                   "\" />"))
          (next-line 2))))))

(use-package message
  :defer t
  :init
  (progn
    (add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime))
  :config
  (progn
    (use-package notmuch)
    (use-package smtpmail)))

(use-package gnus-alias
  :straight t
  :after message
  :init (gnus-alias-init)
  :bind (:map message-mode-map
         ("C-c i" . 'gnus-alias-select-identity))
  :config
  (defun my-message-gnus-alias-init ()
    (when (equal (my-where-am-i) "logio")
      (run-with-timer 0.2 nil (lambda () (gnus-alias-use-identity "logio")))))

  (add-hook 'message-setup-hook 'my-message-gnus-alias-init))

(use-package ob-mongo :straight t)

(use-package multiple-cursors
  :straight t
  :bind (("C-c C-S-c" . mc/edit-lines)
         ("s-\\" . mc/mark-more-like-this-extended)
         ("s-=" . mc/mark-all-like-this-dwim)
         ("H-SPC" . set-rectangular-region-anchor)
         ("M-A" . mc/edit-beginnings-of-lines)
         ("M-E" . mc/edit-ends-of-lines))
  :config
  (bind-key "<f1>" 'mc/insert-numbers mc/keymap))

(use-package neon-mode
  :mode ("\\.neon\\'" . neon-mode)
  :config
  (use-package yaml-mode)
  (sp-local-pair 'neon-mode "%" "%" :unless '(sp-in-comment-p))
  (defun my-neon-mode-init ()
    (setq-local beginning-of-defun-function 'my-yaml-beginning-of-defun)
    (setq-local font-lock-extra-managed-props '(syntax-table))
    (setq-local parse-sexp-lookup-properties t)
    (font-lock-add-keywords
     nil
     '(("\\({{{?\\)\\(\\(#\\)\\|/\\)?\\(.*?\\)\\(\\(}}}?\\)\\|\\( \\(.*?\\)\\(}}}?\\)\\)\\)"
        (1 font-lock-builtin-face t)
        (3 '(face font-lock-builtin-face syntax-table (1)) t t)
        (4 font-lock-keyword-face t)
        (6 font-lock-builtin-face t t)
        (8 nil t t)
        (9 font-lock-builtin-face t t))))
    (smartparens-mode 1)
    (smartparens-strict-mode 1))
  (add-hook 'neon-mode-hook 'my-neon-mode-init))

(use-package nginx-mode :straight t)

(use-package notmuch
  :init
  (progn
    (autoload #'my-notmuch-unread "notmuch" nil t)
    (autoload #'my-notmuch-inbox "notmuch" nil t)
    (bind-keys :map ctl-dot-prefix-map
      ("C-u" . my-notmuch-unread)
      ("TAB" . my-notmuch-inbox)
      ("C-a" . my-notmuch-archived)
      ("C-r" . my-notmuch-newsletter)
      ("C-n" . notmuch)))
  :config
  (progn
    (defun my-notmuch-unread ()
      "Display buffer with unread mail."
      (interactive)
      (notmuch-search "tag:unread"))

    (defun my-notmuch-inbox ()
      "Display buffer with inbox mail."
      (interactive)
      (notmuch-search "tag:inbox"))

    (defun my-notmuch-archived ()
      "Display buffer with archived mail."
      (interactive)
      (notmuch-search "tag:archived"))

    (defun my-notmuch-newsletter ()
      "Display buffer with newsletter tag."
      (interactive)
      (notmuch-search "tag:newsletter and not tag:archived"))

    (defun my-notmuch-delete-mail (&optional beg end)
      (interactive (if (use-region-p)
                       (list (region-beginning) (region-end))
                     (list nil nil)))
      (let ((change '("+deleted" "-unread" "-inbox")))
        (if (eq major-mode 'notmuch-search-mode)
            (progn
              (notmuch-search-tag change beg end)
              (notmuch-search-next-thread))
          (notmuch-show-tag change))))

    (defun my-notmuch-open-attachment-dwim (command arg file)
      "Open attachment at point using COMMAND.

ARG and FILE are passed to `dired-do-shell-command' as expected
by that command."
      (interactive
       (-if-let (original-file (cdadr (assoc 'attachment (notmuch-show-current-part-handle nil))))
           (let ((file (make-temp-name
                        (expand-file-name
                         (concat "notmuch-attachment-" original-file)
                         temporary-file-directory))))
             (list
              (dired-read-shell-command "& on %s: " current-prefix-arg (list file))
              current-prefix-arg
              file))
         (user-error "Point not on attachment.")))
      (mm-save-part-to-file (notmuch-show-current-part-handle nil) file)
      (dired-do-shell-command command arg (list file)))

    (bind-key "C-&" 'my-notmuch-open-attachment-dwim notmuch-show-mode-map)
    (bind-key "RET" 'goto-address-at-point goto-address-highlight-keymap)
    (bind-key "d" 'my-notmuch-delete-mail notmuch-show-mode-map)
    (bind-key "d" 'my-notmuch-delete-mail notmuch-search-mode-map)
    (bind-key "g" 'notmuch-poll-and-refresh-this-buffer notmuch-search-mode-map)))

(use-package notmuch-unread
  :after notmuch
  :straight t
  :config
  (el-patch-defun notmuch-unread-update-handler ()
    "Update the mode line."
    (el-patch-swap
      (setq notmuch-unread-mode-line-string
            (format " [✉ %d]" (notmuch-unread-count)))
      (let ((count (notmuch-unread-count)))
        (if (> count 0)
            (setq notmuch-unread-mode-line-string
                  (format " [✉ %d]" count))
          (setq notmuch-unread-mode-line-string ""))))
    (force-mode-line-update)))

(use-package occur
  :commands occur
  :init
  (bind-key "<f2>" 'my-occur-dwim)
  :config
  (bind-keys :map occur-mode-map
    ("n" . occur-next)
    ("p" . occur-prev)
    ("o" . occur-mode-display-occurrence)))

(use-package password-generator :straight t)
(use-package paren-face :straight t)

(use-package sallet
  :straight (:repo "git@github.com:Fuco1/sallet.git")
  :bind (("C-. C-." . helm-occur)
         ("C-'" . csallet-buffer)
         ("H-b". sallet-register-point)
         ("M-.". sallet-imenu))
  :config
  (progn
    (require 'sallet-concurrent)
    (bind-key "C-c p"
              (defhydra sallet-hydra (:color blue)
                "Sallet hydra"
                ("f" sallet-gtags-files "Find files")
                ("t" sallet-gtags-tags "Find tags")))
    (use-package autobookmarks)))

(use-package org
  :straight org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  ;; The following lines are always needed.  Choose your own keys.
  :bind  (("C-c l" . org-store-link)
          ("<f12>" . org-agenda)
          ("C-<f12>" . org-agenda-time-limit)
          ("C-M-r" . org-capture)
          ("C-c C-x C-j" . org-clock-goto)
          ("C-c C-x C-o" . org-clock-out)
          ("C-c C-x C-g" . my-org-get-goal-templates)
          ("C-c C-x l" . my-org-get-items-summary)
          ("C-c C-x <C-i-key>" . org-clock-in))
  :config
  (progn
    ;; Re-set this to just space to avoid funny issues when realigning
    ;; tables in variable-width settings.
    (setq org-table-separator-space " ")
    (my-load-or-tangle (f-join (f-parent my-vendor-file) "org-defs"))
    (load-relative "org-defs")))

(use-package org-jira
  :straight
  (org-jira :repo "git@github.com:ahungry/org-jira.git"
            :fork (:repo "git@github.com:Fuco1/org-jira.git")))

;; TODO: move into a separate file
(use-package php-mode
  :straight t
  :mode ("\\.php[st]?\\'" . php-mode)
  :init
  (add-to-list 'magic-mode-alist `(,(rx "<?php") . php-mode))
  :config
  (progn
    (use-package better-jump)
    (use-package php-eldoc :straight t)
    (use-package php-refactor)
    (use-package nette-tester)
    (use-package lsp-mode)

    (bind-key "M-'" 'smart-jump-go php-mode-map)
    (bind-key "C-M-'" 'smart-jump-back php-mode-map)
    (bind-key "(" 'self-insert-command php-mode-map)
    (bind-key "{" 'self-insert-command php-mode-map)
    (bind-key "M-j" 'my-join-lines php-mode-map)

    (font-lock-add-keywords 'php-mode '((" \\(:\\_<.*?\\_>\\)" 1 'font-lock-builtin-face t)))

    (use-package ob-php
      :straight
      (ob-php :repo "https://framagit.org/steckerhalter/ob-php.git"
              :fork (:repo "git@github.com:Fuco1/ob-php.git")))

    (bind-key "C-x C-d"
              (defhydra hydra-php-refactor (:color blue)
                ("d" my-php-debug-geben "Debug with XDebug")
                ("r" php-refactor-rename-variable "Rename variable")
                ("i" php-refactor-inline-variable "Inline variable")
                ("e" php-refactor-extract-variable "Extract variable")
                ("c" my-php-implement-constructor "Implement constructor")
                ("v" my-php-add-private-variables-for-constructor-arguments "Create properties for constructor")
                ("g" my-php-implement-getters-and-setters "Implement getters")
                ("C-s" my-php-goto-specific "Goto specific"))
              php-mode-map)

    ;; imenu for instance variables
    (defun php-create-regexp-for-instance-variable ()
      (concat
       "^\\s-*" (regexp-opt '("public" "protected" "private" "var"))
       ;; Static?
       "\\s-+\\(?:static\\s-+\\)?"
       ;; Capture name
       "\\$\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*\\(?:;\\|=\\)"))

    (setq php-imenu-generic-expression
          `(("Namespaces"
             ,(php-create-regexp-for-classlike "namespace") 1)
            ("Classes"
             ,(php-create-regexp-for-classlike "class") 1)
            ("Interfaces"
             ,(php-create-regexp-for-classlike "interface") 1)
            ("Traits"
             ,(php-create-regexp-for-classlike "trait") 1)
            ("All Methods"
             ,(php-create-regexp-for-method "\\(?:\\sw\\|\\s_\\)+") 1)
            ("Instance Variables"
             ,(php-create-regexp-for-instance-variable) 1)
            ("Named Functions"
             "^\\s-*function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1)))

    (defun my-php-jump-to-variable ()
      "Jump to a variable in the selected window."
      (interactive)
      (bjump-jump ?$))
    (bind-key "C-$" 'my-php-jump-to-variable php-mode-map)
    (unbind-key "C-." php-mode-map)

    (defun my-php-eldoc-function ()
      (or (php-eldoc-function)
          (unless (and (featurep 'tramp)
                       (tramp-tramp-file-p (buffer-file-name)))
            (when (fboundp 'ggtags-eldoc-function)
              (ggtags-eldoc-function)))))

    (defun my-php-find-project-root (&rest _ignored)
      (shut-up
        (-when-let* ((file-name (buffer-file-name))
                     (file (expand-file-name file-name)))
          (expand-file-name
           (with-temp-buffer
             (if (and (not (file-remote-p file))
                      (= 0 (shell-command "global -p" (current-buffer))))
                 (s-trim (buffer-string))
               ;; TODO: add projectile "php" project type?
               (locate-dominating-file default-directory "composer.json")))))))

    (defun my-php-local-file-name (filename)
      "Get local part of file name.

Works both on local files and tramp files (where it cuts off the
network prefix)."
      (when filename
        (if (file-remote-p filename)
            (tramp-file-name-localname (tramp-dissect-file-name filename))
          filename)))

    (defun my-php-update-gtags ()
      "Update gtags for current file upon saving."
      (start-file-process
       "global--single-udpate"
       (get-buffer-create "*async update tags*")
       "global"
       "--single-update"
       (my-php-local-file-name
        (expand-file-name (buffer-file-name)))))

    (defun my-php-compile ()
      (interactive)
      (let* ((dir (file-name-directory (ft-get-test-file)))
             (search-path (-cons*
                           (f-parent (f-parent dir))
                           (f-parent dir)
                           (f-entries dir 'f-dir? :recursive))))
        (let ((default-directory dir))
          (my-compile))
        (with-current-buffer (get-buffer "*compilation*")
          (setq-local compilation-search-path search-path))))

    (bind-key "C-c C-c" 'my-php-compile php-mode-map)

    (defun my-php-run ()
      "Run all Nette tests found in current directory."
      (interactive)
      (let* ((file (my-php-local-file-name (buffer-file-name)))
             ;; support for running PW stub tests
             (pw-root (if (bound-and-true-p my-pw-root) (format "PW_ROOT='%s'" my-pw-root) ""))
             (pw-test-uuid (if (bound-and-true-p my-pw-test-uuid) (format "PW_TEST_UUID='%s'" my-pw-test-uuid) "")))
        (when (buffer-modified-p) (save-buffer))
        (async-shell-command (format "%s %s php '%s'" pw-root pw-test-uuid file))))
    (bind-key "C-c C-r" 'my-php-run php-mode-map)

    (defun my-php-get-function-args (&optional name types)
      "Return all arguments of php function.

Point should be at the line containing `function'.

If TYPES is non-nil, return a list of pairs with the variable
name in car and the type in cdr."
      (cl-block exit
        (save-excursion
          (when name
            (goto-char (point-min))
            (unless (search-forward (concat "function " name) nil t)
              (cl-return-from exit nil)))
          (let ((function-args (sp-get (sp-down-sexp)
                                 (buffer-substring-no-properties :beg :end)))
                (args nil))
            (save-match-data
              (with-temp-buffer
                (insert function-args)
                (goto-char (point-min))
                (if types
                    (while (re-search-forward "\\(?:\\([a-zA-Z0-9\\_]+\\) +\\)?\\(&?\\$.*?\\)[ \n\t,)]" nil t)
                      (push (cons (match-string 2) (match-string 1)) args))
                  (while (re-search-forward "\\(&?\\$.*?\\)[ \n\t,)]" nil t)
                    (push (match-string 1) args)))))
            (nreverse args)))))

    (defun my-php-get-function-return-type (&optional name)
      "Return the return type of the function.

Point should be at the line containing `function'."
      (cl-block exit
        (save-excursion
          (when name
            (goto-char (point-min))
            (unless (search-forward (concat "function " name) nil t)
              (cl-return-from exit nil)))
          (goto-char (sp-get (sp-down-sexp) :end))
          (when (search-forward ":" (save-excursion (search-forward "{")) t)
            (when (re-search-forward "[a-zA-Z0-9\\_]+" nil t)
              (match-string-no-properties 0))))))

    (defun my-php-implement-constructor (&optional use-instance-variables)
      "Implement constructor.

This assings all variables in the argument list to instance
variables of the same name.

With prefix argument, use all the instance variables as inputs."
      (interactive "P")
      (when use-instance-variables
        (let* ((vars (my-php-get-instance-variables))
               (long-format (>= (length vars) 4)))
          (save-excursion
            (goto-char (point-min))
            (when (search-forward "__construct(" nil t)
              (when long-format (insert "\n"))
              (insert (mapconcat (lambda (x) (concat "$" x)) vars
                                 (if long-format ", \n" ", ")))
              (when long-format (insert "\n"))))))
      (let ((args (my-php-get-function-args)))
        (sp-restrict-to-pairs (list "{" "}") 'sp-down-sexp)
        (forward-line)
        (--each args
          (insert (format "$this->%s = %s;"
                          (replace-regexp-in-string "[&$]" "" it)
                          (replace-regexp-in-string "[&]" "" it)))
          (insert "\n"))
        (delete-char -1)
        (when (looking-at " *}")
          (newline))
        (let ((p (point)))
          (save-excursion
            (goto-char (point-min))
            (search-forward "__construct(")
            (indent-region (point) p)))))

    (defun my-php-translate-type-annotation (type)
      "Translate TYPE into string for annotation.

If the TYPE is array, return mixed[].  If the type is an object,
return as it is.  If type is nil, return an empty string."
      (cond
       ((equal type "array") "mixed[] ")
       ((stringp type) type)
       ((null type) "")))

    (defun my-php-should-insert-type-annotation (type)
      "Test if we should insert a TYPE annotation.

Only insert an docstring annotation if the TYPE and translated
type differ."
      (not (equal type (my-php-translate-type-annotation type))))

    (defun my-php-add-private-variables-for-constructor-arguments ()
      "Generate private variable definitions for constructor arguments."
      (interactive)
      (-when-let (args (my-php-get-function-args "__construct" t))
        (let ((beg (point)))
          (--each args
            (insert (format "/**\n* @var %s\n*/\n private %s;\n\n\n"
                            (my-php-translate-type-annotation (cdr it))
                            (replace-regexp-in-string "[&]" "" (car it)))))
          (delete-char -2)
          (indent-region beg (point)))))

    (defun my-php-implement-getters-and-setters (&optional getters-only)
      "Implement getters and setters for all instance variables.

With prefix argument, only implemnent getters."
      (interactive "P")
      (let ((variables (my-php-get-instance-variables))
            (p (point)))
        (--each variables
          (unless (save-excursion
                    (goto-char (point-min))
                    (search-forward (format "public function get%s(" (s-upper-camel-case it)) nil t))
            (insert (format "public function get%s() {\n return $this->%s;\n }\n\n"
                            (s-upper-camel-case it) it)))
          (unless getters-only
            (unless (save-excursion
                      (goto-char (point-min))
                      (search-forward (format "public function set%s(" (s-upper-camel-case it)) nil t))
              (insert (format "public function set%s($%s) {\n $this->%s = $%s; \n }\n\n"
                              (s-upper-camel-case it) it it it)))))
        (indent-region p (point))))

    (defun my-php-get-instance-variables ()
      "Return all instance variables.

These are retrieved from `imenu--index-alist'."
      (unless (or imenu--index-alist
                  (car-safe imenu--index-alist))
        (imenu--make-index-alist))
      (-map
       'car
       (-concat (cdr (assoc "Instance Variables" imenu--index-alist))
                (cdr (assoc "Property" imenu--index-alist)))))

    (defun my-php-implement-proxy-function-call (proxy-through)
      "Proxy this method through instance property."
      (interactive (list (progn
                           (imenu--make-index-alist)
                           (completing-read "Proxy through: " (my-php-get-instance-variables)))))
      (let ((args (my-php-get-function-args)))
        (sp-restrict-to-pairs (list "{" "}") 'sp-down-sexp)
        (forward-line)
        (insert (format "return $this->%s->%s(%s);" proxy-through (which-function)
                        (mapconcat (lambda (it) (replace-regexp-in-string "[&]" "" it)) args ", ")))
        (indent-according-to-mode)))

    (defun my-php-ggtags-get-definition (defs)
      (ignore-errors
        (let* ((defs-sorted (-sort
                             (-lambda ((_ _ a) (_ _ b))
                               (equal (f-ext a) "php"))
                             defs))
               (candidate-text (caar defs-sorted)))
          (--> (s-trim candidate-text)
               (replace-regexp-in-string "[ \t]*[{!]?$" "" it)
               (ggtags-fontify-code it)
               (concat it (and (cdr defs) " [guess]"))))))

    (defun my-php-debug-geben ()
      "Run current PHP script for debugging with geben"
      (require 'geben)
      (interactive)
      (call-interactively 'geben)
      (call-process-shell-command
       (concat "XDEBUG_CONFIG='idekey=my-php-559' php " (buffer-file-name) " &")
       nil 0))

    (defun my-php-open-line (&optional arg)
      (when (and (nth 4 (syntax-ppss))
                 (save-excursion
                   (forward-line -1)
                   (back-to-indentation)
                   ;; TODO: simplify
                   (or (looking-at "\\*")
                       (and (looking-at "/\\*\\*")
                            (progn
                              (forward-line)
                              (back-to-indentation)
                              (not (looking-at "*/")))))))
        (insert "* ")
        (indent-according-to-mode)))

    (defun my-php-mode-init ()
      (lsp)
      (setq-local php-style-delete-trailing-whitespace t)
      (add-hook 'after-save-hook 'my-php-update-gtags t t)
      (when (and (buffer-file-name)
                 (string-match-p "/vendor/" (buffer-file-name)))
        (flycheck-mode -1))
      (setq-local flycheck-php-phpstan-executable
                  (-first 'file-exists-p
                          (--map (concat (my-php-find-project-root) "/" it)
                                 (list
                                  "/vendor/bin/phpstan"
                                  "/vendor/bin/phpstan.phar"))))
      (let ((phpcs (concat (my-php-find-project-root)
                           "/vendor/bin/phpcs")))
        (when (file-exists-p phpcs)
          (setq-local flycheck-php-phpcs-executable phpcs)))
      (setf (flycheck-checker-get 'php-phpcs 'working-directory)
            (lambda (_checker)
              (my-php-find-project-root)))
      (setq-local flycheck-phpcs-standard
                  (-first 'file-exists-p
                          (--map (concat (my-php-find-project-root) "/" it)
                                 (list
                                  "phpcs.xml"
                                  "ruleset.xml"
                                  ))))
      (bind-key "<tab>" 'smart-tab php-mode-map)
      (add-hook 'my-newline-hook 'my-php-open-line nil :local)
      (php-enable-default-coding-style)
      (editorconfig-mode-apply)
      (setq-local ggtags-get-definition-function 'my-php-ggtags-get-definition)
      (setq-local eldoc-documentation-function 'my-php-eldoc-function)
      (when buffer-file-name
        (setq-local compile-command (concat "php -l " (my-php-local-file-name buffer-file-name))))

      (eldoc-mode 1))
    (add-hook 'php-mode-hook 'my-php-mode-init 'append)))

(use-package popwin
  :commands popwin-mode
  :config
  (progn
    (push '("*Pp Eval Output*" :height 15) popwin:special-display-config)))

(use-package prodigy
  :straight t
  :bind (:map ctl-dot-prefix-map
         ("o" . prodigy))
  :config
  (progn
    (defun prodigy-term-emulate-terminal (service output)
      (let ((process (plist-get service :process)))
        (prodigy-with-service-process-buffer service
          (unless (bound-and-true-p term-pending-delete-marker)
            (set (make-local-variable 'term-pending-delete-marker) (set-marker (make-marker) (point-min))))
          (unless (marker-position (process-mark process))
            (set-marker (process-mark process) (point-max)))
          (unwind-protect
              (progn
                (set-process-buffer process (current-buffer))
                (term-emulate-terminal process output))
            (set-process-buffer process nil)))))

    ;; (add-hook 'prodigy-process-on-output-hook 'prodigy-term-emulate-terminal)
    ))

(use-package prog-mode
  :init
  (progn
    (defun my-init-prog-mode ()
      "Init `prog-mode' based modes."
      (unless (string-prefix-p " *" (buffer-name))
        (flycheck-mode)
        (turn-on-smartparens-strict-mode)
        (highlight-thing-mode)
        (which-function-mode 1)
        (yas-minor-mode 1)))
    (add-hook 'prog-mode-hook 'my-init-prog-mode)))

(use-package proof-site
  :disabled t
  :load-path "/usr/share/emacs/site-lisp/ProofGeneral/generic/")

(use-package projectile
  :straight t
  :defer t
  :diminish projectile-mode
  :commands (my-projectile-rgrep)
  :config
  (progn
    (defun my-projectile-rgrep (regexp &optional files dir confirm)
      "Just like `rgrep' but takes the project directory as default."
      (interactive
       (progn
         (grep-compute-defaults)
         (cond
          ((and grep-find-command (equal current-prefix-arg '(16)))
           (list (read-from-minibuffer "Run: " grep-find-command
                                       nil nil 'grep-find-history)))
          ((not grep-find-template)
           (error "grep.el: No `grep-find-template' available"))
          (t (let* ((regexp (grep-read-regexp))
                    (files (grep-read-files regexp))
                    (dir (read-directory-name "Base directory: "
                                              (car (projectile-get-project-directories))
                                              (car (projectile-get-project-directories)) t))
                    (confirm (equal current-prefix-arg '(4))))
               (list regexp files dir confirm))))))
      (rgrep regexp files dir confirm))))

(use-package psysh
  :bind ("C-. r p" . psysh)
  :config
  (defun my-psysh-mode-init ()
    (smartparens-strict-mode 1))
  (add-hook 'psysh-mode-hook 'my-psysh-mode-init))

(use-package puppet-mode :straight t)

(use-package quail
  :config
  (progn
    (define-key quail-simple-translation-keymap (kbd "C-SPC") 'quail-select-current)
    (define-key quail-simple-translation-keymap "\C-f" 'quail-next-translation)
    (define-key quail-simple-translation-keymap "\C-b" 'quail-prev-translation)

    (defun my-noninteractive-toggle-input-method (&rest _ignored)
      (toggle-input-method)
      (throw 'quail-tag nil))

    (load-relative "layouts")))

(use-package recentf
  :disabled t
  :defer t
  :config
  (progn
    (defvar my-recentf-autosave-timer (run-with-timer 500 500 'recentf-save-list))))

(use-package restclient
  :straight t
  :mode ("\\.rest$" . restclient-mode)
  :config
  (use-package ob-restclient :straight t)
  (defun my-restclient-indirect-edit ()
    "Use `edit-indirect-region' to edit the request body in a
separate buffer."
    (interactive)
    (save-excursion
      (goto-char (restclient-current-min))
      (when (re-search-forward restclient-method-url-regexp (point-max) t)
        (forward-line)
        (while (cond
                ((and (looking-at restclient-header-regexp) (not (looking-at restclient-empty-line-regexp))))
                ((looking-at restclient-use-var-regexp)))
          (forward-line))
        (when (looking-at restclient-empty-line-regexp)
          (forward-line))
        (edit-indirect-region (min (point) (restclient-current-max)) (restclient-current-max) t))))

  (bind-key "C-c '" 'my-restclient-indirect-edit restclient-mode-map)

  (defun my-restclient-mode-init ()
    (smartparens-strict-mode 1))
  (add-hook 'restclient-mode-hook 'my-restclient-mode-init))

(use-package rst
  :config
  (progn
    (use-package sphinx-mode)
    (defun my-rst-mode-init ()
      (sphinx-mode 1))
    (add-hook 'rst-mode-hook 'my-rst-mode-init)))

(use-package server
  :config
  (setq server-temp-file-regexp
        (rx (or
             ;; Default
             (and bol "/tmp/Re")
             ;; Default
             (and "/draft" eol)
             ;; Fish prompt editation, do not ask to save buffer
             (and bol "/tmp/tmp" (1+ any) ".fish" eol)))))

(use-package sgml-mode
  :defer t
  :config
  (progn
    (defadvice sgml-delete-tag (after reindent-buffer activate)
      (cleanup-buffer))

    (defun my-html-mode-setup ()
      (smartparens-mode 1)
      (bind-keys :map html-mode-map
        ("C-c <deletechar>" . sgml-delete-tag)
        ("C-c C-f" . sp-html-next-tag)
        ("C-c C-b" . sp-html-previous-tag)))
    (add-hook 'html-mode-hook 'my-html-mode-setup)))

(use-package shackle
  :straight t
  :custom
  (shackle-rules
   '(
     ("*Help*" :select t)
     ("[0-9]\\{5\\}/.*\\.php" :regexp t :select t :inhibit-window-quit t :same t :popup nil)
     ("magit-log\\(-popup\\)?" :regexp t :select t :inhibit-window-quit t :same t)
     ("\\`\\*?magit:" :regexp t :select t :inhibit-window-quit t :same t)
     (Man-mode :select t :inhibit-window-quit t :same t)
     ("\\*ag search" :regexp t :inhibit-window-quit t :same t)
     (messages-buffer-mode :select t :inhibit-window-quit t :same t)
     (swb-result-mode :align 'below)
     (ess-r-help-mode :same t :inhibit-window-quit t)
     ("\\*prodigy" :regexp t :select t :inhibit-window-quit t :same t)
     ))
  :config
  (shackle-mode 1))

(use-package shell-pop
  :bind ("<f11>" . shell-pop)
  :custom
  (shell-pop-window-size 50)
  (shell-pop-autocd-to-working-dir t)
  (shell-pop-shell-type '("eshell" "*eshell*" (lambda nil (eshell))))
  (shell-pop-window-height 50))

;; TODO: move the registers to the respective modes
(use-package smart-jump
  :straight t
  :bind (("M-'" . smart-jump-go)
         ("C-M-'" . smart-jump-back)
         ("M-\"" . smart-jump-references))
  :config
  (progn
    (smart-jump-register
     :modes 'ess-r-mode
     :jump-fn 'xref-find-definitions
     :pop-fn 'xref-pop-marker-stack
     :refs-fn 'xref-find-references
     :should-jump t
     :heuristic 'error
     :async nil)

    (require 'smart-jump-typescript-mode)
    (smart-jump-typescript-mode-register)

    (smart-jump-register
     :modes 'php-mode
     :jump-fn 'xref-find-definitions
     :pop-fn 'xref-pop-marker-stack
     :should-jump t
     :heuristic 'error
     :async nil)))

(use-package smartparens
  :defer t
  :diminish smartparens-mode
  :init
  (progn
    (load-relative "smartparens")))

(use-package smerge-mode
  :config
  (progn
    ;; unfortunately C-. can't be represented by anything customize allows
    ;; for this variable.
    (setq smerge-command-prefix (kbd "C-. m"))

    (easy-mmode-defmap smerge-mode-map
      `((,smerge-command-prefix . ,smerge-basic-map))
      "Keymap for `smerge-mode'.")))

(use-package smex
  :defer t
  :init
  (progn
    (bind-key "M-x" 'beautify-smex)
    (bind-key "M-X" 'beautify-smex-mm)
    ;; This is your old M-x.
    (bind-key "C-c C-c M-x" 'execute-extended-command)

    (defun beautify-smex ()
      (interactive)
      (let ((ido-decorations
             '("{" "}" " | " " | ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
        (smex)))

    (defun beautify-smex-mm ()
      (interactive)
      (let ((ido-decorations
             '("{" "}" " | " " | ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
        (smex-major-mode-commands))))
  :config
  (progn
    (defadvice smex-prepare-ido-bindings (after add-more-bindings activate)
      (define-key ido-completion-map (kbd "=") "-"))))

(use-package sql
  :commands sql-mysql
  :config
  (progn
    (defun sql-send-paragraph ()
      "Send the current paragraph to the SQL process."
      (interactive)
      (let ((start (save-excursion
                     (backward-paragraph)
                     (when (looking-at "$") (forward-char))
                     (point)))
            (end (save-excursion
                   (forward-paragraph)
                   (point))))
        (sql-send-region start end)))

    (defun sql-send-string (str)
      "Send the string STR to the SQL process."
      (interactive "sSQL Text: ")

      (let ((comint-input-sender-no-newline nil)
            (s (replace-regexp-in-string "[[:space:]\n\r]+\\'" "" str)))
        (if (sql-buffer-live-p sql-buffer)
            (progn
              (save-excursion
                (with-current-buffer sql-buffer
                  ;; Send the string (trim the trailing whitespace)
                  ;; (sql-input-sender (get-buffer-process sql-buffer) s)
                  ;; FUCO: following two lines replace the above
                  (insert s)
                  (comint-send-input)
                  ;; FUCO-end

                  ;; Send a command terminator if we must
                  (if sql-send-terminator
                      (sql-send-magic-terminator sql-buffer s sql-send-terminator))

                  (message "Sent string to buffer %s." sql-buffer)))

              ;; Display the sql buffer
              (if sql-pop-to-buffer-after-send-region
                  (pop-to-buffer sql-buffer)
                (display-buffer sql-buffer)))

          ;; We don't have no stinkin' sql
          (message "No SQL process started."))))

    (defun my-sql-mode-init ()
      (when (require 'php-mode nil t)
        (font-lock-add-keywords
         nil
         `((,php-string-interpolated-variable-regexp
            0
            'font-lock-variable-name-face t))))
      (sql-set-product "mysql"))
    (add-hook 'sql-mode-hook 'my-sql-mode-init)

    (defun my-sql-interactive-mode-init ()
      (orgtbl-mode))
    (add-hook 'sql-interactive-mode-hook 'my-sql-interactive-mode-init)))

(use-package sql-workbench
  :mode ("\\.swb$" . swb-mode)
  :straight (:repo "git@github.com:Fuco1/sql-workbench.git")
  :config
  (progn
    (defun my-swb-mode-init ()
      "Init swb mode."
      (company-mode 1)
      (abbrev-mode 1))

    (add-hook 'swb-mode-hook 'my-swb-mode-init)))

(use-package stocklist
  :bind (("C-. q" . stocklist-show))
  :init
  (progn
    (defface my-stocklist-buffett
      '((t (:background "#212526")))
       "Face to highlight Warren Buffett's holdings.")

    (defface my-stocklist-div-growth
      '((t (:background "#4e9a06")))
       "Face to highlight stocks with a buy order.")

    (defface my-stocklist-buy-order
      '((t (:background "#4e9a06")))
       "Face to highlight stocks with a buy order.")))

(use-package textile-mode
  :mode "\\.textile\\'")

(use-package tex-site
  :load-path "site-lisp/auctex/"
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :commands (TeX-latex-mode
             TeX-mode
             tex-mode
             LaTeX-mode
             latex-mode)
  :config
  (progn
    (load-relative "latex-defs")))

(use-package "text-mode"
  :init
  (progn
    (defun my-init-text-mode ()
      "Init `text-mode' based modes."
      (variable-pitch-mode)
      (setq cursor-type 'bar)
      (turn-on-visual-line-mode)
      (smartparens-mode 1))
    (--each '(
              LaTeX-mode-hook
              org-mode-hook
              markdown-mode-hook
              rst-mode-hook
              gnus-article-mode-hook
              textile-mode-hook
              jira-markup-mode-hook
              )
      (add-hook it 'my-init-text-mode))))

(use-package transpose-frame
  :init
  (progn
    (defun transpose-frame-reverse (&optional frame)
      "Transpose windows arrangement at FRAME, flopping first..
Omitting FRAME means currently selected frame."
      (interactive)
      (transpose-frame-set-arrangement (transpose-frame-get-arrangement frame) frame
                                       'transpose 'flop 'flip)
      (if (interactive-p) (recenter)))
    (bind-key "C-^" 'transpose-frame-reverse)))

(use-package typescript-mode
  :straight t
  :defer t
  :config
  (bind-key "M-'" 'smart-jump-go typescript-mode-map)
  (bind-key "C-M-'" 'smart-jump-back typescript-mode-map)
  (bind-key "M-\"" 'smart-jump-references typescript-mode-map)

  (use-package tide
    :straight t
    :config
    (emr-declare-command 'tide-rename-symbol
      :title "rename symbol"
      :description "Rename symbol at point"
      :modes 'typescript-mode
      :predicate (lambda () (symbol-at-point)))

    (emr-declare-command 'tide-rename-file
      :title "rename file"
      :description "Rename current file and all it’s references in other files"
      :modes 'typescript-mode
      :predicate (lambda () t))

    (emr-declare-command 'tide-organize-imports
      :title "organize imports"
      :description "Organize imports to be in a cannonical form"
      :modes 'typescript-mode
      :predicate (lambda () t))

    (defun setup-tide-mode ()
      (interactive)
      (tide-setup)
      (flycheck-mode +1)
      (setq flycheck-check-syntax-automatically '(save mode-enabled))
      (eldoc-mode +1)
      (tide-hl-identifier-mode +1)
      ;; company is an optional dependency. You have to
      ;; install it separately via package-install
      ;; `M-x package-install [ret] company`
      (company-mode +1))

    (add-hook 'typescript-mode-hook #'setup-tide-mode)

    ;; formats the buffer before saving
    (add-hook 'before-save-hook 'tide-format-before-save)

    ;; aligns annotation to the right hand side
    (setq company-tooltip-align-annotations t))

  (defun my-ts-console-log (symbol)
    (interactive (list
                  (if (use-region-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    (tide-get-symbol-at-point))))
    (save-excursion
      (my-open-line)
      (insert (s-lex-format "console.log(\"${symbol} = \", JSON.stringify(${symbol}, null, 4))"))))

  (bind-key "C-c C-m l t" 'my-ts-console-log typescript-mode-map)

  (defun my-tslint-fix ()
    "Fix the current buffer with tslint."
    (interactive)
    (when (and flycheck-typescript-tslint-executable
               (buffer-file-name)
               (--any? (eq (flycheck-error-checker it) 'typescript-tslint) flycheck-current-errors))
      (message "Fixing buffer: tslint --fix %s" (buffer-file-name))
      (call-process
       flycheck-typescript-tslint-executable
       nil "*my-tslint errors*" nil "--fix" (buffer-file-name))
      (revert-buffer t t t)))

  (defun my-typescript-mode-setup ()
    (add-hook 'after-save-hook 'my-tslint-fix nil 'local)
    (when (buffer-file-name)
      (-when-let (root (locate-dominating-file (buffer-file-name) "tslint.json"))
        (setq-local
         flycheck-typescript-tslint-executable
         (concat root "/node_modules/.bin/tslint")))))

  (add-hook 'typescript-mode-hook #'my-typescript-mode-setup))

(use-package two-column
  :defer t
  :config
  (progn
    (defadvice 2C-dissociate (after close-window-after-disconnect activate)
      (delete-window))))

(use-package undercover
  :bind (("C-c C" . my-toggle-coverage))
  :config
  (defun my-toggle-coverage ()
    "Toggle display of undercover coverage report in current buffer.

If coverage is displayed, hide it, if not, show it."
    (interactive)
    (if (ov-in 'type 'undercover-report)
        (my-hide-coverage)
      (my-display-coverage)))

  (defun my-display-coverage (&optional report-file)
    "Display undercover coverage report in current buffer.

Before you call this function you should have manually run the
test suite over your project.

It is a good idea to set the value of
`undercover--report-file-path' locally for your project.  Use
`add-dir-local-variable' to store the report in the project
directory so that it won't get overwritten by a report from
anohther project.  You must then set the same value to the Emacs
instance that runs the tests, for example with

  --eval \"(setq undercover--report-file-path \\\"$PWD\\\")\"

runtime option."
  (interactive)
  (-when-let* ((root (locate-dominating-file (buffer-file-name) "Cask"))
               (data (json-read-file (or report-file
                                         undercover--report-file-path)))
               ((&alist 'source_files source-files) data)
               (this-file (-first
                           (-lambda ((&alist 'name name))
                             (equal (expand-file-name
                                     (concat root "/" name))
                                    (buffer-file-name)))
                           (-concat source-files nil)))
               ((&alist 'coverage coverage) this-file)
               (line-number 1)
               (lines-covered 0)
               (lines-coverable 0))
    (ov-clear 'type 'undercover-report)
    (set-window-margins (selected-window) 0 4)
    (save-excursion
      (mapc
       (lambda (call-count)
         (when call-count
           (cl-incf lines-coverable)
           (when (< 0 call-count)
             (goto-char (point-min))
             (forward-line (1- line-number))
             (ov (line-beginning-position)
                 (line-end-position)
                 'type 'undercover-report
                 'face '(:background "#4e9a06")
                 'before-string (propertize
                                 " "
                                 'display `((margin right-margin)
                                            ,(propertize
                                              (format "%dx" call-count)
                                              'face 'default))))
             (cl-incf lines-covered)))
         (cl-incf line-number))
       coverage))
    (message "%d from %d lines covered (%.2f%%)"
             lines-covered
             lines-coverable
             (/ (* 100.0 lines-covered) lines-coverable))))

  (defun my-hide-coverage ()
    "Hide undercover coverage report in current buffer."
    (interactive)
    (set-window-margins (selected-window) 0 0)
    (ov-clear 'type 'undercover-report)))

(use-package undo-tree
  :bind (("C-x u" . undo-tree-visualize))
  :diminish undo-tree-mode)

(use-package vc
  :defer t
  :config
  (progn
    (defadvice vc-diff (before call-register-automatically activate)
      (when (buffer-file-name)
        (unless (vc-registered (buffer-file-name))
          (vc-register))))

    (defadvice vc-annotate-show-diff-revision-at-line (around fix-hidden-details activate)
      "When the details are hidden, vc can't read the commit
info, because it is INVISIBLE TEXT!!! Why not, IDK, use a text property?"
      (if (memq 'vc-annotate-annotation buffer-invisibility-spec)
          (let ((old-buffer-invisibility-spec buffer-invisibility-spec)
                (cb (current-buffer)))
            (remove-from-invisibility-spec 'vc-annotate-annotation)
            (force-window-update (current-buffer))
            ad-do-it
            (with-current-buffer cb
              (setq buffer-invisibility-spec old-buffer-invisibility-spec)
              (force-window-update (current-buffer))))
        ad-do-it))

    (bind-key "t" 'my-svn-diff-branch-and-trunk vc-prefix-map)
    (bind-key "h" 'my-svn-diff-wc-and-trunk vc-prefix-map)))

(use-package visual-regexp
  :init
  (bind-keys :prefix "C-c v"
             :prefix-map ctl-c-v-map
             :prefix-docstring "Visual regexp map")
  :bind (("C-c v r" . vr/replace)
         ("C-c v q" . vr/query-replace)))

(use-package wc-mode
  :commands wc-mode)

(use-package web-mode
  :mode (("\\.tpl\\'" . web-mode)
         ("\\.twig\\'" . web-mode)
         ("\\.handlebars\\'" . web-mode)
         ("\\.hbs\\'" . web-mode)))

(use-package which-key
  :defer 2)

(use-package whitaker
  :commands whitaker
  :bind (("A-a" . whitaker-send-input)
         ("A-s" . whitaker-jump)))

(use-package whitespace
  :bind (("M-W" . whitespace-mode))
  :custom
  (whitespace-style
   (quote
    (face trailing tabs spaces lines-tail newline empty indentation space-after-tab space-before-tab space-mark tab-mark newline-mark))))

(use-package wiktionary-translate
  :bind ("<insert> <delete>" . wd-show-translation))

(use-package windmove
  :bind (("A-C-p" . windmove-up)
         ("A-C-;" . windmove-down)
         ("A-C-l" . windmove-left)
         ("A-C-'" . windmove-right)))

(use-package world-time-mode
  :bind ("C-. t" . world-time-list))

(use-package xref
  :defer t
  :config
  ;; these two extra methods are required to work with lsp-mode return
  ;; values
  (cl-defmethod xref-location-marker ((l xref-item))
    (with-slots (location) l
      (xref-location-marker location)))

  (cl-defmethod xref-location-group ((l xref-item))
    (with-slots (location) l
      (xref-location-group location))))

(use-package yaml-mode
  :mode (("\\.dockerapp\\'" . yaml-mode))
  :config
  (defun my-yaml-beginning-of-defun (&optional arg)
    (interactive)
    (let ((cc (current-indentation)))
      (while (and (or (save-excursion
                        (beginning-of-line)
                        (looking-at "[ \t]*\\($\\|#.*$\\)"))
                      (>= (current-indentation) cc))
                  (= 0 (forward-line -1))))
      (back-to-indentation)))

  (bind-key "C-c '" 'edit-indirect-region yaml-mode-map)
  (defun my-yaml-mode-init ()
    (setq-local beginning-of-defun-function 'my-yaml-beginning-of-defun)
    (smartparens-strict-mode 1)
    (font-lock-add-keywords nil '(("[^[:alnum:]]@\\_<\\(.*?\\)\\_>" 0 'font-lock-type-face)) 'append))
  (add-hook 'yaml-mode-hook 'my-yaml-mode-init))

(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-minor-mode
             yas-expand)
  :init
  (progn
    (autoload #'yas-hippie-try-expand "yasnippet"))
  :config
  (progn
    (defvar my-yas-snippet-parent-mode '((malabar-mode . java-mode))
      "An alist mapping major modes to their parents.

When creating a snippet with `my-yas-add-or-edit-snippet', use
the parent's directory.")

    (defun my-yas-add-or-edit-snippet (filename trigger)
      "Add snippet FILENAME triggered by TRIGGER to current major mode.

If such snippet already exists, just open it for editing."
      (interactive "sFilename: \nsSnippet trigger: ")
      ;; we only use one directory
      (find-file (concat (car yas-snippet-dirs) "/"
                         (symbol-name (or (cdr (assq major-mode my-yas-snippet-parent-mode))
                                          major-mode)) "/"
                         filename))
      (when (= (buffer-size) 0)
        (insert (format "# -*- coding: utf-8; mode: snippet -*-
# name: %s
# key: %s
# contributor: Matus Goljer <matus.goljer@gmail.com>
# --
" trigger trigger))
        (snippet-mode)))

    (defun my-yas-ucc ()
      "Get `car' of `kill-ring' and call `s-upper-camel-case' on it."
      (s-upper-camel-case (car kill-ring)))

    (defun my-yas-startup ()
      ;; stupid yasnippet :/ we define <tab> behaviour elsewhere
      (define-key yas-minor-mode-map [(tab)] nil)
      (define-key yas-minor-mode-map (kbd "TAB") nil)
      (define-key yas-minor-mode-map (kbd "C-c & C-a") 'my-yas-add-or-edit-snippet))

    ;; Replace yasnippets's TAB, was yas/expand
    (add-hook 'yas-minor-mode-hook 'my-yas-startup)))

(my-load-or-tangle (f-join (f-parent my-vendor-file) "vendor"))
(my-load-or-tangle (f-join (f-parent my-vendor-file) "keys"))

;; Local Variables:
;; eval: (add-to-list 'imenu-generic-expression '("Used Packages" "\\(^(use-package +\\)\\(\\_<.+\\_>\\)" 2))
;; End:

;;; vendor.el ends here
