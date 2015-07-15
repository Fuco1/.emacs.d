(use-package ag
  :commands (ag ag-regexp ag-files ag-project-dired ag-dired ag-dired-regexp)
  :init
  (progn
    (bind-key "<f9>"
              (defhydra f9-hydra (:color blue)
                "F9 hydra: ag"
                ("<f8>" ag-files "ag-files")
                ("<f9>" ag "ag")
                ("<f10>" ag-regexp "ag-regexp")
                ("7" ag-project-dired "ag-project-dired")
                ("8" ag-dired "ag-dired")
                ("9" ag-dired-regexp "ag-dired-regexp"))))
  :config
  (progn
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

(use-package autobookmarks
  :defer t
  :idle
  (progn
    (use-package autobookmarks)
    (autobookmarks-mode 1)))

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

(use-package bookmark+
  :defer t
  :init
  (progn
    (require 'bookmark+-autoloads)
    (autoload #'my-bmkp-tag-jump "bookmark+" nil t)
    (autoload #'my-bmkp-tag-dired "bookmark+" nil t)
    (bind-key "C-x j t t" 'my-bmkp-tag-jump)
    (bind-key "C-x j t d" 'my-bmkp-tag-dired))
  :config
  (progn
    (defun my-bmkp-tag-jump (tag)
      "Jump to bookmark that has TAG.

This is like `bmkp-some-tags-jump' but reads only one tag."
      (interactive (list (progn
                           (bookmark-maybe-load-default-file)
                           (completing-read "Tag: " (or bmkp-tags-alist (bmkp-tags-list)) nil t))))
      (let* ((alist (bmkp-some-tags-alist-only (list tag))))
        (unless alist (error "No bookmarks have any of the specified tags"))
        (bookmark-jump
         (bookmark-completing-read "Bookmark" (bmkp-default-bookmark-name alist) alist))))

    (defun my-bmkp-tag-dired (tags)
      "Dieplay a dired buffer containing all files tagged with TAGS."
      (interactive (list (bmkp-read-tags-completing)))
      (let* ((alist (bmkp-all-tags-alist-only tags))
             (files (-map 'f-canonical (--map (cdr (assoc 'filename it)) alist)))
             (common-parent (f-common-parent files))
             (files (--map (s-chop-prefix common-parent it) files))
             ;; the following two settings take care of dired bullshit
             (dired-buffers nil)
             (default-directory common-parent))
        (dired (cons (concat common-parent) files))
        (rename-buffer (with-temp-buffer
                         (insert "Tags")
                         (--each tags (insert ":") (insert it))
                         (insert ":" common-parent)
                         (buffer-string))
                       :uniquify)))

    (defadvice bookmark-jump (after fix-no-hack-local activate)
      (hack-local-variables))

    (bind-key "M-o" 'elwm-activate-window bookmark-bmenu-mode-map)
    ;; re-init the map after loading the package
    (bind-key "C-x j t t" 'my-bmkp-tag-jump)
    (bind-key "C-x j t d" 'my-bmkp-tag-dired)))

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
    (fset 'calc-one-minus [?1 return ?- ?n])
    (fset 'calc-vector-normalize [return ?A ?/])
    (bind-keys :map calc-mode-map
      ("C-. m" . calc-one-minus)
      ("C-. n" . calc-vector-normalize)
      ("C-<tab>" . calc-roll-up)
      ("<tab>" . calc-roll-down))
    (setq calc-display-trail nil)))

(use-package calendar
  :defer t
  :init
  (use-package slovak-holidays
    :init (slovak-holidays-add))
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

(use-package circe
  :commands circe
  :init
  (progn
    (autoload #'my-circe-open-irc-frame "circe" nil t)
    (setq circe-networks nil))
  :config
  (progn
    (tracking-mode 1)

    (defun my-circe-get-dasnet-irssi-passwd (_)
      (with-temp-buffer
        (insert-file-contents "~/secrets/dasnet-irssi-proxy")
        (buffer-string)))

    (setq lui-highlight-keywords
          '(("^--> .*" (face (:foreground "#4e9a06")))
            ;; specific nick highlights
            ("^<taylanub[`_]*>" (face (:foreground "#3465a4")))
            ("<queen[`_]*>" (face (:foreground "#e6a8df")))
            ("<jordigh[`_]*>" (face (:foreground "#e6a8df")))
            ("<fsbot[`_]*>" (face (:foreground "#41423f")))
            ("<rudybot[`_]*>" (face (:foreground "#41423f")))
            ("<rhemaxx0s[`_]*>" (face (:foreground "#8ae234")))
            ("<rhemax0s[`_]*>" (face (:foreground "#8ae234")))
            ("<rhemaxxos[`_]*>" (face (:foreground "#8ae234")))
            ("<rhemaxos[`_]*>" (face (:foreground "#8ae234")))
            ("<magnars[`_]*>" (face (:foreground "#5c3566")))
            ("<tanagoljerova[`_]*>" (face (:foreground "#ef2929")))
            ("<nicferrier[`_]*>" (face (:foreground "#ef2929")))
            ("<macrobat[`_]*>" (face (:foreground "#5c3566")))
            ("<ijp[`_]*>" (face (:foreground "#729fcf")))
            ("<johnw[`_]*>" (face (:foreground "#5c3566")))
            ("<godmy[`_]*>" (face (:foreground "#ef2929")))
            ("<lambdabot[`_]*>" (face (:foreground "#41423f")))
            ("<wgreenhouse[`_]*>" (face (:foreground "#8ae234")))
            ("<tali713[`_]*>" (face (:foreground "#8ae234")))
            ("<forcer[`_]*>" (face (:foreground "#4e9a06")))
            ("<tonitrus[`_]*>" (face (:foreground "#4e9a06")))
            ;; default nick
            ("^<.*?>" circe-originator-face)))

    (defun my-circe-after-colon (_ _ _)
      (save-excursion
        (backward-char 1)
        (looking-back ":")))

    (sp-with-modes 'circe-channel-mode
      (sp-local-pair "`" "'")
      (sp-local-pair "(" nil :unless '(:add my-circe-after-colon)))

    (add-hook 'circe-channel-mode-hook 'my-circe-channel-setup)
    (add-hook 'circe-query-mode-hook 'my-circe-channel-setup)
    (defun my-circe-channel-setup ()
      "Setup channel buffer."
      (my-init-text-based-modes)
      (smartparens-mode t)
      (set (make-local-variable 'sp-autoescape-string-quote) nil))

    (defvar my-lui-highlight-buffer "*Circe-Highlights*"
      "Name of the highlight buffer.")

    (defvar my-lui-highlight-filter (lambda ()
                                      (memq 'circe-highlight-nick-face
                                            (lui-faces-in-region (point-min)
                                                                 (point-max))))
      "A function used to filter messages which should go into highlight buffer.

Should return non-nil if we want to keep the message.

Called with zero argument in a buffer narrowed to the current
message.")

    (defun my-lui-highlight-filter ()
      "Return non-nil if we should keep the message."
      (and (memq 'circe-highlight-nick-face
                 (lui-faces-in-region (point-min)
                                      (point-max)))
           (not (text-property-any (point-min) (point-max)
                                   'lui-format 'circe-format-server-numeric))))

    (setq my-lui-highlight-filter 'my-lui-highlight-filter)

    (add-hook 'lui-post-output-hook 'my-lui-save-highlights)
    (defun my-lui-save-highlights ()
      (when (funcall my-lui-highlight-filter)
        (let ((buffer (buffer-name))
              (target circe-chat-target)
              (network (with-circe-server-buffer
                         circe-server-network))
              (text (buffer-substring (next-single-property-change (point-min) 'face) (point-max))))
          (with-current-buffer (get-buffer-create my-lui-highlight-buffer)
            (goto-char (point-max))
            (save-restriction
              (narrow-to-region (point) (point))
              (insert (propertize (format-time-string "[%Y-%m-%d %H:%M:%S]")
                                  'face 'lui-time-stamp-face)
                      " "
                      (propertize
                       (concat (or target buffer)
                               "@"
                               network)
                       'face '(:foreground "#8ae234"))
                      " "
                      (my-remove-text-properties-when
                       'face '(circe-highlight-nick-face)
                       0 (1- (length text))
                       '(face)
                       text))
              (lui-buttonize))))))

    (defun my-circe-open-irc-frame ()
      "Open an IRC frame."
      (interactive)
      (select-frame (make-frame-command))
      (set-frame-parameter (selected-frame) :frame-type :circe)
      (set-frame-parameter (selected-frame) 'name "Circe")
      (set-frame-parameter (selected-frame) 'explicit-name "Circe")
      (set-frame-parameter (selected-frame) 'background-color "#111111"))

    (defun my-circe-kill-all-irc-buffers ()
      "Kill all circe buffers."
      (interactive)
      (--each (buffer-list)
        (with-current-buffer it
          (when (eq major-mode 'circe-server-mode)
            (kill-buffer it)))))))

(use-package clippy
  :commands clippy-describe-function)

(use-package conf-mode
  :mode (("\\.pwm\\'" . conf-mode)))

(use-package csharp-mode
  :config
  (progn
    (bind-key "M-'" 'helm-etags-select csharp-mode-map)))

(use-package css-mode
  :defer t
  :config
  (progn
    (defun my-css-mode-setup ()
      (multi-web-mode 1)
      (emmet-mode 1))
    (add-hook 'css-mode-hook 'my-css-mode-setup)))

(use-package "cua-base"
  :defer t
  :config
  (progn
    (define-key cua--region-keymap [remap my-emacs-lisp-open-line] 'cua-replace-region)))

(use-package custom
  :pre-init
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
      (setq buffer-display-table (make-display-table))
      (aset buffer-display-table ?\^M []))
    (add-hook 'diff-mode-hook 'my-diff-mode-init)))

;; see commentary in dired-defs.el
(use-package dired
  :mode ("\\.wdired\\'" . my-virtual-dired-mode)
  :bind (("C-x d"  . my-dired-files)
         ("C-x C-j" . dired-jump))
  :init
  (progn
    (defvar my-virtual-dired-p nil
      "Non-nil if the buffer is virtual dired.")

    (defun my-virtual-dired-mode ()
      (save-excursion
        (goto-char (point-min))
        (back-to-indentation)
        (let* ((ddir (thing-at-point 'filename))
               (dired-buffers dired-buffers))
          (virtual-dired (substring ddir 0 (1- (length ddir))))
          (set-buffer-modified-p nil)
          (setq write-contents-functions 'dired-virtual-save-buffer)
          (set (make-local-variable 'my-virtual-dired-p) t))
        (goto-line 2)
        (let ((buffer-name (s-trim (thing-at-point 'line))))
          (dired-virtual-revert)
          (rename-buffer buffer-name))))

    (defun my-dired-files (&optional arg)
      "Like `ido-dired'.  With prefix argument call
`diredp-dired-files' with negative argument."
      (interactive "P")
      (if arg
          (progn
            (when (not (featurep 'icicles))
              (require 'icicles))
            (setq current-prefix-arg -1)
            (call-interactively 'diredp-dired-files))
        (ido-dired))))
  :config
  (progn
    (load "files/dired-defs")

    ;; overload to fix bullshit
    (defun dired-hack-local-variables () nil)))

(use-package dired-list
  :commands (dired-list-find-file dired-list-grep))

(use-package dired-tagsistant
  :pre-init
  (bind-keys :prefix-map ctl-x-t-map
             :prefix "C-x T"
             :prefix-docstring "C-x T prefix map")
  :bind (("C-x T r" . dired-tagsistant-add-relation)
         ("C-x T +" . dired-tagsistant-some-tags)
         ("C-x T *" . dired-tagsistant-all-tags)
         ("C-x T % +" . dired-tagsistant-some-tags-regexp)
         ("C-x T % *" . dired-tagsistant-all-tags-regexp)))

(use-package ediff
  :pre-init
  (progn
    (defvar ctl-dot-equals-prefix-map)
    (define-prefix-command 'ctl-dot-equals-prefix-map)
    (bind-key "C-. =" 'ctl-dot-equals-prefix-map))
  :bind (("C-. = b" . ediff-buffers)
         ("C-. = B" . ediff-buffers3)
         ("C-. = =" . ediff-files)
         ("C-. = f" . ediff-files)
         ("C-. = F" . ediff-files3)
         ("C-. = r" . ediff-revision)
         ("C-. = t" . my-svn-ediff-branch-and-trunk)
         ("C-. = p" . ediff-patch-file)
         ("C-. = P" . ediff-patch-buffer)
         ("C-. = l" . ediff-regions-linewise)
         ("C-. = w" . ediff-regions-wordwise))
  :config
  (progn
    (defhydra hydra-ediff (:color blue :hint nil)
      "
^Buffers           Files           VC                     Ediff regions
----------------------------------------------------------------------
_b_uffers           _f_iles (_=_)       _r_evisions              _l_inewise
_B_uffers (3-way)   _F_iles (3-way)   _t_runk against branch   _w_ordwise
"
      ("b" ediff-buffers)
      ("B" ediff-buffers3)
      ("=" ediff-files)
      ("f" ediff-files)
      ("F" ediff-files3)
      ("r" ediff-revision)
      ("t" my-svn-ediff-branch-and-trunk)
      ("l" ediff-regions-linewise)
      ("w" ediff-regions-wordwise))
    (defvar my-ediff-before-config nil "Window configuration before ediff.")
    (defvar my-ediff-after-config nil "Window configuration after ediff.")

    (defun my-ediff-before-setup ()
      "Function to be called before any buffers or window setup for
    ediff."
      (setq my-ediff-before-config (current-window-configuration)))

    (defun my-ediff-after-setup ()
      "Function to be called after buffers and window setup for ediff."
      (setq my-ediff-after-config (current-window-configuration)))

    (defun my-ediff-quit ()
      "Function to be called when ediff quits."
      (when my-ediff-before-config
        (set-window-configuration my-ediff-before-config))
      ;; clean up ediff bullshit
      (->> (buffer-list)
        (-map 'buffer-name)
        (--select (string-match-p "\\*[Ee]diff" it))
        (-map 'kill-buffer)))

    (add-hook 'ediff-before-setup-hook 'my-ediff-before-setup)
    (add-hook 'ediff-after-setup-windows-hook 'my-ediff-after-setup 'append)
    (add-hook 'ediff-quit-hook 'my-ediff-quit)))

(use-package elfeed
  :if (member (my-where-am-i) '("home" "brno"))
  :bind (("C-. C-f" . elfeed))
  :idle
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

    (defadvice elfeed-tag (before update-unread activate)
      (when (and (not (member 'unread (elfeed-entry-tags (ad-get-arg 0))))
                 (member 'unread (ad-get-args 1)))
        (cl-incf my-elfeed-unread-count)))

    (defadvice elfeed-untag (before update-unread activate)
      (when (and (member 'unread (elfeed-entry-tags (ad-get-arg 0)))
                 (member 'unread (ad-get-args 1)))
        (cl-decf my-elfeed-unread-count)))

    (bind-keys :map elfeed-show-mode-map
      ("M-n" . shr-next-link)
      ("M-p" . shr-previous-link))))

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
         ("C-x C-2" . elwm-split-window))
  :init
  (progn
    (add-hook 'dired-mode-hook (lambda () (bind-key "M-o" 'elwm-activate-window dired-mode-map)))
    (add-hook 'diff-mode-hook (lambda () (bind-key "M-o" 'elwm-activate-window diff-mode-map)))))

(use-package emmet-mode
  :defer t
  :diminish emmet-mode)

(use-package eshell
  :commands eshell
  :config
  (progn
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
    (load "files/eshell-defs")))

(use-package ess
  :defer t
  :config
  (progn
    (defun my-ess-post-run-hook ()
      (modify-syntax-entry ?$ ".")
      (ess-execute-screen-options))
    (add-hook 'ess-post-run-hook 'my-ess-post-run-hook)))

(use-package expand-region
  :bind ("s-'" . er/expand-region))

(use-package god-mode
  :defer t
  :config
  (progn
    (defvar my-god-mode-buffer-input-method nil
      "Input method of the buffer when god-mode is disabled.")
    (make-variable-buffer-local 'my-god-mode-buffer-input-method)
    (defun my-update-cursor ()
      (set-cursor-color (if (or god-local-mode buffer-read-only)
                            "#ef2929"
                          "#fce94f")))
    (defun my-god-mode-init ()
      (setq my-god-mode-buffer-input-method current-input-method)
      (deactivate-input-method)
      (my-update-cursor))
    (defun my-god-mode-deinit ()
      (when my-god-mode-buffer-input-method
        (set-input-method my-god-mode-buffer-input-method))
      (my-update-cursor))
    (add-hook 'god-mode-enabled-hook 'my-god-mode-init)
    (add-hook 'god-mode-disabled-hook 'my-god-mode-deinit)))

(use-package ggtags-mode
  :disabled t ;disabled in favour of helm-gtags
  :bind (("M-'" . ggtags-find-tag-dwim))
  :config
  (progn
    (unbind-key "M-o" ggtags-navigation-map)
    (unbind-key "M-." ggtags-mode-map)
    (defun my-ggtags-navigation-next-file (n)
      (interactive "p")
      (ggtags-ensure-global-buffer
        (compilation-next-file n)
        (ggtags-global-next-error-function)))
    (defun my-ggtags-navigation-previous-file (n)
      (interactive "p")
      (my-ggtags-navigation-next-file (- n)))
    (bind-keys :map ggtags-navigation-map
      ("n" . my-ggtags-navigation-next-file)
      ("p" . my-ggtags-navigation-previous-file)
      ("RET" . compile-goto-error)
      ("<return>" . compile-goto-error)
      ("q" . ggtags-navigation-mode-done))
    (defun my-ggtags-global-mode-init ()
      (visible-mode 1))
    (add-hook 'ggtags-global-mode-hook 'my-ggtags-global-mode-init)))

;; disable for now and write a better, cleaner solution WITHOUT using
;; the idiotic hooks
(use-package golden-ratio
  :diminish golden-ratio-mode
  :disabled t
  :config
  (progn
    (defun my-golden-ratio-inhibit ()
      (or (--any? (string-match-p "\\*Ediff Control Panel" it)
                  (mapcar 'buffer-name (mapcar 'window-buffer (window-list))))))
    (defun my-golden-ratio ()
      "Run `golden-ratio' if `golden-ratio-mode' is enabled."
      (interactive)
      (when golden-ratio-mode
        (golden-ratio)))

    (use-package guide-key
      :config
      (progn
        (defadvice guide-key/popup-guide-buffer (around fix-golden-ratio activate)
          (when (featurep 'golden-ratio) (golden-ratio-mode -1))
          ad-do-it
          (when (featurep 'golden-ratio) (golden-ratio-mode 1)))))

    (use-package ispell
      :config
      (progn
        (defadvice ispell-word (around fix-golden-ratio activate)
          (when (featurep 'golden-ratio) (golden-ratio-mode -1))
          ad-do-it
          (when (featurep 'golden-ratio) (golden-ratio-mode 1)))))

    (defadvice quit-window (around fix-golden-ratio activate)
      ad-do-it
      (my-golden-ratio))))

(use-package google-maps
  :commands google-maps)

(use-package google-this
  :defer t
  :init
  (progn
    (defvar google-this-mode-submap)
    (define-prefix-command 'google-this-mode-submap)
    (define-key google-this-mode-submap [return] 'google-search)
    (define-key google-this-mode-submap " " 'google-region)
    (define-key google-this-mode-submap "t" 'google-this)
    (define-key google-this-mode-submap "g" 'google-lucky-search)
    (define-key google-this-mode-submap "i" 'google-lucky-and-insert-url)
    (define-key google-this-mode-submap "w" 'google-word)
    (define-key google-this-mode-submap "s" 'google-symbol)
    (define-key google-this-mode-submap "l" 'google-line)
    (define-key google-this-mode-submap "e" 'google-error)
    (define-key google-this-mode-submap "f" 'google-forecast)
    (define-key google-this-mode-submap "r" 'google-cpp-reference)
    (define-key google-this-mode-submap "m" 'google-maps)
    ;; "c" is for "convert language" :-P
    (define-key google-this-mode-submap "c" 'google-translate-query-or-region)
    (bind-key "C-x g" google-this-mode-submap)))

(use-package grep
  :bind (("<f7> <f7>" . rgrep))
  :config
  (progn
    (use-package scf-mode)
    (bind-key "s" 'scf-mode grep-mode-map)
    (defun my-grep-mode-init ()
      (scf-mode 1))
    (add-hook 'grep-mode-hook 'my-grep-mode-init)))

(use-package guide-key
  :defer t
  :diminish guide-key-mode)

(use-package flycheck
  :commands flycheck-mode
  :config
  (progn
    (use-package flycheck-phplint)))

(use-package fold-this
  :bind (("C-c C-v f" . fold-this)))

(use-package free-keys
  :commands free-keys)

(use-package haskell-mode
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.chs\\'" . haskell-mode))
  :config
  (progn
    (use-package hs-lint
      :config
      (progn
        (bind-key "C-c i" 'hs-lint haskell-mode-map)))
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
      (set (make-local-variable 'end-of-defun-function) 'my-hs-end-of-defun)
      (set (make-local-variable 'beginning-of-defun-function) 'my-hs-beg-of-defun))
    (add-hook 'haskell-mode-hook 'my-haskell-init)

    (defun my-inferior-haskell-init ()
      (smartparens-mode 1))
    (add-hook 'inferior-haskell-mode-hook 'my-inferior-haskell-init)))

(use-package helm
  :defer t
  :init
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
      ("o h" . helm-org-in-buffer-headings))
    (use-package helm-gtags
      :bind (("M-'" . helm-gtags-dwim)
             ("C-M-'" . helm-gtags-pop-stack)))))

(use-package help-mode
  :defer t
  :config
  (progn
    (defun my-help-mode-init ()
      (use-package better-jump))
    (add-hook 'help-mode-hook 'my-help-mode-init)
    (bind-key "<tab>" 'forward-button help-mode-map)
    (bind-key "l" 'help-go-back help-mode-map)))

(use-package highlight-thing
  :commands highlight-thing-mode)

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
    (load "files/ibuffer-defs")))

(use-package ido
  :defer t
  :bind (("M-." . ido-goto-symbol)) ;; was Find tag
  :config
  (progn
    (load "files/ido-defs")))

(use-package info
  :defer t
  :config
  (defun my-info-mode-init ()
    (use-package better-jump))
  (add-hook 'info-mode-hook 'my-info-mode-init))

(use-package "isearch"
  :bind (("C-s" . isearch-forward-regexp)
         ("C-r" . isearch-backward-regexp))
  :config
  (progn
    (load "files/isearch-defs")))

(use-package ispell
  :bind (("<f10>" . ispell-word)
         ("C-<f10>" . flyspell-mode))
  :init
  (progn
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

(use-package js
  :defer t
  :config
  (progn
    (defun my-js-mode-init ()
      (when (string-match-p "conkeror" (buffer-file-name))
        (conkeror-minor-mode 1))
      (multi-web-mode 1))
    (add-hook 'js-mode-hook 'my-js-mode-init)
    (defun my-js-disable-multi-web-mode ()
      "Set current buffer to `js-mode' and disable `multi-web-mode'."
      (interactive)
      (js-mode)
      (multi-web-mode -1)
      (setq indent-region-function nil))))

(use-package jump-char
  :bind (("M-m" . jump-char-forward)))

(use-package keyadvice
  :defer t
  :init (progn (load "projects/keyadvice.el/autoloads.el")))

(use-package keyfreq
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

(use-package lisp-mode
  :defer t
  :init
  (progn
    (use-package letcheck
      :commands letcheck-mode)
    (defun my-emacs-lisp-init ()
      (bind-keys :map emacs-lisp-mode-map
        ("<return>" . my-emacs-lisp-open-line)
        ("C-M-;" . clippy-describe-function)
        ("C-. ." . my-describe-thing-in-buffer))
      (bind-key "C-x C-d"
                (defhydra hydra-elisp-refactor (:color blue)
                  "Refactor"
                  ("l" my-extract-to-let "extract to let")
                  ("m" my-merge-let-forms "merge let forms")
                  ("c" my-lisp-if-to-cond "if => cond")
                  ("i" my-lisp-cond-to-if "cond => if"))
                emacs-lisp-mode-map)
      (unbind-key "C-x C-a" emacs-lisp-mode-map)
      (set-input-method "english-prog")
      (eldoc-mode 1)
      (letcheck-mode t)
      (setq completion-at-point-functions nil))
    (add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-init))
  :config
  (progn
    (load "files/lisp-mode-defs")))

(use-package magit
  :pre-init
  (bind-keys :prefix "C-c m"
             :prefix-map ctl-c-m-map
             :prefix-docstring "Magit map")
  :bind (("C-c m b" . magit-key-mode-popup-branching)
         ("C-c m c" . magit-key-mode-popup-committing)
         ("C-c m d" . magit-key-mode-popup-dispatch)
         ("C-c m f" . magit-key-mode-popup-fetching)
         ("C-c m i" . magit-key-mode-popup-diff-options)
         ("C-c m l" . magit-key-mode-popup-logging)
         ("C-c m m" . magit-key-mode-popup-merging)
         ("C-c m p" . magit-key-mode-popup-pushing)
         ("C-c m v" . magit-branch-manager)
         ("C-c m s" . magit-status))
  :config
  (progn
    (bind-key "<tab>" 'magit-toggle-section magit-mode-map)
    (require 'flyspell)
    (diminish 'magit-auto-revert-mode)))

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
  :mode ("\\.md$" . gfm-mode)
  :config
  (progn
    (load "files/markdown-defs")))

(use-package message
  :defer t
  :init
  (progn
    (add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime))
  :config
  (progn
    (use-package notmuch)
    (use-package smtpmail)
    (use-package gnus-alias
      :init
      (progn
        (gnus-alias-init)
        (bind-key "C-c i" 'gnus-alias-select-identity message-mode-map)
        (defun my-message-gnus-alias-init ()
          (when (equal (my-where-am-i) "logio")
            (run-with-timer 0.2 nil (lambda () (gnus-alias-use-identity "logio")))))
        (add-hook 'message-setup-hook 'my-message-gnus-alias-init)))))

(use-package multi-web-mode
  :defer t
  :config
  (progn
    (setq mweb-tags '((php-mode "<\\?php" "\\?>")
                      (js-mode "<script[^>]*>" "</script>")
                      (css-mode "<style[^>]*>" "</style>")))
    (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))

    ;; redefined to properly re-set indentation functions
    (defun mweb-disable ()
      "Disable the minor mode."
      (assq-delete-all 'multi-web-mode minor-mode-map-alist)
      (remove-hook 'post-command-hook 'mweb-post-command-hook)
      (cond
       ((eq major-mode 'php-mode)
        (setq indent-line-function 'php-cautious-indent-line)
        (setq indent-region-function 'php-cautious-indent-region))))))

(use-package neon-mode
  :mode ("\\.neon\\'" . neon-mode))

(use-package notmuch
  :bind (("C-. C-n" . notmuch))
  :init
  (progn
    (autoload #'my-notmuch-unread "notmuch" nil t)
    (autoload #'my-notmuch-inbox "notmuch" nil t)
    (bind-keys :map ctl-dot-prefix-map
      ("C-u" . my-notmuch-unread)
      ("TAB" . my-notmuch-inbox)
      ("C-a" . my-notmuch-archived)))
  :idle
  (progn
    (defun my-notmuch-update-mail ()
      (interactive)
      (set-process-sentinel
       (start-process "mail-update" nil "/bin/bash" "/home/matus/bin/run-getmail")
       'my-notmuch-update-sentinel))

    (defun my-notmuch-update-sentinel (proc state)
      (when (equal state "finished\n")
        (message "Mail update: %s" (format-time-string "%B %e %Y %H:%M:%S %Z"))
        (require 'notmuch)))

    (defvar my-notmuch-update-mail-timer
      (run-with-timer 10 400 'my-notmuch-update-mail)
      "Mail update timer."))
  :config
  (progn
    (use-package notmuch-unread
      :config
      (progn
        ;; REDEFINED FROM notmuch-unread-mode
        ;; Don't show anything if there's no unread mail
        (defun notmuch-unread-update-handler ()
          "Update the mode line."
          (let ((count (notmuch-unread-count)))
            (if (> count 0)
                (setq notmuch-unread-mode-line-string
                      (format " [✉ %d]" count))
              (setq notmuch-unread-mode-line-string "")))
          (force-mode-line-update))))

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

    (bind-key "RET" 'goto-address-at-point goto-address-highlight-keymap)
    (bind-key "d" 'my-notmuch-delete-mail notmuch-show-mode-map)
    (bind-key "d" 'my-notmuch-delete-mail notmuch-search-mode-map)
    (bind-key "g" 'notmuch-poll-and-refresh-this-buffer notmuch-search-mode-map)))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  ;; The following lines are always needed.  Choose your own keys.
  :bind  (("C-c l" . org-store-link)
          ("<f12>" . org-agenda)
          ("C-<f12>" . org-agenda-time-limit)
          ("C-M-r" . org-capture)
          ("C-c C-x C-j" . org-clock-goto)
          ("C-c C-x C-o" . org-clock-out)
          ("C-c C-x <C-i-key>" . org-clock-in))
  :config
  (progn
    (load "files/org-defs.el")))

;; TODO: move into a separate file
(use-package php-mode
  :mode ("\\.php[st]?\\'" . my-php-disable-multi-web-mode)
  :config
  (progn
    (use-package better-jump)
    (use-package php-eldoc)
    (use-package which-func)

    ;; imenu for instance variables
    (defun php-create-regexp-for-instance-variable (visibility)
      (concat
       "^\\s-*" visibility
       ;; Static?
       "\\s-+\\(?:static\\s-+\\)?"
       ;; Capture name
       "\\$\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*\\(?:;\\|=\\)"))

    (defconst my-php-public-variables "Public Instance Variables")
    (defconst my-php-protected-variables "Protected Instance Variables")
    (defconst my-php-private-variables "Private Instance Variables")

    (add-to-list 'php-imenu-generic-expression `(,my-php-public-variables ,(php-create-regexp-for-instance-variable "public") 1))
    (add-to-list 'php-imenu-generic-expression `(,my-php-protected-variables  ,(php-create-regexp-for-instance-variable "protected") 1))
    (add-to-list 'php-imenu-generic-expression `(,my-php-private-variables ,(php-create-regexp-for-instance-variable "private") 1))

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
            (ggtags-eldoc-function))))

    (defun my-php-find-project-root ()
      (with-temp-buffer
        (shell-command "global -p" (current-buffer))
        (s-trim (buffer-string))))

    (defun my-php-local-file-name (filename)
      "Get local part of file name.

Works both on local files and tramp files (where it cuts off the
network prefix)."
      (when filename
        (if (file-remote-p filename)
            (tramp-file-name-localname (tramp-dissect-file-name filename))
          filename)))

    (defun my-php-run-tests ()
      "Run all Nette tests found in current directory."
      (interactive)
      (let* ((root (my-php-find-project-root))
             (tester-dir (concat root "/composer/vendor/bin/"))
             (dir (my-php-local-file-name default-directory)))
        (when (buffer-modified-p) (save-buffer))
        (let ((cmd (format "php %stester -c %s/tools/tester/php.ini '%s'" tester-dir root dir)))
          (async-shell-command cmd))))
    (bind-key "C-c C-c" 'my-php-run-tests php-mode-map)

    (defun my-php-switch-to-test ()
      "Switch to corresponding unit test.

Unit tests are specified by .unit.phpt extension.

If already in a unit test, go to source."
      (interactive)
      (let ((other-file
             (if (string-match-p "\\.php\\'" (buffer-file-name))
                 (replace-regexp-in-string "\\.php\\'" ".unit.phpt" (buffer-file-name))
               (replace-regexp-in-string "\\.unit\\.phpt\\'" ".php" (buffer-file-name))))
            (func (which-function)))
        (find-file other-file)))
    (bind-key "C-c C-t" 'my-php-switch-to-test php-mode-map)

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

    ;; TODO: get the types as well as the names... maybe use an actual
    ;; parser for this?
    (defun my-php-get-function-args (&optional name)
      "Return all arguments of php function.

Point should be at the line containing `function'."
      (save-excursion
        (when name
          (goto-char (point-min))
          (unless (search-forward (concat "function " name) nil t)
            (error "Function %s does not exist" name)))
        (let ((function-args (sp-get (sp-down-sexp)
                               (buffer-substring-no-properties :beg :end)))
              (args nil))
          (save-match-data
            (with-temp-buffer
              (insert function-args)
              (goto-char (point-min))
              (while (re-search-forward "\\(&?\\$.*?\\)[ \n\t,)]" nil t)
                (push (match-string 1) args))))
          (nreverse args))))

    (defun my-php-implement-constructor ()
      "Implement constructor.

This assings all variables in the argument list to instance
variables of the same name."
      (interactive)
      (let ((args (my-php-get-function-args)))
        (sp-restrict-to-pairs (list "{" "}") 'sp-down-sexp)
        (forward-line)
        (--each args
          (insert (format "$this->%s = %s;"
                          (replace-regexp-in-string "[&$]" "" it)
                          (replace-regexp-in-string "[&]" "" it)))
          (indent-according-to-mode)
          (insert "\n"))
        (delete-char -1)))
    (bind-key "C-x C-d c" 'my-php-implement-constructor php-mode-map)

    (defun my-php-add-private-variables-for-constructor-arguments ()
      "Generate private variable definitions for constructor arguments."
      (interactive)
      (-when-let (args (save-excursion
                         (goto-char (point-min))
                         (when (search-forward "__construct" nil t)
                           (my-php-get-function-args))))
        (let ((beg (point)))
          (--each args
            (insert (format "/** @var */\n private $%s;\n\n"
                            ;; TODO: abstract this cleanup
                            (replace-regexp-in-string "[&$]" "" it)
                            (replace-regexp-in-string "[&]" "" it))))
          (delete-char -2)
          (indent-region beg (point)))))

    (defun my-php-get-instance-variables ()
      "Return all instance variables.

These are retrieved from `imenu--index-alist'."
      (unless imenu--index-alist
        (imenu--make-index-alist))
      (-map
       'car
       (-concat
        (cdr (assoc my-php-public-variables imenu--index-alist))
        (cdr (assoc my-php-protected-variables imenu--index-alist))
        (cdr (assoc my-php-private-variables imenu--index-alist)))))

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
    (bind-key "C-x C-d p" 'my-php-implement-proxy-function-call php-mode-map)

    (defun my-php-run-codesniffer ()
      "Run phpcs(1) on file associated with current buffer."
      (interactive)
      (let ((file (my-php-local-file-name (buffer-file-name))))
        (async-shell-command (format "phpcs --standard=PW %s" file))))
    (bind-key "C-x C-d s" 'my-php-run-codesniffer php-mode-map)

    (defun my-php-disable-multi-web-mode ()
      "Set current buffer to `php-mode' and disable `multi-web-mode'."
      (interactive)
      (php-mode)
      (multi-web-mode -1))

    (defvar my-php-wrap-with-profiler-call-history nil
      "History for profiler name.")

    (defun my-php-wrap-with-profiler-call (profiler name scope)
      "Wrap active region with call to utils-profiler.

PROFILER is the name of the php instance variable containing the
profiler.

NAME is the name of the profiled block.

SCOPE is the scope, one of: batch, thread, plid."
      (interactive (list (let ((possible (--filter (string-match-p "profiler" it) (my-php-get-instance-variables))))
                           (if (= 1 (length possible))
                               (car possible)
                             (completing-read "Profiler to use: "
                                              possible nil nil
                                              (car my-php-wrap-with-profiler-call-history)
                                              'my-php-wrap-with-profiler-call-history)))
                         (read-from-minibuffer "Name of the profiled block: "
                                               (let ((str (buffer-substring-no-properties (region-beginning) (region-end))))
                                                 (with-temp-buffer
                                                   (insert str)
                                                   (goto-char (point-min))
                                                   (if (re-search-forward "->\\(.*?\\)(" nil t)
                                                       (s-dashed-words (match-string 1))
                                                     ""))))
                         (read-from-minibuffer "Scope of the profiled block: " "plid" nil nil nil "plid")))
      (when (use-region-p)
        (let ((b (region-beginning))
              (e (region-end))
              (format (format "$this->%s->%%s('%s', __METHOD__, '%s');" profiler name scope)))
          (goto-char e)
          (unless (= 0 (length (s-trim (thing-at-point 'line))))
            (newline))
          (insert (format format "stop"))
          (goto-char b)
          (unless (= 0 (length (s-trim (thing-at-point 'line))))
            (newline)
            (forward-line -1))
          (insert (format format "start"))
          (indent-region b (+ e (* 2 (length format)))))))
    (bind-key "C-x C-d C-p" 'my-php-wrap-with-profiler-call php-mode-map)

    (defun my-php-goto-specific ()
      (interactive)
      (let* ((tramp-prefix (if (tramp-tramp-file-p (buffer-file-name))
                               (-let (([method _ host] (tramp-dissect-file-name (buffer-file-name))))
                                 (concat "/" method ":" host ":"))
                             ""))
             (root (with-temp-buffer
                     (shell-command "global -r -p" t)
                     (s-trim (buffer-string))))
             (specific-file (concat tramp-prefix root "/specific/settings/default/default.pwm"))
             (project (with-temp-buffer
                        ;; TODO: write .pwm parser?
                        (insert-file-contents specific-file)
                        (goto-char (point-min))
                        (when (re-search-forward "project: +\\(.*\\)")
                          (match-string 1))))
             (module-name (f-base (buffer-file-name)))
             (specific-module-name (concat module-name "-" project)))
        (when project
          (find-file (concat tramp-prefix root "/specific/source/"
                             project "/extensions/" specific-module-name
                             "/" specific-module-name ".php")))))
    (bind-key "C-x C-d C-s" 'my-php-goto-specific php-mode-map)


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

    (defun my-php-mode-init ()
      (bind-key "<tab>" 'smart-tab php-mode-map)
      (c-set-style "php")
      (setq-local ggtags-get-definition-function 'my-php-ggtags-get-definition)
      (setq-local eldoc-documentation-function 'my-php-eldoc-function)
      (setq-local compile-command (concat "php -l " (my-php-local-file-name buffer-file-name)))
      (multi-web-mode 1)
      (eldoc-mode 1))
    (add-hook 'php-mode-hook 'my-php-mode-init)))

(use-package popwin
  :commands popwin-mode
  :config
  (progn
    (push '("*Pp Eval Output*" :height 15) popwin:special-display-config)))

(use-package projectile
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

(use-package quail
  :config
  (progn
    (define-key quail-simple-translation-keymap (kbd "C-SPC") 'quail-select-current)
    (define-key quail-simple-translation-keymap "\C-f" 'quail-next-translation)
    (define-key quail-simple-translation-keymap "\C-b" 'quail-prev-translation)

    (defun my-noninteractive-toggle-input-method (&rest _ignored)
      (toggle-input-method)
      (throw 'quail-tag nil))

    (load "files/layouts")))

(use-package recentf
  :disabled t
  :defer t
  :config
  (progn
    (defvar my-recentf-autosave-timer (run-with-timer 500 500 'recentf-save-list))))

(use-package revbufs
  :bind ("C-<f5>" . revbufs))

(use-package sallet
  :bind (("C-. C-." . helm-occur)
         ("C-'" . sallet-buffer))
  :config
  (progn
    (use-package autobookmarks)))

(use-package sgml-mode
  :defer t
  :config
  (progn
    (defadvice sgml-delete-tag (after reindent-buffer activate)
      (cleanup-buffer))

    (defun my-html-mode-setup ()
      (multi-web-mode 1)
      (emmet-mode 1)
      (smartparens-mode 1)
      (bind-keys :map html-mode-map
        ("C-c C-f" . sp-html-next-tag)
        ("C-c C-b" . sp-html-previous-tag)))
    (add-hook 'html-mode-hook 'my-html-mode-setup)))

(use-package shell-mode
  :defer t
  :config
  (progn
    (defun my-shell-mode-init ()
      (setq tab-width 8))
    (add-hook 'shell-mode-hook 'my-shell-mode-init)))

(use-package skeleton-complete
  :commands skeleton-complete-mode
  :diminish skeleton-complete-mode)

(use-package smartparens
  :defer t
  :diminish smartparens-mode
  :init
  (progn
    (load "~/.emacs.d/files/smartparens")))

(use-package smartscan
  :bind (("C->" . smartscan-symbol-go-forward)
         ("C-<" . smartscan-symbol-go-backward)))

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
      (sql-set-product "mysql"))
    (add-hook 'sql-mode-hook 'my-sql-mode-init)

    (defun my-sql-interactive-mode-init ()
      (orgtbl-mode))
    (add-hook 'sql-interactive-mode-hook 'my-sql-interactive-mode-init)))

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
    (load "files/latex-defs")))

(use-package tramp
  :defer t
  :config
  (progn
    ;; (setq tramp-default-method "plinkx")
    ;; (setq tramp-terminal-type "dumb")

    ;; in file `tramp-sh.el' it is necessary to add
    ;; (tramp-password-end-of-line "xy") ;see docstring for "xy"
    ;; to "plinkx" method.
    ))

(use-package transpose-frame
  :init
  (progn
    (defun transpose-frame-reverse (&optional frame)
      "Transpose windows arrangement at FRAME, flopping first..
Omitting FRAME means currently selected frame."
      (interactive)
      (transpose-frame-set-arrangement (transpose-frame-get-arrangement frame) frame
                                       'transpose 'flop 'flip)
      (if (interactive-p) (recenter)))))

(use-package two-column
  :defer t
  :config
  (progn
    (defadvice 2C-dissociate (after close-window-after-disconnect activate)
      (delete-window))))

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

    (bind-key "t" 'my-svn-diff-branch-and-trunk vc-prefix-map)))

(use-package visual-regexp
  :pre-init
  (bind-keys :prefix "C-c v"
             :prefix-map ctl-c-v-map
             :prefix-docstring "Visual regexp map")
  :bind (("C-c v r" . vr/replace)
         ("C-c v q" . vr/query-replace)))

(use-package wc-mode
  :commands wc-mode)

(use-package whitaker
  :commands whitaker
  :bind (("A-a" . whitaker-send-input)
         ("A-s" . whitaker-jump)))

(use-package wiktionary-translate
  :bind ("<insert> <delete>" . wd-show-translation))

(use-package windmove
  :bind (("A-C-p" . windmove-up)
         ("A-C-;" . windmove-down)
         ("A-C-l" . windmove-left)
         ("A-C-'" . windmove-right)))

(use-package world-time-mode
  :bind ("C-. t" . world-time-list))

(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-minor-mode
             yas-expand)
  :init
  (progn
    (autoload #'yas/hippie-try-expand "yasnippet"))
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
                         filename ".yasnippet"))
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


;; Local Variables:
;; eval: (add-to-list 'imenu-generic-expression '("Used Packages" "\\(^(use-package +\\)\\(\\_<.+\\_>\\)" 2))
;; End:
