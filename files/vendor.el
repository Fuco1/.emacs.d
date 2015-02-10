(use-package ag
  :pre-init
  (progn
    (defvar f9-ag-prefix-map)
    (define-prefix-command 'f9-ag-prefix-map)
    (bind-key "<f9>" 'f9-ag-prefix-map))
  :bind (("<f9> <f9>" . ag)
         ("<f9> <f10>" . ag-regexp)
         ("<f9> <f8>" . ag-files)
         ("M-<f2>" . ag-project-dired)
         ("C-<f2>" . ag-dired)
         ("A-<f2>" . ag-dired-regexp))
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

(use-package better-jump
  :bind (("C-\\" . bjump-word-jump)
         ("A-l" . bjump-char-jump-line)
         ("A-;" . bjump-word-jump-paragraph)
         ("s-o" . bjump-window-jump)
         ("A-j" . bjump-window-jump)
         ("A-k" . bjump-window-delete))
  :config
  (progn
    (define-key Info-mode-map "o" 'bjump-info-link-jump)
    (define-key help-mode-map "o" 'bjump-help-link-jump)))

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

    (bind-key "M-o" 'elwm-activate-window bookmark-bmenu-mode-map)
    ;; re-init the map after loading the package
    (bind-key "C-x j t t" 'my-bmkp-tag-jump)
    (bind-key "C-x j t d" 'my-bmkp-tag-dired)))

(use-package c-mode
  :defer t
  :config
  (progn
    (defun my-c-mode-setup ()
      (c-set-style "stroustrup"))
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
    (bind-key "C-. m" 'calc-one-minus calc-mode-map)
    (bind-key "C-. n" 'calc-vector-normalize calc-mode-map)
    (bind-key "C-<tab>" 'calc-roll-up calc-mode-map)
    (setq calc-display-trail nil)))

(use-package calendar
  :defer t
  :init
  (use-package slovak-holidays
    :init (slovak-holidays-add))
  :config
  (progn
    (defadvice calendar-exit (around close-window activate)
      (let* ((wl (window-list))
             (cb (calendar-buffer-list))
             (wins-to-kill (mapcar (lambda (w) (cons (member (window-buffer w) cb) w)) wl)))
        ad-do-it
        (mapc (lambda (w) (when (car w) (delete-window (cdr w)))) wins-to-kill)))))

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

    ;; TODO: write a macro to fontify nicks
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

    ;; TODO: napisat "go to mention" ktore skoci na miesto kde nastal highlight
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
              ;; TODO: abstract this into a formatter
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
  :bind (("<f7> <f8>" . dired-list-find-file)))

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
         ("C-. = p" . ediff-patch-file)
         ("C-. = P" . ediff-patch-buffer)
         ("C-. = l" . ediff-regions-linewise)
         ("C-. = w" . ediff-regions-wordwise))
  :config
  (progn
    (defvar my-ediff-before-config nil "Window configuration before ediff.")
    (defvar my-ediff-after-config nil "Window configuration after ediff.")

    (defun my-ediff-before-setup ()
      "Function to be called before any buffers or window setup for
    ediff."
      (setq my-ediff-before-config (current-window-configuration))
      (set-register ?b (list my-ediff-before-config (point-marker))))

    (defun my-ediff-after-setup ()
      "Function to be called after buffers and window setup for ediff."
      (setq my-ediff-after-config (current-window-configuration))
      (set-register ?e (list my-ediff-after-config (point-marker))))

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
    (load "files/eshell-defs")))

(use-package expand-region
  :bind ("s-'" . er/expand-region))

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
  :bind (("<f7> <f7>" . rgrep)))

(use-package guide-key
  :defer t
  :diminish guide-key-mode)

(use-package find-dired
  :bind (("<f7> <f8>" . find-dired)
         ("<f7> <f9>" . find-grep-dired)))

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

    (add-hook 'haskell-mode-hook 'my-haskell-init)))

(use-package helm
  :defer t
  :init
  (progn
    (require 'helm-config)
    (autoload 'helm-org-in-buffer-search "helm-org" nil t)
    (bind-keys
     ("<f8>" . helm-occur)
     ("C-'" . helm-buffers-list)
     ("C-x C-f" . helm-find-files))
    (bind-keys :map ctl-dot-prefix-map
      ("k" . helm-show-kill-ring))
    (bind-keys :map helm-command-map
      ("o a" . helm-org-agenda-files-headings)
      ("o h" . helm-org-in-buffer-headings))
    (use-package helm-gtags
      :bind (("M-'" . helm-gtags-dwim)))))

(use-package help-mode
  :defer t
  :config
  (progn
    (use-package better-jump)
    (bind-key "l" 'help-go-back help-mode-map)))

(use-package ibuffer
  :bind ("<f1> <f1>" . ibuffer)
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
  (use-package better-jump))

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
    (add-hook 'js-mode-hook 'my-js-mode-init)))

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

(use-package letcheck
  :commands letcheck-mode)

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

(use-package notmuch
  :bind (("C-. C-n" . notmuch))
  :init
  (progn
    (autoload #'my-notmuch-unread "notmuch" nil t)
    (autoload #'my-notmuch-inbox "notmuch" nil t)
    (bind-keys :map ctl-dot-prefix-map
      ("C-u" . my-notmuch-unread)
      ("<C-i-key>" . my-notmuch-inbox)
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

(use-package php-mode
  :mode ("\\.php\\'" . php-mode)
  :config
  (progn
    (use-package better-jump)
    (use-package php-eldoc)

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

    (defun my-php-run-tests ()
      "Run all Nette tests found in current directory."
      (interactive)
      (let* ((root (my-php-find-project-root))
             (tester-dir (concat root "/composer/vendor/bin/"))
             (dir (if (file-remote-p default-directory)
                      (tramp-file-name-localname (tramp-dissect-file-name default-directory))
                    default-directory)))
        (when (buffer-modified-p) (save-buffer))
        (async-shell-command (format "cd '%s'; php tester -c php.ini '%s'" tester-dir dir))))
    (bind-key "C-c C-c" 'my-php-run-tests php-mode-map)

    (defun my-php-run ()
      "Run all Nette tests found in current directory."
      (interactive)
      (let* ((file (if (file-remote-p (buffer-file-name))
                       (tramp-file-name-localname (tramp-dissect-file-name (buffer-file-name)))
                     (buffer-file-name))))
        (when (buffer-modified-p) (save-buffer))
        (async-shell-command (format "php '%s'" file))))
    (bind-key "C-c C-r" 'my-php-run php-mode-map)

    (defun my-php-get-function-args ()
      "Return all arguments of php function.

Point should be at the line containing `function'."
      (let ((function-args (sp-get (sp-down-sexp)
                             (buffer-substring-no-properties :beg :end)))
            (args nil))
        (save-match-data
          (with-temp-buffer
            (insert function-args)
            (goto-char (point-min))
            (while (re-search-forward "\\(&?\\$.*?\\)[ \n\t,)]" nil t)
              (push (match-string 1) args))))
        (nreverse args)))

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

    (defun my-php-disable-multi-web-mode ()
      "Set current buffer to `php-mode' and disable `multi-web-mode'."
      (interactive)
      (php-mode)
      (multi-web-mode -1))

    (defun my-php-mode-init ()
      (c-set-style "php")
      (setq-local eldoc-documentation-function 'my-php-eldoc-function)
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
  :bind (("S-RET" . projectile-switch-to-buffer)))

(use-package quail
  :defer t
  :config
  (progn
    (define-key quail-simple-translation-keymap (kbd "C-SPC") 'quail-select-current)
    (define-key quail-simple-translation-keymap "\C-f" 'quail-next-translation)
    (define-key quail-simple-translation-keymap "\C-b" 'quail-prev-translation)

    (set-input-method "TeX")
    (quail-define-rules
     ((append . t))
     ("\\Bbb{Q}" ?ℚ)
     ("\\,a" ?ạ)
     ("\\,b" ?ḅ)
     ("\\,d" ?ḍ)
     ("\\,e" ?ẹ)
     ("\\,h" ?ḥ)
     ("\\,i" ?ị)
     ("\\,k" ?ḳ)
     ("\\,l" ?ḷ)
     ("\\,m" ?ṃ)
     ("\\,n" ?ṇ)
     ("\\,o" ?ọ)
     ("\\,r" ?ṛ)
     ("\\,s" ?ṣ)
     ("\\,t" ?ṭ)
     ("\\,u" ?ụ)
     ("\\,v" ?ṿ)
     ("\\,w" ?ẉ)
     ("\\,y" ?ỵ)
     ("\\,z" ?ẓ)
     ("\\,A" ?Ạ)
     ("\\,B" ?Ḅ)
     ("\\,D" ?Ḍ)
     ("\\,E" ?Ẹ)
     ("\\,H" ?Ḥ)
     ("\\,I" ?Ị)
     ("\\,K" ?Ḳ)
     ("\\,L" ?Ḷ)
     ("\\,M" ?Ṃ)
     ("\\,N" ?Ṇ)
     ("\\,O" ?Ọ)
     ("\\,R" ?Ṛ)
     ("\\,S" ?Ṣ)
     ("\\,T" ?Ṭ)
     ("\\,U" ?Ụ)
     ("\\,V" ?Ṿ)
     ("\\,W" ?Ẉ)
     ("\\,Y" ?Ỵ)
     ("\\,Z" ?Ẓ))
    (toggle-input-method)))

(use-package recentf
  :defer t
  :config
  (progn
    (defvar my-recentf-autosave-timer (run-with-timer 1200 nil 'recentf-save-list))))

(use-package revbufs
  :bind ("C-<f5>" . revbufs))

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

(use-package two-column
  :defer t
  :config
  (progn
    (defadvice 2C-dissociate (after close-window-after-disconnect activate)
      (delete-window))))

(use-package undo-tree
  :bind (("C-x u" . undo-tree-visualize))
  :diminish undo-tree-mode)

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
  :bind ("<insert>" . wd-show-translation))

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
      (find-file (f-join (car yas-snippet-dirs)
                         (symbol-name (or (cdr (assq major-mode my-yas-snippet-parent-mode))
                                          major-mode))
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
