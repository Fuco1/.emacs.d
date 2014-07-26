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
         ("A-l" . bjump-word-jump-line)
         ("A-;" . bjump-word-jump-paragraph)
         ("s-o" . bjump-window-jump)
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

    (bind-key "M-o" 'elwm-activate-window bookmark-bmenu-mode-map)))

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
    (defun my-circe-get-dasnet-irssi-passwd (_)
      (with-temp-buffer
        (insert-file "~/secrets/dasnet-irssi-proxy.gpg")
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
            ("^<.*?>" circe-originator-face))))
  :config
  (progn
    (sp-with-modes 'circe-channel-mode
      (sp-local-pair "`" "'"))

    (defun my-circe-channel-setup ()
      "Setup channel buffer."
      (my-init-text-based-modes)
      (smartparens-mode t))

    (add-hook 'circe-channel-mode-hook 'my-circe-channel-setup)

    (defun my-circe-kill-all-irc-buffers ()
      "Kill all circe buffers."
      (interactive)
      (--each (buffer-list)
        (with-current-buffer it
          (when (eq major-mode 'circe-server-mode)
            (kill-buffer it))))
      (--each (buffer-list)
        (with-current-buffer it
          (when (eq major-mode 'circe-channel-mode)
            (kill-buffer it)))))

    (defun my-circe-open-irc-frame ()
      "Open an IRC frame."
      (interactive)
      (select-frame (make-frame-command))
      (set-frame-parameter (selected-frame) :frame-type :circe)
      (set-frame-parameter (selected-frame) 'name "Circe")
      (set-frame-parameter (selected-frame) 'explicit-name "Circe"))))

(use-package clippy
  :commands clippy-describe-function)

(use-package css-mode
  :defer t
  :config
  (progn
    (defun my-css-mode-setup ()
      (multi-web-mode 1)
      (emmet-mode 1))
    (add-hook 'css-mode-hook 'my-css-mode-setup)))

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

(use-package dired-tagsistant
  :pre-init
  (bind-keys :prefix-map ctl-x-t-map
             :prefix "C-x T"
             :prefix-docstring "C-x T prefix map")
  :bind (("C-x T +" . dired-tagsistant-some-tags)
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
  :bind (("C-. C-f" . elfeed))
  :config
  (progn
    (bind-keys :map elfeed-show-mode-map
      ("M-n" . shr-next-link)
      ("M-p" . shr-previous-link))
    (bind-keys :map elfeed-search-mode-map
      ("g" . elfeed-update))))

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
    (add-hook 'dired-mode-hook (lambda () (bind-key "M-o" 'elwm-activate-window dired-mode-map)))))

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

(use-package golden-ratio
  :diminish golden-ratio-mode
  :config
  (progn
    (defun my-golden-ratio-inhibit ()
      (or (--any? (string-match-p "\\*Ediff Control Panel" it)
                  (mapcar 'buffer-name (mapcar 'window-buffer (window-list))))))))

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

(use-package guide-key
  :diminish guide-key-mode
  :config
  (progn
    (defadvice guide-key/popup-guide-buffer (around fix-golden-ration activate)
      (golden-ratio-mode -1)
      ad-do-it
      (golden-ratio-mode 1))))

(use-package free-keys
  :commands free-keys)

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
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
  :config
  (progn
    (defadvice ispell-word (around fix-golden-ration activate)
      (golden-ratio-mode -1)
      ad-do-it
      (golden-ratio-mode 1))))

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
    (require 'flyspell)))

(use-package markdown-mode
  :mode ("\\.md$" . gfm-mode)
  :config
  (progn
    (load "files/markdown-defs")))

(use-package multi-web-mode
  :defer t
  :config
  (progn
    (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                      (javascript-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                      (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
    (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))))

(use-package notmuch
  :bind (("C-. C-n" . notmuch))
  :init
  (progn
    (defun my-notmuch-unread ()
      "Display buffer with unread mail."
      (interactive)
      (require 'notmuch)
      (notmuch-search "tag:unread"))

    (bind-key "C-. C-u" 'my-notmuch-unread))
  :config
  (progn
    (use-package notmuch-unread)
    ;; REDEFINED FROM notmuch-unread-mode
    ;; Don't show anything if there's no unread mail
    (defun notmuch-unread-update-handler ()
      "Update the mode line."
      (let ((count (notmuch-unread-count)))
        (if (> count 0)
            (setq notmuch-unread-mode-line-string
                  (format " [✉ %d]" count))
          (setq notmuch-unread-mode-line-string "")))
      (force-mode-line-update))

    (defun my-notmuch-update-mail ()
      (interactive)
      (start-process "mail-update" nil "/bin/bash" "/home/matus/bin/run-getmail"))

    (defvar my-notmuch-update-mail-timer
      (run-with-timer 10 400 'my-notmuch-update-mail)
      "Mail update timer.")

    (defun my-notmuch-delete-mail (&optional beg end)
      (interactive (if (use-region-p)
                       (list (region-beginning) (region-end))
                     (list nil nil)))
      (let ((change '("+deleted" "-unread" "-inbox")))
        (if (eq major-mode 'notmuch-search-mode)
            (notmuch-search-tag change beg end)
          (notmuch-show-tag change))))

    (bind-key "d" 'my-notmuch-delete-mail notmuch-show-mode-map)
    (bind-key "d" 'my-notmuch-delete-mail notmuch-search-mode-map)))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  ;; The following lines are always needed.  Choose your own keys.
  :bind  (("C-c l" . org-store-link)
          ("<f12>" . org-agenda)
          ("C-c C-x C-o" . org-clock-out)
          ("C-c C-x <C-i-key>" . org-clock-in))
  :config
  (progn
    (load "files/org-defs.el")
    (load "projects/org-velocity/org-velocity.el")
    (bind-key "C-c s" 'org-velocity org-mode-map)))

(use-package popwin
  :commands popwin-mode
  :config
  (progn
    (push '("*Pp Eval Output*" :height 15) popwin:special-display-config)))

(use-package projectile
  :defer t
  :diminish projectile-mode
  :bind (("S-RET" . projectile-switch-to-buffer))
  :config
  (progn
    (defun projectile-project-root ()
      "Retrieves the root directory of a project if available.
The current directory is assumed to be the project's root otherwise."
      (let ((project-root
             (or (->> projectile-project-root-files
                   (--map (locate-dominating-file (file-truename default-directory) it))
                   (-remove #'null)
                   (--max-by (> (s-count-matches "/" it) (s-count-matches "/" other))) ;;; return the closest "parent dir" for this (possible) subproject
                   (projectile-file-truename))
                 (if projectile-require-project-root
                     (error "You're not in a project")
                   default-directory))))
        project-root))

    (defun projectile-get-ext-command ()
      "Determine which external command to invoke based on the project's VCS."
      (concat
       "find . -not \\( \\( "
       (mapconcat (lambda (x)
                    (concat "-path \"*/" x "/*\"")) projectile-globally-ignored-directories " -or ")
       (let ((proj-ig-dirs (projectile-project-ignored-directories)))
         (if (not proj-ig-dirs) ""
           (concat
            " -or "
            (mapconcat (lambda (x)
                         (concat "-path \"" x "\""))
                       (let ((project-root (projectile-project-root)))
                         (--map (concat "./" (file-relative-name it project-root)) proj-ig-dirs)) " -or "))))
       " \\) -prune \\)"
       " -not "
       (mapconcat (lambda (x)
                    (concat "-path \"*/" x "\"")) projectile-globally-ignored-directories " -not ")
       " -type f -print0"))))

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
      (bind-keys :map html-mode-map
        ("C-c C-f" . sp-html-next-tag)
        ("C-c C-b" . sp-html-previous-tag)))
    (add-hook 'html-mode-hook 'my-html-mode-setup)))

(use-package skeleton-complete
  :commands skeleton-complete-mode
  :diminish skeleton-complete-mode)

(use-package smartparens
  :defer t
  :diminish smartparens-mode
  :init
  (progn
    (load "~/.emacs.d/files/smartparens")))

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
    (require 'dropdown-list)

    (yas-global-mode 1)

    (setq yas-snippet-dirs '("~/.emacs.d/vendor/yasnippet/snippets"))

    (setq yas-prompt-functions '(yas-ido-prompt))

    (defun my-yas-startup ()
      ;; stupid yasnippet :/ we define <tab> behaviour elsewhere
      (define-key yas-minor-mode-map [(tab)] nil)
      (define-key yas-minor-mode-map (kbd "TAB") nil))

    ;; Replace yasnippets's TAB, was yas/expand
    (add-hook 'yas-minor-mode-hook 'my-yas-startup)))
