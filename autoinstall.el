(defvar autoinstall-packages
  '(
    ag
    bookmark+
    browse-kill-ring
    dash
    dash-functional
    diminish
    dired+
    dired-details
    eldoc-eval
    eimp
    eldoc-eval
    elfeed
    emmet-mode
    expand-region
    f
    flx
    flx-ido
    fold-this
    fuzzy-match
    google-this
    haskell-mode
    helm-descbinds
    hydra
    ibuffer-vc
    icicles
    ido-ubiquitous
    jump-char
    keyfreq
    legalese
    macrostep
    magit
    make-it-so
    malabar-mode
    markdown-mode
    markdown-mode+
    multi-web-mode
    multiple-cursors
    noflet
    notmuch
    notmuch-unread
    org-plus-contrib
    ov
    paren-face
    php-eldoc
    php-mode
    php-refactor-mode
    pos-tip
    projectile
    rainbow-mode
    s
    shell-pop
    smartscan
    smex
    undo-tree
    visual-regexp
    w3m
    wgrep-ag
    world-time-mode
    )
  "A list of packages to ensure are installed at launch.")

(when (--any? (not (package-installed-p it)) autoinstall-packages)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p autoinstall-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'autoinstall)
