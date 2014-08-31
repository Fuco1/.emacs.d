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
    elfeed
    emmet-mode
    expand-region
    f
    flx
    flx-ido
    fuzzy-match
    haskell-mode
    helm-descbinds
    ibuffer-vc
    icicles
    ido-ubiquitous
    jump-char
    keyfreq
    legalese
    macrostep
    magit
    make-it-so
    markdown-mode
    markdown-mode+
    multi-web-mode
    multiple-cursors
    noflet
    ov
    parenface
    pos-tip
    projectile
    rainbow-mode
    s
    shell-pop
    smex
    undo-tree
    visual-regexp
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
