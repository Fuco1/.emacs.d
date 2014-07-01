;; autoinstall packages
(require 'cl)

(defvar prelude-packages
  '(
    ace-jump-mode
    ack-and-a-half
    bookmark+
    browse-kill-ring
    dash
    dash-functional
    diminish
    dired+
    dired-details
    eldoc-eval
    emmet-mode
    expand-region
    f
    flx
    flx-ido
    fuzzy-match
    golden-ratio
    haskell-mode
    ibuffer-vc
    icicles
    ido-ubiquitous
    jump-char
    keyfreq
    macrostep
    magit
    map-regexp
    markdown-mode
    markdown-mode+
    multi-web-mode
    multiple-cursors
    noflet
    parenface
    php-mode
    pos-tip
    projectile
    rainbow-mode
    s
    shell-pop
    smex
    undo-tree
    w32-browser
    )
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (prelude-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'autoinstall)
