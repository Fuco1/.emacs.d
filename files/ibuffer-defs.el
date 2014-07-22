(bind-keys :map ibuffer-mode-map
           ("M-o" . elwm-activate-window)
           ("U" . (lambda () (interactive) (ibuffer-unmark-all 0))))

;; filter groups
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Org" ;; all org-related buffers
                (mode . org-mode))
               ("emacs-elpa"
                (or (predicate
                     .
                     (let ((bfn (buffer-file-name (current-buffer))))
                       (when bfn
                         (and (string-match-p "\\.emacs\\.d/elpa" bfn)
                              (eq major-mode 'emacs-lisp-mode)))))))
               ("emacs-config"
                (or (predicate
                     .
                     (let ((bfn (buffer-file-name (current-buffer))))
                       (when bfn
                         (and (string-match-p "\\.emacs\\.d" bfn)
                              (eq major-mode 'emacs-lisp-mode)))))))
               ("emacs"
                (or (mode . emacs-lisp-mode)
                    (mode . lisp-interaction-mode)
                    (mode . inferior-emacs-lisp-mode)))
               ("TeX"
                (or (mode . tex-mode)
                    (mode . plain-tex-mode)
                    (mode . latex-mode)
                    (mode . bibtex-mode)))
               ("Markdown" (or (mode . markdown-mode)
                               (mode . gfm-mode)))
               ("Web"
                (or (mode . html-mode)
                    (mode . css-mode)
                    (mode . php-mode)
                    (mode . js-mode)))
               ("Dired"
                (mode . dired-mode))
               ("Langs"
                (or (predicate
                     .
                     (let ((bfn (buffer-file-name (current-buffer))))
                       (when bfn
                         (string-match-p "d:/languages" bfn))))))
               ("Images"
                (or (mode . image-dired-display-image-mode)
                    (mode . image-dired-thumbnail-mode)
                    (mode . image-mode)))
               ("Tramp"
                (or (name . "tramp")))
               ("Programming" ;; prog stuff not already in MyProjectX
                (or
                 (mode . c-mode)
                 (mode . cc-mode)
                 (mode . c++-mode)
                 (mode . js3-mode)
                 (mode . perl-mode)
                 (mode . python-mode)
                 (mode . haskell-mode)
                 (mode . makefile-gmake-mode)
                 ;; etc
                 ))
               ("IRC"
                (or (mode . erc-mode)
                    (mode . rcirc-mode)
                    (mode . circe-channel-mode)
                    (mode . circe-server-mode)))
               ("Search"
                (or (mode . ag-mode)))
               ("Org Agenda"
                (or (mode . org-agenda-mode)))
               ("Images"
                (or (mode . di-view-mode)
                    (mode . di-thumb-mode)))
               ))))

;; (define-ibuffer-filter in-directory
;;   "Toggle current view to buffers whose default-directory is in QUALIFIER."
;;   (:description "in-directory"
;;    :reader (read-directory-name "Directory: "))
;;   (with-current-buffer buf (file-in-directory-p default-directory qualifier)))

(define-ibuffer-column size-h
  (:name "Size"
   :inline t
   :summarizer
   (lambda (column-strings)
     (let ((total 0))
       (dolist (string column-strings)
         (setq total
               ;; like, ewww ...
               (+
                (let ((number (float (string-to-number string))))
                  (cond
                   ((string-match-p "K" string)
                    (* number 1000))
                   ((string-match-p "M" string)
                    (* number 1000000))
                   (t number)))
                total)))
       (file-size-human-readable total 'si))))
  (file-size-human-readable (buffer-size) 'si))

;; Modify the default ibuffer-formats
(setq ibuffer-formats
      '((mark modified vc-status-mini read-only
              " " (name 25 25 :left :elide)
              " " (size-h 9 -1 :right)
              " " (mode 16 16 :left :elide)

              " " filename-and-process)
        (mark " " (name 30 -1)
              " " filename)))

;; Switching to ibuffer puts the cursor on the most recent buffer
(defadvice ibuffer (around ibuffer-point-to-most-recent activate) ()
  "Open ibuffer with cursor pointed to most recent buffer name"
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))
