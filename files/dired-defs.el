;;;_. Commentary & basic stuff

(defconst my-dired-media-files-extensions '("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "flv" "ogg" "wmv" "mkv" "mov" "wma")
  "Media file extensions that should launch in VLC.

Also used for highlighting.")

(use-package image-dired)
(use-package dired-aux)
(use-package dired-x
  :config
  (progn
    (defun dired-virtual-revert (&optional _arg _noconfirm)
      "Enable revert for virtual direds."
      (let ((m (dired-file-name-at-point))
            (buffer-modified (buffer-modified-p)))
        (goto-char 1)
        (dired-next-subdir 1)
        (dired-do-redisplay nil t)
        (while (dired-next-subdir 1 t)
          (dired-do-redisplay nil t))
        (when m (dired-goto-file m))
        (set-buffer-modified-p buffer-modified)))

    (defun my-dired-jump ()
      (interactive)
      (if (eq major-mode 'dired-mode)
          (let ((file (dired-utils-get-filename)))
            (dired (f-parent file))
            (dired-utils-goto-line file))
        (dired-jump)))
    (bind-key "C-j" 'my-dired-jump ctl-x-map)

    (add-to-list 'dired-guess-shell-alist-user
                 (list (concat "\\."
                               (regexp-opt my-dired-media-files-extensions)
                               "\\'")
                       "vlc"))))
(use-package dired+
  :config
  (progn
    ;; Remove stupid font-locking
    (setf (nth 3 diredp-font-lock-keywords-1)
          ;; Properly handle the extensions
          '("[^ .\\/]\\(\\.[^. /]+\\)$" 1 diredp-file-suffix))
    (setf (nth 6 diredp-font-lock-keywords-1)
          (list (concat "^  \\(.*\\(" (concat (mapconcat 'regexp-quote
                                                          (or (and (boundp 'dired-omit-extensions)
                                                                   dired-omit-extensions)
                                                              completion-ignored-extensions)
                                                          "[*]?\\|")
                                              "[*]?")        ; Allow for executable flag (*).
                        "\\)\\)$") ; Do not treat compressed files as garbage... why the hell!
                1 diredp-ignored-file-name t))
    ))

(use-package dired-avfs)
(use-package dired-filter)
(use-package dired-open)

;; we should just hijack C-t map from image-dired which is crap anyway
(use-package dired-images)

(use-package dired-subtree
  :init
  (bind-keys :map dired-mode-map
             :prefix "C-,"
             :prefix-map dired-subtree-map
             :prefix-docstring "Dired subtree map."
    ("<C-i-key>" . dired-subtree-insert)
    ("C-/" . dired-subtree-apply-filter)
    ("C-k" . dired-subtree-remove)
    ("C-n" . dired-subtree-next-sibling)
    ("C-p" . dired-subtree-previous-sibling)
    ("C-u" . dired-subtree-up)
    ("C-d" . dired-subtree-down)
    ("C-a" . dired-subtree-beginning)
    ("C-e" . dired-subtree-end)
    ("C-c" . dired-subtree-cycle)
    ("m" . dired-subtree-mark-subtree)
    ("u" . dired-subtree-unmark-subtree)
    ("C-o C-f" . dired-subtree-only-this-file)
    ("C-o C-d" . dired-subtree-only-this-directory)))

(use-package dired-rainbow
  :init
  (progn
    (dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
    (dired-rainbow-define xml "#b4fa70" ("xml" "xsd" "xsl" "xslt" "wsdl"))

    (dired-rainbow-define document "#fce94f" ("doc" "docx" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub"))
    (dired-rainbow-define excel "#3465a4" ("xlsx"))
    (dired-rainbow-define media "#ce5c00" my-dired-media-files-extensions)
    (dired-rainbow-define image "#ff4b4b" ("jpg" "png" "jpeg" "gif"))

    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define sourcefile "#fcaf3e" ("py" "c" "cc" "h" "java" "pl" "rb" "R" "php"))

    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#ad7fa8" ("zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#e6a8df" ("deb" "rpm"))
    (dired-rainbow-define encrypted "LightBlue" ("gpg" "pgp"))

    (dired-rainbow-define-chmod executable-unix "Green" "-.*x.*")
    ))

(use-package dired-ranger
  :init
  (progn
    (bind-keys :map dired-mode-map
               :prefix "c"
               :prefix-map dired-ranger-map
               :prefix-docstring "Map for ranger operations."
      ("c" . dired-ranger-copy)
      ("p" . dired-ranger-paste)
      ("m" . dired-ranger-move))

    (bind-keys :map dired-mode-map
      ("'" . dired-ranger-bookmark)
      ("`" . dired-ranger-bookmark-visit))))

(use-package dired-narrow
  :commands dired-narrow
  :init
  (bind-key "s" 'dired-narrow dired-mode-map))

(use-package dired-tagsistant
  :init
  (progn
    (bind-keys :map dired-mode-map
               :prefix "M-t"
               :prefix-map dired-tagsistant-map
               :prefix-docstring "Dired tagsistant map."
      ("t" . dired-tagsistant-tag)
      ("s" . dired-tagsistant-tag-symlink))))

;; TODO: continue with the transformation to org here
(use-package make-it-so
  :defer t
  :commands (make-it-so)
  :init
  (bind-keys :map dired-mode-map
             :prefix ","
             :prefix-map dired-make-it-so-map
             :prefix-docstring "Make it so map."
    ("," . make-it-so)
    ("f" . mis-finalize)
    ("a" . mis-abort)
    ("r" . mis-replace))
  (use-package make-mode
    :config
    (progn
      (bind-key "<f5>" 'mis-save-and-compile makefile-mode-map))))

;;;_. Key bindings & hooks
(defun my-image-dired-thumbnail-mode-init ()
  (bind-keys :map image-dired-thumbnail-mode-map
     ("b" . image-dired-backward-image)
     ("f" . image-dired-forward-image)
     ("n" . image-dired-next-line)
     ("p" . image-dired-previous-line)
     ("q" . kill-this-buffer)))
(add-hook 'image-dired-thumbnail-mode-hook 'my-image-dired-thumbnail-mode-init)

(defun my-image-dired-display-image-init ()
  (bind-keys :map image-dired-display-image-mode-map
     ("q" . kill-this-buffer)
     ("SPC" . my-image-dired-display-next)
     ("<backspace>" . my-image-dired-display-previous)
     ("<wheel-down>" . my-image-dired-display-next)
     ("<wheel-up>" . my-image-dired-display-previous)
     ("m" . my-image-dired-mark-image-in-dired)
     ("u" . my-image-dired-unmark-image-in-dired)
     ("RET" . my-image-dired-display-open)
     ("M-RET" . my-image-dired-display-external)))
(add-hook 'image-dired-display-image-mode-hook 'my-image-dired-display-image-init)

(defun my-dired-beginning-of-defun (&optional arg)
  (interactive "p")
  (unless (= (line-number-at-pos) 1)
    (call-interactively 'diredp-prev-subdir)
    t))

(defun my-dired-extract-index-name ()
  (save-excursion
    (back-to-indentation)
    (buffer-substring-no-properties
     (point)
     (1- (re-search-forward ":$")))))

(defun my-dired-imenu-create-index ()
  (let* ((alist (imenu-default-create-index-function))
         (uniquified (f-uniquify-alist (-map 'car alist))))
    (--remove (= 0 (length (car it))) (--map (cons (cdr (assoc (car it) uniquified)) (cdr it)) alist))))

(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's loaded."
  (when (eq system-type 'windows-nt)
    (set (make-local-variable 'coding-system-for-read) 'cp1250)
    (set (make-local-variable 'file-name-coding-system) 'cp1250))
  (set (make-local-variable 'beginning-of-defun-function) 'my-dired-beginning-of-defun)
  (set (make-local-variable 'imenu-extract-index-name-function) 'my-dired-extract-index-name)
  (set (make-local-variable 'imenu-create-index-function) 'my-dired-imenu-create-index)

  ;; (defvar slash-dired-prefix-map)
  ;; (define-prefix-command 'slash-dired-prefix-map)

  (bind-keys :map dired-mode-map
    ;; clean bullshit bindings so C-h e b shows us real info
    ("A") ("F") ("G") ("P") ("Q") ("X") ("Z") ("#") (".")
    ("~") ("e") ("f") ("l") ("v") ("^") ("?") ("<f1>")

    ("(" . dired-hide-details-mode)

    ("C-x C-f" . my-dired-ido-find-file)
    ("k" . my-dired-do-kill-lines)

    ("K" . my-dired-kill-subdir)
    ("P" . my-dired-parent-directory)
    ("I" . my-dired-maybe-insert-subdir)

    ("e" . diredp-marked)

    ("n" . dired-hacks-next-file)
    ("p" . dired-hacks-previous-file)
    ("C-d" . (lambda (arg)
               "Move to next directory line."
               (interactive "p")
               (ignore-errors (dired-next-dirline arg))))

    ("M-p" . diredp-prev-subdir)
    ("M-n" . diredp-next-subdir)

    ("<insert>" . dired-mark)
    ("SPC" . dired-mark)
    ("<delete>" . dired-unmark-backward)
    ("<backspace>" . dired-up-directory)
    ("C-o" . dired-filter-mode)
    ("C-S-o" . dired-filter-group-mode)
    ("M-<f5>" . dired-arc-pack-files)
    ("M-<f6>" . dired-arc-unpack-file)

    ("* r" . diredp-mark-region-files)
    ("E" . my-dired-encrypt-file))

  (bind-keys :map dired-mode-map
             :prefix "f"
             :prefix-map dired-file-map
             :prefix-docstring "Map containing less common dired operations related to files.")

  (bind-keys :map dired-mode-map
             :prefix "F"
             :prefix-map dired-marked-file-map
             :prefix-docstring "Map containing less common dired operations on marked files."
    ("S" . my-dired-size-of-file))

  (dired-filter-mode t)
  (dired-filter-group-mode t)
  (visual-line-mode -1)
  (toggle-truncate-lines 1))
(add-hook 'dired-mode-hook 'my-dired-init)
(add-hook 'dired-after-readin-hook 'my-format-available-space)

;; revert the dired buffers automatically after these operations.
(--each '(dired-do-rename
          dired-do-copy
          wdired-abort-changes)
  (eval `(defadvice ,it (after revert-buffer activate)
           (revert-buffer))))

(defadvice dired-create-directory (around disable-ido-ubiq-then-revert-then-jump-to-file activate)
  "Disable ido completion of filepaths.

When we are creating new directory, there is nothing to complete."
  (interactive
   (let ((ido-mode-old ido-mode))
     (unwind-protect
         (progn
           (ido-mode -1)
           (list (read-file-name "Create directory: " (dired-current-directory))))
       (when ido-mode-old
         (ido-mode 1)))))
  ad-do-it
  (goto-char (point-min))
  (dired-utils-goto-line (file-truename (ad-get-arg 0))))

;; redefine from dired.el
;; Do not only join filenames with space because there can be spaces in the paths.
;; So we quote the files with space with ''
(defun dired-copy-filename-as-kill (&optional arg)
  "Copy names of marked (or next ARG) files into the kill ring.
The names are separated by a space.
With a zero prefix arg, use the absolute file name of each marked file.
With \\[universal-argument], use the file name relative to the dired buffer's
`default-directory'.  (This still may contain slashes if in a subdirectory.)

If on a subdir headerline, use absolute subdirname instead;
prefix arg and marked files are ignored in this case.

You can then feed the file name(s) to other commands with \\[yank]."
  (interactive "P")
  (let ((string
         (or (dired-get-subdir)
             (mapconcat (lambda (filename)
                          (if (string-match-p " " filename)
                              (s-wrap filename "'" "'")
                            filename))
                        (if arg
                            (cond ((zerop (prefix-numeric-value arg))
                                   (dired-get-marked-files))
                                  ((consp arg)
                                   (dired-get-marked-files t))
                                  (t
                                   (dired-get-marked-files
                    'no-dir (prefix-numeric-value arg))))
                          (dired-get-marked-files 'no-dir))
                        " "))))
    (if (eq last-command 'kill-region)
    (kill-append string nil)
      (kill-new string))
    (message "%s" string)))

(defadvice wdired-change-to-wdired-mode (around do-not-fuck-with-the-revert-function activate)
  (let ((revert-function revert-buffer-function))
    ad-do-it
    (setq revert-buffer-function revert-function)))

(defadvice wdired-change-to-dired-mode (around do-not-fuck-with-the-revert-function activate)
  (let ((revert-function revert-buffer-function))
    ad-do-it
    (setq revert-buffer-function revert-function)))

(defun my-format-available-space ()
  (save-excursion
    (goto-char (point-min))
    (forward-line)
    (when (search-forward "directory" (line-end-position) t)
      (forward-char)
      (let* ((avail (word-at-point))
             (avail-hr (s-trim (ls-lisp-format-file-size (* 1024 (string-to-int avail)) t 1)))
             (inhibit-read-only t)
             (kill-ring kill-ring))
        (kill-word 1)
        (insert avail-hr)))
    (when (search-forward "available" (line-end-position) t)
      (forward-char)
      (let* ((avail (word-at-point))
             (avail-hr (s-trim (ls-lisp-format-file-size (* 1024 (string-to-int avail)) t 1)))
             (inhibit-read-only t)
             (kill-ring kill-ring))
        (kill-word 1)
        (insert avail-hr)))))

(defun my-dired-list-all-subdirs (arg)
  (interactive "P")
  (let ((dir (if arg
                 (ido-read-directory-name "Directory: " default-directory default-directory)
               default-directory)))
    (dired dir "-lR")))

(defun my-dired-create-file-backup (file)
  "Create a backup from file under point.

The backup file is in the same directory with the same name but
with <date><time>.bak appended."
  (interactive (list (read-file-name "File to backup: " nil (car (dired-get-marked-files)))))
  (let ((new-name (concat file "." (format-time-string "%Y%m%d%H%M%S") ".bak")))
    (copy-file file new-name)))

(defun my-image-dired--with-image-in-dired (operation)
  "OPERATION is a function of two arguments, the file we operate
on and associated dired buffer."
  (let* ((old-buf (current-buffer))
         (file (image-dired-original-file-name))
         (dired-name (progn
                       (string-match ".*/\\(.*\\)/.*" file)
                       (match-string 1 file)))
         (dired-buf (get-buffer dired-name)))
    (if (not dired-buf)
        (error "No associated dired buffer found.")
      (set-buffer dired-buf)
      (funcall operation file dired-buf)
      (set-buffer old-buf))))

(defun my-image-dired--skip-non-image (arg)
  "Skip non-image files in direction ARG"
  (while (and (or (file-directory-p (car (dired-get-marked-files)))
                  (not (eq (get-char-property (point) 'face) 'my-diredp-image-face)))
              (not (eobp))
              (not (bobp)))
    (dired-next-line arg)
    (set-window-point (get-buffer-window dired-buf) (point))))

(defun my-image-dired-display-next (arg)
  (interactive "p")
  (my-image-dired--with-image-in-dired
   (lambda (file dired-buf)
     (dired-goto-file file)
     (dired-next-line arg)
     (ignore-errors (my-image-dired--skip-non-image arg))
     (let ((next-img (ignore-errors (dired-get-marked-files))))
       (unless next-img
         (if (> arg 0)
             (progn
               (goto-char (point-min))
               (dired-next-line (if dired-omit-mode 2 4)))
           (goto-char (point-max))
           (dired-previous-line 1))
         (my-image-dired--skip-non-image arg))
       (image-dired-dired-display-image)
       (set-window-point (get-buffer-window dired-buf) (point))))))

(defun my-image-dired-display-previous ()
  (interactive)
  (my-image-dired-display-next -1))

(defun my-image-dired-goto-image-in-dired ()
  (interactive)
  (my-image-dired--with-image-in-dired
   (lambda (file dired-buf)
     (dired-goto-file file)
     (set-window-point (get-buffer-window dired-buf) (point)))))

(defun my-image-dired-mark-image-in-dired ()
  (interactive)
  (my-image-dired--with-image-in-dired
   (lambda (file dired-buf)
     (dired-goto-file file)
     (dired-mark 1)))
  (my-image-dired-display-next 1))

(defun my-image-dired-unmark-image-in-dired ()
  (interactive)
  (my-image-dired--with-image-in-dired
   (lambda (file dired-buf)
     (dired-goto-file file)
     (dired-unmark 1))))

(defun my-image-dired-display-open ()
  (interactive)
  (find-file (image-dired-original-file-name)))

(defun my-image-dired-display-external ()
  (interactive)
  (w32-browser (image-dired-original-file-name)))

;; FUCO PATCH image-dired
(defun image-dired-track-original-file ()
  "Track the original file in the associated dired buffer.
See documentation for `image-dired-toggle-movement-tracking'.
Interactive use only useful if `image-dired-track-movement' is nil."
  (interactive)
  (let ((old-buf (current-buffer))
        (dired-buf (image-dired-associated-dired-buffer))
        (file-name (image-dired-original-file-name)))
    (when (and (buffer-live-p dired-buf) file-name)
      (with-current-buffer dired-buf
        (if (not (dired-goto-file file-name))
            (message "Could not track file")
          (let ((p (point)))
            (mapc
             (lambda (w) (set-window-point w p))
             (cl-remove-if-not
              (lambda (w) (equal (window-buffer w) dired-buf))
              (cl-mapcan 'window-list (frame-list))))))))))

(defadvice select-window (after image-dired-resize-image activate)
  (when (eq major-mode 'image-dired-display-image-mode)
    (image-dired-display-current-image-sized)))

(defadvice other-window (around image-dired-resize-image activate)
  (let ((buffer (current-buffer)))
    ad-do-it
    (with-current-buffer buffer
      (when (eq major-mode 'image-dired-display-image-mode)
        (image-dired-display-current-image-sized)))
    (when (eq major-mode 'image-dired-display-image-mode)
      (image-dired-display-current-image-sized))))

;;;_. Virtual dired
;; these functions depend on the dired plus package
(defun dired-virtual-save-buffer ()
  (let ((subdirs (nreverse (mapcar 'car dired-subdir-alist)))
        (title (buffer-name))
        (file (buffer-file-name)))
    (with-temp-buffer
      (--map (insert "  " it ":\n\n") subdirs)
      (goto-char (point-min))
      (forward-line)
      (insert "  " title "\n")
      (write-region (point-min) (point-max) (file-truename file)))
    (set-buffer-modified-p nil)
    t))

(defadvice dired-maybe-insert-subdir (around fix-virtual-dired activate)
  ad-do-it
  (when (bound-and-true-p my-virtual-dired-p)
    (set-buffer-modified-p t)))

(defun my-dired-ido-find-file ()
  "Like `ido-find-file' but with `default-directory' set to the
one specified by listing header."
  (interactive)
  (let ((default-directory (dired-current-directory)))
    (ido-find-file)))

(defun my-dired-do-kill-lines (&optional arg fmt)
  "See `dired-do-kill-lines'.

With no prefix argument, kill file under point or marked files.

With prefix argument, kill that many files."
  (interactive "P")
  (cond
   ((not arg)
    (condition-case err
        (if (or (equal (car (dired-get-marked-files))
                       (dired-file-name-at-point))
                (equal (concat (car (dired-get-marked-files)) "/")
                       (dired-file-name-at-point)))
            (progn
              (dired-mark 1)
              (diredp-previous-line 1)
              (dired-do-kill-lines nil fmt)
              (diredp-next-line 1)
              (diredp-previous-line 1))
          (dired-do-kill-lines arg fmt))
      (error ;; if no file is under point, kill the next subdir
       (my-dired-kill-subdir))))
   (t
    (dired-do-kill-lines arg fmt))))

(defun my-dired-kill-subdir (&optional arg)
  "Kill this directory listing.

With \\[universal-argument] and point on subdirectory line, kill
the subdirectory listing."
  (interactive "P")
  (cond
   ((equal '(4) arg)
    (save-excursion
      (dired-goto-subdir (dired-file-name-at-point))
      (dired-kill-subdir)))
   (t
    (my-dired-parent-directory)
    (save-excursion
      (call-interactively 'dired-maybe-insert-subdir)
      (dired-do-kill-lines '(4))))))

(defun my-dired-maybe-insert-subdir ()
  "Just like `dired-maybe-insert-subdir' but don't move point."
  (interactive)
  (save-excursion
    (call-interactively 'dired-maybe-insert-subdir)))

(defun my-dired-parent-directory ()
  "Go to parent directory listing. This is like calling
`dired-maybe-insert-subdir' on the listing header."
  (interactive)
  (diredp-next-subdir 0)
  (call-interactively 'dired-maybe-insert-subdir))

;;;_. Sorting
(require 'ls-lisp)

;; redefine this function, to fix the formatting of file sizes in dired mode
(defun ls-lisp-format-file-size (file-size &optional human-readable level)
  (setq level (or level 1000))
  (if (or (not human-readable)
          (< file-size 1024))
      (format (if (floatp file-size) " %11.0f" " %11d") file-size)
    (do ((file-size (/ file-size 1024.0) (/ file-size 1024.0))
         ;; kilo, mega, giga, tera, peta, exa
         (post-fixes (list "k" "M" "G" "T" "P" "E") (cdr post-fixes))
         (l level (1- l)))
        ((or (= 0 l)
             (< file-size 1024)) (format " %10.0f%s"  file-size (car post-fixes))))))

(defvar dired-sort-modes-list
  '(("size" "S" "")
    ("ext" "X" "S")
    ("cdate" "ct" "X")
    ("date" "t" "ct")
    ("name" "" "t"))
  "List of dired buffer sort modes.")

(defvar dired-sort-current-mode ""
  "Current mode for sorting dired buffer.")

;; redefining from dired.el. Just cycle the options
(defun dired-sort-toggle ()
  (cond
   ((equal dired-sort-current-mode "") (setq dired-sort-current-mode "S") (dired-sort-size))
   ((equal dired-sort-current-mode "S") (setq dired-sort-current-mode "X") (dired-sort-extension))
   ((equal dired-sort-current-mode "X") (setq dired-sort-current-mode "ct") (dired-sort-ctime))
   ((equal dired-sort-current-mode "ct") (setq dired-sort-current-mode "t") (dired-sort-time))
   ((equal dired-sort-current-mode "t") (setq dired-sort-current-mode "") (dired-sort-name))))

;; redefining from dired.el. With double-prefix show a menu to chose the sorting from
(defun dired-sort-toggle-or-edit (&optional arg)
  "Toggle sorting by date, and refresh the Dired buffer.

With a prefix argument \\[universal-argument], edit the current listing switches instead.

With a prefix argument \\[universal-argument] \\[universal-argument] prompt user with list of choices
to chose from."
  (interactive "P")
  (when dired-sort-inhibit
    (error "Cannot sort this dired buffer"))
  (cond
   ((equal arg '(4))
    (dired-sort-other
     (read-string "ls switches (must contain -l): " dired-actual-switches)))
   ((equal arg '(16))
    (let* ((sort-mode (completing-read "Sort by: "
                                       (mapcar 'car dired-sort-modes-list)
                                       nil
                                       t))
           (sort-switch (caddr (assoc sort-mode dired-sort-modes-list))))
      (setq dired-sort-current-mode sort-switch)
      (dired-sort-toggle)))
   (t (dired-sort-toggle))))

(defun dired-sort-size ()
  "Dired sort by size."
  (interactive)
  (dired-sort-other (concat dired-listing-switches "S")))

(defun dired-sort-extension ()
  "Dired sort by extension."
  (interactive)
  (dired-sort-other (concat dired-listing-switches "X")))

(defun dired-sort-ctime ()
  "Dired sort by create time."
  (interactive)
  (dired-sort-other (concat dired-listing-switches "ct")))

(defun dired-sort-time ()
  "Dired sort by time."
  (interactive)
  (dired-sort-other (concat dired-listing-switches "t")))

(defun dired-sort-name ()
  "Dired sort by name."
  (interactive)
  (dired-sort-other (concat dired-listing-switches "")))

;; redefined from dired.el to support new types of sorting
(defun dired-sort-set-modeline ()
  (when (eq major-mode 'dired-mode)
    (setq mode-name
          (let (case-fold-search)
            (cond
             ((string-match "ct" dired-actual-switches)
              "Dired by ctime")
             ((string-match "^-[^t]*t[^t]*$" dired-actual-switches)
              "Dired by date")
             ((string-match "^-[^X]*X[^X]*$" dired-actual-switches)
              "Dired by ext")
             ((string-match "^-[^S]*S[^S]*$" dired-actual-switches)
              "Dired by size")
             ((string-match "^-[^SXUt]*$" dired-actual-switches)
              "Dired by name")
             (t
              (concat "Dired " dired-actual-switches)))))
    (force-mode-line-update)))

;; redefined from dired-aux.el to not make isearch in dired go into
;; recursive edit.
(defun dired-isearch-filenames ()
  "Search for a string using Isearch only in file names in the Dired buffer."
  (interactive)
  (let ((dired-isearch-filenames t))
    (isearch-forward nil t)))

;;;_. Zip support

(add-to-list 'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip"))
(add-to-list 'dired-compress-file-suffixes '("\\.gz\\'" "" "zcat"))
(add-to-list 'dired-compress-file-suffixes '("\\.bz2\\'" "" "bzcatt"))
(add-to-list 'dired-compress-file-suffixes '("\\.tar\\.bz2\\'" "" "untarbz2"))
(add-to-list 'dired-compress-file-suffixes '("\\.tar\\.gz\\'" "" "untargz"))

(defvar dired-arc-unpack-list
  '(("\\.zip\\'"
     (:program "unzip" :switches "")
     (:program "7z" :switches "x -y"))
    ("\\.rar\\'"
     (:program "unrar" :switches "x")
     (:program "7z" :switches "x -y"))
    ("\\.tar\\'" (:program "7z" :switches "x -y"))
    ("\\.tar\\.gz\\'"
     (:program "bsdtar" :switches "zxvf")
     (:program "gzip" :switches "-df" :name-after ".tar")
     (:program "gzip" :switches "-dkf" :name-after ".tar" :comment "Keep original file"))
    ("\\.gz\\'"
     (:program "7z" :switches "x -y")
     (:program "gzip" :switches "-df" :name-after ""))
    ("\\.tar.\\bz2\\'"
     (:program "bsdtar" :switches "jxvf")
     (:program "bunzip2" :switches "-f" :name-after ".tar")
     (:program "bunzip2" :switches "-fk" :name-after ".tar" :comment "Keep original file"))
    ("\\.bz2\\'"
     (:program "7z" :switches "x -y")
     (:program "bunzip2" :switches "-f" :name-after ""))
    ("\\.tgz\\'"
     (:program "bsdtar" :switches "zxvf")
     (:program "gzip" :switches "-df" :name-after ".tar")
     (:program "gzip" :switches "-dkf" :name-after ".tar" :comment "Keep original file"))
    ("\\.tbz\\'"
     (:program "bsdtar" :switches "jxvf")
     (:program "bunzip2" :switches "-f" :name-after ".tar")
     (:program "bunzip2" :switches "-fk" :name-after ".tar" :comment "Keep original file")))
  "List assigning file extensions to methods to unpack them.
The file name is also appended after the program and switches
automatically.

Each method is a plist with these possible keys:
  :program - a program to run
  :switches - switches to pass to the program
  :name-after - for gz/bz2 etc. this will specify how the matched
    extension will be replaced after the extraction.  Useful to
    ask for confirmation if the extraction will be run as async
    process.

If more than one sensible methods exist, they are listed one
after another using the same format.")

(defun dired-arc-pack-files (zip-file)
  "Create an archive containing the marked files."
  (interactive
   (list (let ((files (dired-get-marked-files)))
           (read-from-minibuffer
            "Enter name of zip file: "
            (if (cadr files)
                ;; more than one file selected, use directory name as default
                (file-name-nondirectory
                 (substring default-directory 0 (1- (length default-directory))))
              ;; otherwise use the current file name as defalt
              (file-name-nondirectory (car files)))))))
  (let ((zip-file (if (string-match ".zip$" zip-file) zip-file (concat zip-file ".zip"))))
    (shell-command
     (concat "zip -r "
             (concat "\"" zip-file "\"")
             " "
             (mapconcat (lambda (obj) (format "\"%s\"" obj))
                        (mapcar
                         (lambda (filename)
                           (file-name-nondirectory filename))
                         (dired-get-marked-files)) " ")))
    (dired-unmark-all-marks)
    (revert-buffer)
    (goto-char 0)
    (when (search-forward zip-file nil t)
      (goto-char (match-beginning 0)))))

(defun dired-arc-unpack-file (where)
  "Unpack this archive into the target directory WHERE.
With \\[universal-argument] present user with list of possible methods to unpack the file."
  (interactive
   (list (ido-read-directory-name
          "Enter target directory (where to unpack): "
          (dired-dwim-target-directory))))
  (let ((default-directory default-directory))
    (unless (file-directory-p where)
      (when (y-or-n-p (format "Directory %s does not exist. Create?" where))
        (make-directory where t)))
    (cd where)
    (let* ((file (car (dired-get-marked-files)))
           (method (cdr (--first (string-match-p (car it) file) dired-arc-unpack-list)))
           (cmd (and method
                     (progn
                       (cond
                        ((equal '(4) current-prefix-arg)
                         (let* ((choices (mapcar (lambda (m)
                                                   (cons (concat
                                                          (plist-get m :program)
                                                          " " (plist-get m :switches)
                                                          (when (plist-get m :comment)
                                                            (concat " (" (plist-get m :comment) ")"))) m))
                                                 method))
                                (r (completing-read
                                    "Unpacking method: "
                                    choices
                                    nil t nil nil (caar choices))))
                           (setq method (cdr (assoc r choices)))))
                        (t (setq method (car method))))
                       (concat
                        (plist-get method :program)
                        " " (plist-get method :switches) " \""
                        file "\"")))))
      (shell-command cmd))
    (revert-buffer)))

(defun dired-arc-list-archive ()
  (interactive)
  (let ((buffer (get-buffer-create "*Archive listing*")))
    (with-current-buffer buffer
      (widen)
      (kill-all-local-variables)
      (setq buffer-read-only nil)
      (erase-buffer))
    (shell-command (concat "7z l \"" (car (dired-get-marked-files)) "\"")
                   buffer)
    (with-current-buffer buffer
      (goto-char 1)
      (unless (search-forward "Can not open file as archive" 300 t)
        (goto-char 1)
        (delete-region (point) (progn (forward-line 6) (point)))
        (search-forward "-----------")
        (beginning-of-line 0)
        (delete-region (point) (progn (forward-line 2) (point)))
        (insert "  ---------- ----------- ----------- ------------------------\n")
        (let (date size total)
          (while (not (looking-at "[ ]*-----------"))
            ;; copy the date here
            (delete-char 5)
            (let ((p (point)))
              (forward-char 11)
              (setq date (buffer-substring-no-properties p (point)))
              (delete-char -11))
            (delete-char 4)
            (if (looking-at "D")
                (insert "  drwxrwxrwx")
              (insert "  -rwxrwxrwx"))
            (delete-char 6)
            (let ((p (point)))
              (forward-char 13)
              (setq size (buffer-substring-no-properties p (point)))
              (delete-char -13)
              (insert (ls-lisp-format-file-size (string-to-int size) t) " "))
            (delete-char 13)
            (insert date)
            (forward-line))
          (delete-region (point) (progn (forward-line 1) (point)))
          (insert "  ---------- ----------- ----------- ------------------------\n")
          (forward-char 25)
          (let ((p (point)))
            (forward-char 13)
            (setq total (buffer-substring-no-properties p (point)))
            (delete-char -13))
          (delete-char -13)
          (insert (ls-lisp-format-file-size (string-to-int total) t) " ")
          (delete-char 13)
          (insert "          "))
        (goto-char 1)
        (replace-string "\\" "/")
        (dired-mode "/")))))

;;;_. gpg stuff
;; gpg --output passwords.gpg --symmetric passwords.txt
(defun my-dired-encrypt-file ()
  (interactive)
  (require 'epa)
  (if (not (dired-utils-is-file-p))
      (message "We can only encrypt single files.")
    (epa-encrypt-file (dired-utils-get-filename) nil)
    (revert-buffer)))


;;;_. other-marker-do
(defun my-dired-with-marker-do (marker-char)
  (interactive "cMarker char: ")
  (let ((dired-marker-char marker-char))
    (call-interactively (key-binding (read-key-sequence "Dired command: " t)))))

;;;_. File-size info
(defun my-dired-size-of-file ()
  "Print size of file under point, or a list of results for marked files."
  (interactive)
  (let* ((marked-files (dired-get-marked-files))
         (parent (f-common-parent marked-files))
         (marked-files (--map (s-chop-prefix parent it) marked-files)))
    (if (cdr marked-files)
        (dired-do-shell-command "du --apparent-size -s -h -c * &" nil marked-files)
      (dired-do-shell-command "du --apparent-size -s -h" nil marked-files))))
