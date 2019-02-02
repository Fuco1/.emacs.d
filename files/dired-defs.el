;;;_. Commentary & basic stuff
(require 'use-package)
(require 'imenu)

(defconst my-dired-media-files-extensions '("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "flv" "ogg" "wmv" "mkv" "mov" "wma")
  "Media file extensions that should launch in VLC.

Also used for highlighting.")

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

(use-package dired-avfs)
(use-package dired-filter)
(use-package dired-open)
(use-package diredfl
  :straight t)

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
  :config
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
  (bind-keys :map dired-mode-map
             :prefix "c"
             :prefix-map dired-ranger-map
             :prefix-docstring "Map for ranger operations."
    ("c" . dired-ranger-copy)
    ("p" . dired-ranger-paste)
    ("m" . dired-ranger-move))

  (bind-keys :map dired-mode-map
    ("'" . dired-ranger-bookmark)
    ("`" . dired-ranger-bookmark-visit)))

(use-package dired-narrow
  :commands dired-narrow
  :init
  (bind-key "s" 'dired-narrow dired-mode-map))

(use-package dired-tagsistant
  :init
  (bind-keys :map dired-mode-map
             :prefix "M-t"
             :prefix-map dired-tagsistant-map
             :prefix-docstring "Dired tagsistant map."
    ("t" . dired-tagsistant-tag)
    ("s" . dired-tagsistant-tag-symlink)))

(use-package make-it-so
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
    (bind-key "<f5>" 'mis-save-and-compile makefile-mode-map)))

;;;_. Key bindings & hooks
(defun my-dired-imenu-prev-index-position (&optional arg)
  "Go to the header line of previous directory."
  (interactive "p")
  (unless (= (line-number-at-pos) 1)
    (call-interactively 'dired-prev-subdir)
    t))

(defun my-dired-extract-index-name ()
  "Extract name of the current item for imenu."
  (save-excursion
    (back-to-indentation)
    (buffer-substring-no-properties
     (point)
     (1- (re-search-forward ":$")))))

(defun my-dired-imenu-create-index ()
  "Create `imenu' index for dired."
  (let* ((alist (imenu-default-create-index-function))
         (uniquified (f-uniquify-alist (-map 'car alist))))
    (--remove
     (= 0 (length (car it)))
     (--map (cons (cdr (assoc (car it) uniquified)) (cdr it))
            alist))))

(defun my-dired-imenu-init ()
  "Initialize `imenu' variables in current buffer."
  (setq-local imenu-prev-index-position-function
              'my-dired-imenu-prev-index-position)
  (setq-local imenu-extract-index-name-function
              'my-dired-extract-index-name)
  (setq-local imenu-create-index-function
              'my-dired-imenu-create-index))

(add-hook 'dired-mode-hook 'my-dired-imenu-init)

;; TODO: continue with org here
(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's loaded."
  (when (eq system-type 'windows-nt)
    (set (make-local-variable 'coding-system-for-read) 'cp1250)
    (set (make-local-variable 'file-name-coding-system) 'cp1250))

  (bind-keys :map dired-mode-map
    ;; clean bullshit bindings so C-h e b shows us real info
    ("A") ("F") ("G") ("P") ("Q") ("X") ("Z") ("#") (".")
    ("~") ("e") ("f") ("l") ("v") ("^") ("?") ("<f1>")

    ("(" . dired-hide-details-mode)

    ("C-x C-f" . my-dired-ido-find-file)
    ("j" . my-dired-ido-find-file)
    ("J" . dired-goto-file)
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

  (diredfl-mode 1)
  (dired-filter-mode t)
  (dired-filter-group-mode t)
  (dired-hide-details-mode 1)
  (dired-collapse-mode 1)
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
  (revert-buffer)
  (goto-char (point-min))
  (dired-utils-goto-line (file-truename (ad-get-arg 0))))

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
    (let ((inhibit-read-only t)
          (kill-ring kill-ring)
          (limit (line-end-position)))
      (while (re-search-forward "directory\\|available" nil t)
        (forward-char)
        (when (not (dired-utils-get-filename))
          (let* ((avail (or (word-at-point) "0"))
                 (avail-hr (s-trim (ls-lisp-format-file-size (* 1024 (string-to-number avail)) t 1))))
            (kill-word 1)
            (insert avail-hr)))))))

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
  (if arg
      (dired-do-kill-lines arg fmt)
    (condition-case err
        (if (and (not (cdr (dired-get-marked-files)))
                 (equal (car (dired-get-marked-files))
                        (dired-utils-get-filename)))
            (progn
              (save-excursion) (dired-mark 1)
              (dired-do-kill-lines nil fmt))
          (dired-do-kill-lines arg fmt))
      (error ;; if no file is under point, kill the next subdir
       (my-dired-kill-subdir)))))

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
    (cl-do ((file-size (/ file-size 1024.0) (/ file-size 1024.0))
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

;;;_. Zip support

(add-to-list 'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip"))
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
  (let ((zip-file (if (string-match ".zip$" zip-file)
                      zip-file
                    (concat zip-file ".zip")))
        (process nil))
    (setq process
          (apply 'start-process
                 "zip"
                 (with-current-buffer (get-buffer-create "*dired-arc-pack-files*")
                   (erase-buffer)
                   (goto-char (point-min))
                   (pop-to-buffer (current-buffer))
                   (current-buffer))
                 "zip" "-r" zip-file
                 (--map (file-name-nondirectory it) (dired-get-marked-files))))
    (dired-unmark-all-marks)))

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
  (let* ((marked-files (f-uniquify (dired-get-marked-files))))
    (if (cdr marked-files)
        (dired-do-shell-command "du --apparent-size -s -h -c * &" nil marked-files)
      (dired-do-shell-command "du --apparent-size -s -h" nil marked-files))))

(provide 'dired-defs)
;; dired-defs.el ends here
