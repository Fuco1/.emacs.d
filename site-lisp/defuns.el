;;; Personal functions

(defun lorem ()
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Praesent libero orci, auctor sed, faucibus vestibulum, gravida vitae, arcu. Nunc posuere. Suspendisse potenti. Praesent in arcu ac nisl ultricies ultricies. Fusce eros. Sed pulvinar vehicula ante. Maecenas urna dolor, egestas vel, tristique et, porta eu, leo. Curabitur vitae sem eget arcu laoreet vulputate. Cras orci neque, faucibus et, rhoncus ac, venenatis ac, magna. Aenean eu lacus. Aliquam luctus facilisis augue. Nullam fringilla consectetuer sapien. Aenean neque augue, bibendum a, feugiat id, lobortis vel, nunc. Suspendisse in nibh quis erat condimentum pretium. Vestibulum tempor odio et leo. Sed sodales vestibulum justo. Cras convallis pellentesque augue. In eu magna. In pede turpis, feugiat pulvinar, sodales eget, bibendum consectetuer, magna. Pellentesque vitae augue."))

;; some functions to ease the work with mark and mark-ring
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(defun view-clipboard ()
  (interactive)
  (switch-to-buffer "*Clipboard*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (clipboard-yank)
    (goto-char (point-min))
    (html-mode)
    (view-mode)))

(defsubst buffer-narrowed-p ()
  "Return non-nil if the current buffer is narrowed."
  (/= (- (point-max) (point-min)) (buffer-size)))

;;; opening files
(defun my-find-file-same-ext (filename)
  "Find files with same extension as the file in current buffer."
  (interactive (list (let ((ext (file-name-extension (buffer-file-name))))
                       (completing-read
                        (format "Find file [.%s]: " ext)
                        (directory-files default-directory)
                        `(lambda (f) (equal (file-name-extension f)
                                            ,ext))
                        t))))
  (find-file filename))

(defun my-find-file-same-mode (filename)
  "Find files that would open in the same `major-mode' as current buffer."
  (interactive (list (progn
                       (require 'dired-hacks-utils)
                       (completing-read
                        (format "Find file [major-mode %s]: " major-mode)
                        (directory-files default-directory)
                        `(lambda (f)
                           (-when-let (mm (cdr (dired-utils-match-filename-regexp
                                                f auto-mode-alist)))
                             (eq mm ',major-mode)))
                        t))))
  (find-file filename))

(defun my-find-file-sudo (filename)
  "Open the file through sudo(1).

With \\[universal-argument], visit current file via sudo."
  (interactive
   (list (if current-prefix-arg (buffer-file-name)
           (read-file-name "Find file: " nil default-directory
                           (confirm-nonexistent-file-or-buffer)))))
  (find-file (concat "/sudo::" filename)))

;; TODO: add support for w3m linky/image
(defun my-find-url (url)
  "Download URL and insert into current buffer at point."
  (interactive "sULR: ")
  (insert (progn
            (with-current-buffer (url-retrieve-synchronously url)
              (buffer-string)))))

;;; function overloads
(eval-after-load "hi-lock"
  '(progn
     (defun hi-lock-read-face-name ()
       "Read face name from minibuffer with completion and history."
       (intern (completing-read
                "Highlight using face: "
                (mapcar 'symbol-name (face-list))
                nil
                nil
                "hi-"
                'face-name-history
                (car hi-lock-face-defaults))))))

;; By Stefan Monnier <foo at acm.org>.
(defun unfill-paragraph ()
  "Take a multi-line paragrap and make it into a single line of text.
This is the opposite of fill-paragraph."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun* my-list-interactive (&optional (file-name (buffer-file-name)))
  "Return a list of all interactive functions in file FILE-NAME."
  (loop for i in (cdr (assoc-string file-name load-history))
           if (and (consp i) (eq (car i) 'defun) (commandp (cdr i)))
           collect (cdr i)))

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun my-assimil--format-line (fill)
  (let ((fill-column 38)
        (fill-prefix fill))
    (fill-region (line-beginning-position) (line-end-position))))

(defun my-assimil-format (arg)
  (interactive "p")
  (dotimes (i arg)
    (my-assimil--format-line "     ")
    (forward-line -1)))

(defun my-assimil-format-ubung (arg)
  (interactive "p")
  (dotimes (i arg)
    (my-assimil--format-line "   ")
    (forward-line -1)))

(defun my-assimil-insert-dialog-template (arg)
  (interactive "p")
  (dotimes (i arg)
    (insert (format "%2d - \n" (1+ i)))))

(defun my-assimil-insert-ubung-template (arg)
  (interactive "p")
  (dotimes (i arg)
    (insert (format "%d. \n" (1+ i)))))

(defun my-format-synonyms-from-wiki ()
  (interactive)
  (goto-char (point-min))
  (search-forward "Synonyms[edit]")
  (delete-region (line-beginning-position) (line-end-position))
  (delete-char 1)
  (while (< (point) (point-max))
    (sp-down-sexp)
    (let ((number (string-to-number (word-at-point))))
      (sp-up-sexp)
      (save-excursion
        (let ((text (delete-and-extract-region (point) (line-end-position))))
          (goto-line number)
          (goto-char (line-end-position))
          (insert " (syn:" text ")")))
      (delete-region (line-beginning-position) (line-end-position))
      (delete-char 1)))
  (when (looking-at "$")
    (delete-char 1)))

(defun my-no-mode-find-file (filename)
  (interactive (list (ido-read-file-name
                      "File: "
                      (or (and (eq major-mode 'dired-mode)
                               (dired-current-directory))
                          default-directory))))
  (let ((auto-mode-alist nil))
    (find-file filename)))

(defun my-opera-bookmarks-extract-property (prop beg end)
  (save-excursion
    (goto-char beg)
    (when (and (re-search-forward prop end t)
               (looking-at "="))
      (forward-char)
      (buffer-substring-no-properties (point) (line-end-position)))))

(defun my-opera-bookmarks-to-org (buffer)
  (with-current-buffer buffer
    (erase-buffer))
  (let ((depth 0))
    (while (re-search-forward (concat "^-$\\|" (regexp-opt '("#FOLDER" "#URL"))) nil t)
      (cond
       ((equal (match-string 0) "#FOLDER")
        (cl-incf depth)
        (forward-line)
        (let ((beg (point)))
          (re-search-forward "^$" nil t)
          (let* ((end (point))
                 (name (my-opera-bookmarks-extract-property "NAME" beg end))
                 (created (my-opera-bookmarks-extract-property "CREATED" beg end)))
            (with-current-buffer buffer
              (insert
               (format "%s %s                                                                :folder:
    :PROPERTIES:
    :CREATED:  %s
    :END:

" (make-string depth ?*) name created))))))
       ((equal (match-string 0) "#URL")
        (forward-line)
        (let ((beg (point)))
          (re-search-forward "^$" nil t)
          (let* ((end (point))
                 (name (my-opera-bookmarks-extract-property "NAME" beg end))
                 (url (my-opera-bookmarks-extract-property "URL" beg end))
                 (desc (my-opera-bookmarks-extract-property "DESCRIPTION" beg end))
                 (created (my-opera-bookmarks-extract-property "CREATED" beg end))
                 (visited (my-opera-bookmarks-extract-property "VISITED" beg end)))
            (with-current-buffer buffer
              (insert
               (format "%s %s
    :PROPERTIES:
    :CREATED:  %s
    :VISITED:  %s
    :END:
- source: %s
" (make-string (1+ depth) ?*) name created visited url)
               (if desc (format "- description: %s\n" desc) "")
               "\n")))))
       ((equal (match-string 0) "-")
        (cl-decf depth))))))

(defun my-dired-insert-git-ls-files (path)
  (let* ((full-path (file-truename (concat path "/")))
         (default-directory full-path)
         (buf (with-current-buffer (get-buffer-create " git-ls-files")
                (erase-buffer)
                (current-buffer))))
    (call-process "git"
                  nil
                  buf
                  nil
                  "ls-files")
    (let* ((data (with-current-buffer buf
                   (goto-char (point-min))
                   (when (> (point-max) 0)
                     (insert "\"")
                     (end-of-line)
                     (insert "\"")
                     (while (and (= 0 (forward-line))
                                 (not (eobp)))
                       (insert "\"")
                       (end-of-line)
                       (insert "\"")))
                   (goto-char (point-min))
                   (replace-regexp "\n" " ")
                   (goto-char (point-min))
                   (insert "(")
                   (goto-char (point-max))
                   (insert ")")
                   (buffer-substring-no-properties (point-min) (point-max))))
           (list (read data)))
      (message "%S" list)
      (--map (insert-directory it "-la") list))))

(defun my-extract-buffer-substring (end-pattern)
  "Extract buffer substring between current point and END-PATTERN.

END-PATTERN should be a regular expression delimiting the
substring.  The match is extracted up until the beginning of the
first match.

The point is moved to the end of the match."
  (save-match-data
    (buffer-substring-no-properties
     (point)
     (when (re-search-forward end-pattern nil t)
       (match-beginning 0)))))

(defun my-fix-german-anki-deck-get-plural ()
  (when (and (search-forward "German\n---" nil t)
             (search-forward "noun[" nil t))
    (search-forward "," nil t 2)
    (when (looking-at "gen2")
      (search-forward "," nil t))
    (my-extract-buffer-substring "[],]")))

(defun my-fix-german-anki-deck-get-gender ()
  (when (and (search-forward "German\n---" nil t)
             (search-forward "noun[" nil t))
    (my-extract-buffer-substring ",")))

(defun my-fix-german-anki-deck ()
  (let ((case-fold-search nil)
        (curbuf (current-buffer)))
    (with-every-line
      (when (and (looking-at "[A-Z]")
                 (progn
                   (forward-word)
                   (looking-at "\t")))
        (save-window-excursion
          (call-interactively 'wd-show-translation)
          (with-current-buffer (get-buffer-create "*Wiktionary*")
            (let ((gender (save-excursion (my-fix-german-anki-deck-get-gender)))
                  (plural (save-excursion (my-fix-german-anki-deck-get-plural))))
              (with-current-buffer curbuf
                (beginning-of-line)
                (when gender
                  (insert (cond
                           ((equal gender "m") "der ")
                           ((equal gender "f") "die ")
                           ((equal gender "n") "das "))))
                (when plural
                  (search-forward "\t" nil t)
                  (backward-char)
                  (insert ", " plural))))))))))

(defun my-emacs-status ()
  (let ((org-active-task (cond
                          ((not (marker-buffer org-clock-marker))
                           "<fc=#d3d7cf>-:--</fc>")
                          (t
                           (let* ((status (substring-no-properties org-mode-line-string 1
                                                                   (1- (length org-mode-line-string))))
                                  (split-status (split-string status " (")))
                             (concat "<fc=#8ae234>" (car split-status) "</fc>")))))
        (unread-mail-count (let ((count (notmuch-unread-count)))
                             (if (> count 0) (format "<fc=#ef2929>[✉ %d]</fc>" count) ""))))
    (concat unread-mail-count org-active-task)))
