;;; Personal functions

(defun my-lorem ()
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Praesent libero orci, auctor sed, faucibus vestibulum, gravida vitae, arcu. Nunc posuere. Suspendisse potenti. Praesent in arcu ac nisl ultricies ultricies. Fusce eros. Sed pulvinar vehicula ante. Maecenas urna dolor, egestas vel, tristique et, porta eu, leo. Curabitur vitae sem eget arcu laoreet vulputate. Cras orci neque, faucibus et, rhoncus ac, venenatis ac, magna. Aenean eu lacus. Aliquam luctus facilisis augue. Nullam fringilla consectetuer sapien. Aenean neque augue, bibendum a, feugiat id, lobortis vel, nunc. Suspendisse in nibh quis erat condimentum pretium. Vestibulum tempor odio et leo. Sed sodales vestibulum justo. Cras convallis pellentesque augue. In eu magna. In pede turpis, feugiat pulvinar, sodales eget, bibendum consectetuer, magna. Pellentesque vitae augue."))

(defun my-where-am-i ()
  (with-temp-buffer
    (insert-file-contents "~/.whereami")
    (buffer-string)))

;; some functions to ease the work with mark and mark-ring
(defun my-push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun my-jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun my-exchange-point-and-mark-no-activate ()
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

(defun my-buffer-narrowed-p ()
  "Return non-nil if the current buffer is narrowed."
  (/= (- (point-max) (point-min)) (buffer-size)))

;; TODO: add support for w3m linky/image
(defun my-find-url (url)
  "Download URL and insert into current buffer at point."
  (interactive "sULR: ")
  (insert (my-url-retrieve url)))

(defun my-url-retrieve (url &optional no-headers)
  "Retrieve and return as string the content of URL.

If NO-HEADERS is non-nil, remove the HTTP headers first."
  (with-current-buffer (url-retrieve-synchronously url)
    (when no-headers
      (goto-char (point-min))
      (search-forward "\n\n")
      (delete-region (point-min) (point)))
    (prog1 (buffer-string)
      (kill-buffer))))

(defun my-unfill-paragraph ()
  "Take a multi-line paragrap and make it into a single line of text.
This is the opposite of fill-paragraph."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun my-remove-dos-eol ()
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

(defun my-opera-to-org-contact (target-buffer)
  (let (name email)
    (save-excursion
      (re-search-forward "NAME=\\(.*?\\)$")
      (setq name (match-string 1))
      (re-search-forward "MAIL=\\(.*?\\)$")
      (setq email (match-string 1)))
    (with-current-buffer target-buffer
      (insert (format "* %s
  :PROPERTIES:
  :EMAIL: %s
  :END:
" name email)))))

(defun my-opera-to-org-contacts (target-buffer)
  (interactive "BTarget buffer: ")
  (setq target-buffer (get-buffer-create target-buffer))
  (goto-char (point-min))
  (while (search-forward "#CONTACT" nil t)
    (my-opera-to-org-contact target-buffer)))

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
    (my-with-every-line
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

;; should go into s.el
(defun my-next-property-value (start prop val &optional object limit)
  "Find next position where PROP has value VAL."
  (let ((point start) done)
    (or (and (equal (plist-get (text-properties-at point object) prop) val)
             point)
        (progn
          (while (and (not done)
                      (if (or (not object)
                              (bufferp object))
                          (/= point (point-max))
                        (/= point (1- (length object))))
                      (setq point (next-single-property-change point prop object limit)))
            (setq done (equal (plist-get (text-properties-at point object) prop) val)))
          point))))

;; should go into s.el
(defun my-next-property-not-value (start prop val &optional object limit)
  "Find next position where PROP does not have value VAL."
  (let ((point start) done)
    (or (and (not (equal (plist-get (text-properties-at point object) prop) val))
             point)
        (progn
          (while (and (not done)
                      (if (or (not object)
                              (bufferp object))
                          (/= point (point-max))
                        (/= point (1- (length object))))
                      (setq point (next-single-property-change point prop object limit)))
            (setq done (not (equal (plist-get (text-properties-at point object) prop) val))))
          point))))

;; ported from gnus-*... should go into s.el
;; TODO: add more general version where we can pass a predicate
(defun my-remove-text-properties-when
  (property value start end properties &optional object)
  "Like `remove-text-properties', only applied on where PROPERTY is VALUE."
  (let (point)
    (setq start (my-next-property-value start property value object end))
    (while (and start
                (< start end)
                (setq point (my-next-property-not-value start property value object end)))
      (remove-text-properties start point properties object)
      (setq start (my-next-property-value point property value object end)))
    object))

(defun my-sprunge (text &optional language)
  "Paste TEXT to sprunge.us.

With non-nil prefix argument, ask for LANGUAGE."
  (interactive (list (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-from-minibuffer "Text: "))
                     (when current-prefix-arg (read-from-minibuffer "Language: " nil
                                                                    nil nil nil "cl"))))
  (let* ((buf (get-buffer-create " *sprunge-result*"))
         (url (with-current-buffer buf
                (shell-command (concat "echo "
                                       (shell-quote-argument text)
                                       " | curl -s -F 'sprunge=<-' http://sprunge.us")
                               buf)
                (s-trim (buffer-string)))))
    (kill-buffer buf)
    (kill-new (if language (concat url "?" language) url))))

(defun my-update-mpd ()
  "Update the mpd database using the common parent of current dired listing."
  (interactive)
  (start-process "mpc" nil
                 "mpc" "update" (s-chop-prefix (f-canonical (concat dired-list-mpc-music-directory "/"))
                                               (f-common-parent (dired-utils-get-all-files)))))

(defun my-shuffle-things (thing beg end)
  (goto-char beg)
  (let ((cur (cons 0 beg))
        (things nil)
        (vthings))
    (while (< (cdr cur) end)
      (setq cur (bounds-of-thing-at-point thing))
      (push cur things)
      (forward-thing thing 2)
      (forward-thing thing -1))
    (setq things (nreverse things))
    (setq vthings (apply 'vector things))
    (let ((len (length vthings)))
      (message "%s" len)
      (--dotimes len
        (let* ((r (random (- len it)))
               (tmp (elt vthings r)))
          (aset vthings r (elt vthings (- len it 1)))
          (aset vthings (- len it 1) tmp))))
    (let* ((orig (current-buffer))
           (replacement
            (with-temp-buffer
              ;; todo: grab whitespace around it
              (cl-loop for x across vthings do
                       (insert (with-current-buffer orig
                                 (buffer-substring (car x) (cdr x)))))
              (buffer-string))))
      (delete-region beg end)
      (goto-char beg)
      (insert replacement))))

(defun my-shuffle-lines (beg end)
  "Shuffle lines in the active region."
  (interactive "r")
  (my-shuffle-things 'line beg end))

(defun my-shuffle-words (beg end)
  "Shuffle words in the active region."
  (interactive "r")
  (my-shuffle-things 'word beg end))

(defvar my-abbrev-file-name-alist
  `((,abbreviated-home-dir . "~/")
    ("~/languages/" . "L|")
    ("/usr/local/share/emacs/24.3/lisp/" . "E|")
    ("~/dev/tex/fic/" . "FIC|")
    ("~/.emacs.d/elpa/" . "ELPA|")
    ("~/.emacs.d/" . "ED|")
    ("/var/www/html/devel/" . "WEBD|")
    ("/var/www/html/" . "WEB|")
    ("/modules/source/" . "|MOD-S|")
    ("/specific/source/" . "|SP-S|"))
  "An alist defining translations of paths to shortcuts.")

(defun my-abbrev-file-name (string)
  (save-match-data
    (-each my-abbrev-file-name-alist
      (-lambda ((from . to))
        (when (string-match from string)
          (setq string (replace-match to nil nil string)))
        (when (string-match (concat "|" (substring from 1)) string)
          (setq string (replace-match to nil nil string))))))
  string)

(defvar my-status-line-format
  '((:eval (and (featurep 'tracking)
                (let* ((shortened (tracking-shorten tracking-buffers))
                       (urgent-buffers (--filter (text-property-any 0 (length it)
                                                                    'face 'circe-highlight-nick-face
                                                                    it)
                                                 shortened))
                       (status (concat "<fc=#ef2929>["
                                       (mapconcat 'identity urgent-buffers ",")
                                       "]</fc>")))
                  (when urgent-buffers (substring-no-properties status 0 (length status))))))
    (:eval (and (featurep 'elfeed)
                (format "<fc=#75507b>%d</fc>" my-elfeed-unread-count)))
    (:eval (and (featurep 'notmuch)
                (let ((count (notmuch-unread-count)))
                  (if (> count 0) (format "<fc=#ef2929>[✉ %d]</fc>" count) ""))))
    (org-timer-mode-line-timer
     (:eval (format "<fc=%s><%s></fc>"
                    (let ((time (abs (floor (org-timer-seconds)))))
                      (cond
                       ((< time 30) "#ef2929")
                       ((< time 60) "#f57900")
                       (t "#8cc4ff")))
                    (substring (org-timer-value-string) 0 -1))))
    (:eval (and (featurep 'org)
                (cond
                 ((not (marker-buffer org-clock-marker))
                  "<fc=#d3d7cf>-:--</fc>")
                 (t
                  (let* ((status (substring-no-properties org-mode-line-string 1
                                                          (1- (length org-mode-line-string))))
                         (split-status (split-string status " (")))
                    (concat "<fc=#8ae234>" (car split-status) "</fc>")))))))
  "A format construct following the conventions of
`mode-line-format' used to produce a status for xmobar.")

(defun my-emacs-status ()
  (format-mode-line my-status-line-format))

(defun my-toggle-buffer-input-methods ()
  "Toggle the input methods listed in `my-buffer-input-methods'."
  (interactive)
  (when (bound-and-true-p my-buffer-input-methods)
    (let ((next (car my-buffer-input-methods)))
      (setq my-buffer-input-methods (-rotate 1 my-buffer-input-methods))
      (set-input-method next))))

(defun my-switch-buffer-LRU ()
  "Switch to LRU buffer."
  (interactive)
  (switch-to-buffer (car (helm-buffer-list))))
