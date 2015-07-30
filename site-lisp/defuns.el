;;; defuns.el --- Personal functions

;;; Commentary:
;;; Code:

(require 'dired)
(require 'dired-list)

(defun my-lorem ()
  "Insert lorem ipsum text."
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

(defun my-replace-match (newtext old)
  "Replace text matched by last search with NEWTEXT.

OLD is the string to act on."
  (let* ((b (match-beginning 0))
         (tp (text-properties-at b old))
         (re (replace-match newtext nil nil old)))
    (add-text-properties b (+ b (length newtext)) tp re)
    re))

(defvar my-abbrev-file-name-alist
  `((,abbreviated-home-dir . "~/")
    ("~/languages/" . "L|")
    ("/usr/local/share/emacs/24.3/lisp/" . "E|")
    ("~/dev/tex/fic/" . "FIC|")
    ("~/.emacs.d/elpa/" . "ELPA|")
    ("~/.emacs.d/" . "ED|")
    ("/var/www/html/devel/" . "WEBD|")
    ("/var/www/html/" . "WEB|")
    ("/modules/source/" . "|MOD|")
    ("/specific/source/" . "|SP|"))
  "An alist defining translations of paths to shortcuts.")

(defun my-abbrev-file-name (string)
  (save-match-data
    (-each my-abbrev-file-name-alist
      (-lambda ((from . to))
        (when (string-match from string)
          (setq string (my-replace-match to string)))
        (when (string-match (concat "|" (substring from 1)) string)
          (setq string (my-replace-match to string))))))
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
  (let* ((interesting-buffers (cdr (--drop-while (s-starts-with-p " " (buffer-name it)) (buffer-list))))
         (next (--first (not (s-starts-with-p " " (buffer-name it))) interesting-buffers)))
    (switch-to-buffer next)))

(defun my-svn-get-current-url ()
  "Return svn url representing current buffer."
  (vc-svn-repository-hostname (buffer-file-name)))

(defun my-svn-get-trunk-url (&optional branch-url)
  "Return trunk url for file represented by BRANCH-URL."
  (setq branch-url (or branch-url (my-svn-get-current-url)))
  (replace-regexp-in-string my-svn-branch my-svn-trunk branch-url))

(defun my-svn-diff-branch-and-trunk ()
  "Diff current buffer's file with its trunk version, run `diff-mode' on result."
  (interactive)
  (let* ((branch (vc-svn-repository-hostname (buffer-file-name)))
         (trunk (my-svn-get-trunk-url branch)))
    (with-current-buffer (get-buffer-create "*svndiff*")
      (erase-buffer)
      (vc-svn-command t t nil "diff" "-x -w" trunk branch)
      (diff-mode)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(defun my-svn-checkout-trunk-file-tmp ()
  "Checkout the trunk version of current file into a temporary file.

Return path to temporary file."
  (let ((cb (current-buffer))
        (ext (concat "." (file-name-extension (buffer-file-name)))))
    (with-temp-buffer
      (let ((tmp-file (concat (make-temp-file "my-svn-") ext)))
        (vc-svn-command t t nil "export"
                        (with-current-buffer cb
                          (my-svn-get-trunk-url))
                        tmp-file)
        tmp-file))))

(defun my-svn-ediff-branch-and-trunk ()
  "Diff current buffer's file with its trunk version, run `ediff' on result."
  (interactive)
  (let ((trunk-file (concat (or (file-remote-p (buffer-file-name)) "") (my-svn-checkout-trunk-file-tmp)))
        (buffer (concat "*trunk " (f-filename (buffer-file-name)) "*")))
    (ediff-buffers
     (with-current-buffer (get-buffer-create buffer)
       (erase-buffer)
       (insert-file-contents trunk-file)
       (setq buffer-file-name trunk-file)
       (set-auto-mode)
       (setq buffer-file-name nil)
       (current-buffer))
     (current-buffer))))

(defun my-occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (thing-at-point 'symbol))
        regexp-history)
  (call-interactively 'occur))


(defun my-insert-date-iso (date)
  "Insert timestamp."
  (interactive (list (org-read-date)))
  (insert date))

(defun my-fit-window-to-buffer (w)
  "Just like `fit-window-to-buffer' but with max pre-set.

The max height is set to the minimum of `count-screen-lines' and
half the height of parent window."
  (fit-window-to-buffer
   w
   (min
    (1+ (count-screen-lines nil nil nil w))
    (/ (window-height (window-parent w)) 2))))

(defun my-visit-init-file ()
  "Visit init file."
  (interactive)
  (find-file user-init-file))

(defun my-find-file-in-home ()
  "Find file in the home directory"
  (interactive)
  (let ((default-directory "~"))
    (ido-find-file)))

(defun my-md5-file (filename)
  "Open FILENAME, load it into a buffer and generate the md5 of its contents"
  (interactive "f")
  (with-temp-buffer
    (insert-file-contents filename)
    (md5 (current-buffer))))

(defun my-dired-show-duplicates (list-a list-b)
  (interactive (list (dired-get-marked-files)
                     (with-current-buffer (cdr (assoc (dired-dwim-target-directory) dired-buffers))
                       (dired-get-marked-files))))
  (let* ((list-a (--map (cons (my-md5-file it) it) list-a))
         (list-b (--map (cons (my-md5-file it) it) list-b))
         (same-files 0))
    (with-current-buffer (get-buffer-create "*duplicates*")
      (erase-buffer)
      (--each list-a
        (-when-let (name-other (cdr (assoc (car it) list-b)))
          (insert (format "- File =%s= is the same as =%s=\n" (cdr it) name-other))
          (incf same-files)))
      (insert (format "Total: %d\n" same-files))
      (org-mode)
      (pop-to-buffer (current-buffer)))))

;;; defuns.el ends here
