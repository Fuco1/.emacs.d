;; set frame format

(require 's)

(setq global-mode-string '(""))
(add-to-list 'mode-line-misc-info '(tracking-mode ("" tracking-mode-line-buffers " ")) t)

(setq-default
 frame-title-format
 '("%b ; %*"
   (:eval (when (buffer-file-name)
            (concat " ; "
                    (my-mode-line-construct-path (buffer-file-name) (buffer-name)))))
   " - Emacs"))

;; set line format
(setq-default
 mode-line-format
 '(" "
   ;; show only if the buffer is read-only or modified.
   (:eval
    (cond (buffer-read-only
           (propertize "RO" 'face 'font-lock-keyword-face))
          ((buffer-modified-p)
           (propertize "**" 'face 'mode-line-modified-status))
          (t "  ")))

   ;; evil support
   (:eval (when (bound-and-true-p evil-mode)
            (evil-generate-mode-line-tag evil-state)))
   " "
   ;; cursor position & narrow info
   (:eval (when (buffer-narrowed-p)
            "Narrow "))
   (-3 "%p")
   " "
   (17 (:eval (if (use-region-p)
                  (format "(wc:%d,%d,%d)"
                          (abs (- (point) (mark)))
                          (count-words-region (point) (mark))
                          (abs (- (line-number-at-pos (point))
                                  (line-number-at-pos (mark)))))
                (format "(%%l,%%c,%d)" (point)))))

   "%["
   (:eval (my-mode-line-construct-path-1 (buffer-file-name) (buffer-name)))
   "%]"

   ;; version control
   (vc-mode (" (" (:eval (substring vc-mode 1)) ")"))

   ;; we don't need all the `mode-line-mule-info', just display the
   ;; input method (we can see the encoding in C-u C-x =), and the
   ;; end-of-line info is boring
   " " (current-input-method (:propertize ("(" current-input-method-title ")")))
   " " mode-line-misc-info

   ;; hack to make the modeline refresh after each change in buffer
   (:propertize "%l" face (:foreground "black" :height 0))
   ))

(setq-default 2C-mode-line-format mode-line-format)

(defface mode-line-secondary
  '((t (:foreground "#555753")))
  "Face used for displaying secondary content (minor modes, file path etc.)")

(defface mode-line-modified-status
  '((t (:inherit font-lock-warning-face :weight normal)))
  "Face used for modified status in mode line.")

(defun my-mode-line-construct-path (buffer-file-name buffer-name)
  (let* ((buffer-file-name (my-abbrev-file-name buffer-file-name))
         (buffer-name (my-abbrev-file-name buffer-name)))
    (s-chop-suffix buffer-name buffer-file-name)))

(defun my-mode-line-construct-path-1 (buffer-file-name buffer-name)
  (if (not buffer-file-name)
      (propertize buffer-name 'face 'mode-line-buffer-id)
    (let ((bfns (s-split "/" buffer-file-name))
          (bns (s-split "/" buffer-name))
          (r nil))
      (--each bfns
        (if (equal it (car bns))
            (progn
              (push (propertize (copy-sequence it) 'face 'mode-line-buffer-id) r)
              (!cdr bns))
          (push (propertize (copy-sequence it) 'face 'mode-line-secondary) r)))
      (setq r (nreverse r))
      (let ((ml (mapconcat 'identity r (propertize "/" 'face 'mode-line-secondary))))
        (my-abbrev-file-name ml)))))

(defvar minimal-mode-line-background "darkred"
  "Background colour for active mode line face when minimal minor
  mode is active")

(defvar minimal-mode-line-inactive-background "dim grey"
  "Background colour for inactive mode line face when minimal
  minor mode is active")

(defvar minimal-mode-line-height 0.1
  "Height of mode line when minimal minor mode is active")

(unless (facep 'minimal-mode-line)
  (copy-face 'mode-line 'minimal-mode-line))
(set-face-attribute 'minimal-mode-line nil
                    :background "darkred"
                    :height 0.1)

;;;;;;; emacs-status for xmobar
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
    ("/specific/source/" . "|SP|")
    ("/extensions/" . "|E|")
    )
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
  '((:eval (and (bound-and-true-p my-elfeed-unread-count)
                (format "<fc=#75507b>%d</fc>" my-elfeed-unread-count)))
    (:eval (and (require 'notmuch nil t)
                (let ((count (notmuch-unread-count)))
                  (if (> count 0) (format "<fc=#729fcf>[✉ %d]</fc>" count) ""))))
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
                  (let* ((status (substring-no-properties org-mode-line-string 2
                                                          (1- (length org-mode-line-string))))
                         (split-status (split-string status " (")))
                    (concat "<fc=#8ae234>" (replace-regexp-in-string "[]]" "" (car split-status)) "</fc>")))))))
  "A format construct following the conventions of
`mode-line-format' used to produce a status for xmobar.")

(defun my-emacs-status ()
  "Format emacs status line."
  (format-mode-line my-status-line-format))
