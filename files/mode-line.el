;; set frame format
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

   ;; Path to the file in buffer. If it doesn't have associated file,
   ;; display nothing.
   (:propertize (:eval
                 (when (buffer-file-name)
                   (my-mode-line-construct-path (buffer-file-name) (buffer-name))))
    face mode-line-secondary)

   ;; buffer name
   (:propertize (:eval (my-mode-line-apply-substitutions (buffer-name))) face mode-line-buffer-id)

   ;; activated modes
   "    %[("
   mode-name
   mode-line-process
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-secondary))
   ")%]"

   ;; version control
   (vc-mode vc-mode)

   " (" mode-line-mule-info ")"
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

(defvar my-abbrev-file-name-alist
  `((,abbreviated-home-dir . "~/")
    ("^~/languages/" . "L|")
    ("^/usr/local/share/emacs/24.3/lisp/" . "E|")
    ("~/dev/tex/fic/" . "FIC|")
    ("~/.emacs.d/elpa/" . "ELPA|")
    ("~/.emacs.d/" . "ED|")
    ("/var/www/html/agrocs/" . "AGROCS|")
    ("/var/www/html/devel" . "WEBD|")
    ("/var/www/html/" . "WEB|")
    ("/modules/source/" . "|MOD-S|")
    ("/specific/source/" . "|SP-S|"))
  "An alist defining translations of paths to shortcuts.")

(defun my-mode-line-apply-substitutions (string)
  (save-match-data
    (-each my-abbrev-file-name-alist
      (-lambda ((from . to))
        (when (string-match from string)
          (setq string (replace-match to nil nil string))))))
  string)

(defun my-mode-line-construct-path (buffer-file-name buffer-name)
  (let* ((buffer-file-name (my-mode-line-apply-substitutions buffer-file-name))
         (buffer-name (my-mode-line-apply-substitutions buffer-name)))
    (s-chop-suffix buffer-name buffer-file-name)))

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
