(use-package flex-isearch
  :defer t
  :load-path "~/.emacs.d/vendor/flex-isearch/"
  :config
  (progn
    (defadvice isearch-forward-regexp (around flex-isearch activate)
      (when (and flex-isearch-mode
                 (equal (ad-get-arg 0) '(16)))
        (flex-isearch-activate)
        (ad-set-arg 0 '(4)))
      ad-do-it)

    (defadvice isearch-backward-regexp (around flex-isearch activate)
      (when (and flex-isearch-mode
                 (equal (ad-get-arg 0) '(16)))
        (flex-isearch-activate)
        (ad-set-arg 0 '(4)))
      ad-do-it)))

(defun my-isearch-forward ()
  "Repeat the forward search.

If called from backward search, immediately jump to the following
occurance instead of jumping to the end of current one."
  (interactive)
  (when (not isearch-forward)
    (isearch-repeat 'forward))
  (isearch-repeat 'forward))

(defun my-isearch-backward ()
  "Repeat the backward search.

If called from forward search, immediately jump to the previous
occurance instead of jumping to the beginning of current one."
  (interactive)
  (when (and isearch-forward
             (/= (point) isearch-other-end))
    (isearch-repeat 'backward))
  (isearch-repeat 'backward))

(defun my-isearch-forward-to-beginning ()
  "Repeat the forward search and place the point before the
matched text."
  (interactive)
  (when (not isearch-forward)
    (isearch-repeat 'forward))
  (isearch-repeat 'forward)
  (goto-char isearch-other-end))

(defun my-isearch-kill-match ()
  "Kill the text last matched by isearch."
  (interactive)
  (isearch-exit)
  (kill-region (point) isearch-other-end))

(defun my-isearch-zap-to-match ()
  "Kill the region between the point of origin of isearch and the
closest portion of the isearch match string.  The behaviour is
meant to be analogous to zap-to-char.  The deleted region does
not include the isearch word.  This is meant to be bound only in
isearch mode."
  (interactive)
  (let* ((isearch-bounds (list isearch-other-end (point)))
         (ismin (apply 'min isearch-bounds))
         (ismax (apply 'max isearch-bounds))
         (beg isearch-opoint))
    (cond
     ((< beg ismin)
      (goto-char ismin)
      (kill-region beg ismin))
     ((> beg ismax)
      (goto-char beg)
      (kill-region ismax beg))
     (t
      (error "Internal error in isearch kill function.")))
    (isearch-exit)))

(defun my-isearch-exit-other-end ()
  "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))

(defun my-isearch-yank-symbol ()
  "Pull next symbol from buffer into search string."
  (interactive)
  (isearch-yank-internal (lambda () (sp-forward-symbol) (point))))

(defun my-isearch-exit ()
  "Exit search normally.

If in dired buffer, find file under cursor.  If it is a
directory, go right back into search."
  (interactive)
  (isearch-exit)
  (if (eq major-mode 'dired-mode)
      ;; we're assuming no files are marked
      (let ((d (dired-get-filename)))
        (dired-find-file)
        (when (file-directory-p d)
          (dired-isearch-filenames)))))

;; keybindings
(bind-key "<f6>" 'replace-regexp)

(bind-key "C-v" 'my-isearch-forward-to-beginning isearch-mode-map)
(bind-key "C-r" 'my-isearch-backward             isearch-mode-map)
(bind-key "C-s" 'my-isearch-forward              isearch-mode-map)
(bind-key "C-2" 'my-isearch-kill-match           isearch-mode-map)
(bind-key "C-3" 'my-isearch-exit-other-end       isearch-mode-map)
(bind-key "C-w" 'my-isearch-yank-symbol          isearch-mode-map)
(bind-key "C-M-w" 'isearch-yank-word-or-char     isearch-mode-map)
(bind-key "M-z" 'my-isearch-zap-to-match         isearch-mode-map)
(bind-key "<f2>" 'isearch-occur                  isearch-mode-map)
(bind-key "\r" 'my-isearch-exit                  isearch-mode-map)

(bind-key "C-S-s" 'flex-isearch-forward)
(bind-key "C-S-r" 'flex-isearch-backward)
