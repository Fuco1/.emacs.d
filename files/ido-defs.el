;;; ido-defs.el --- Personal ido definitions
;;; Commentary:
;;; Code:

(require 'ido)
(use-package ido-completing-read+ :straight t)
(use-package flx-ido :straight t)

(ido-mode 1)
(ido-ubiquitous-mode 1)
(flx-ido-mode 1)

;; Display ido results vertically, rather than horizontally
(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

;; see: http://emacswiki.org/emacs/InteractivelyDoThings#toc25
(defun ido-smart-select-text ()
  "Select the current completed item.  Do NOT descend into directories."
  (interactive)
  (when (and (or (not ido-require-match)
                 (if (memq ido-require-match
                           '(confirm confirm-after-completion))
                     (if (or (eq ido-cur-item 'dir)
                             (eq last-command this-command))
                         t
                       (setq ido-show-confirm-message t)
                       nil))
                 (ido-existing-item-p))
             (not ido-incomplete-regexp))
    (when ido-current-directory
      (setq ido-exit 'takeprompt)
      (unless (and ido-text (= 0 (length ido-text)))
        (let ((match (ido-name (car ido-matches))))
          (throw 'ido
                 (setq ido-selected
                       (if match
                           (replace-regexp-in-string "/\\'" "" match)
                         ido-text)
                       ido-text ido-selected
                       ido-final-text ido-text)))))
    (exit-minibuffer)))

(defun my-ido-keys ()
  (bind-key "C-<tab>" 'ido-smart-select-text ido-file-dir-completion-map))

(add-hook 'ido-setup-hook 'my-ido-keys)

(defun my-imenu-build-table ()
  "Build flat \"imenu\" table."
  (save-excursion
    (let (re)
      (-each imenu-generic-expression
        (-lambda ((_ regexp group))
          (goto-char (point-min))
          (while (re-search-forward regexp nil t)
            (push (cons (match-string group) (match-beginning group)) re))))
      re)))

(defun ido-goto-symbol ()
  (interactive)
  (if imenu-generic-expression
      (let* ((list (my-imenu-build-table))
             (default (symbol-name (symbol-at-point)))
             (choice (completing-read "Go to symbol: " list nil t nil nil (when (assoc default list) default))))
        (when choice
          (unless (and (boundp 'mark-active) mark-active)
            (push-mark nil t nil))
          (goto-char (cdr (assoc choice list)))))
    (ido-goto-symbol-old)))

;; ido and imenu integration
(defun ido-goto-symbol-old (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (setq word-under-cursor (thing-at-point 'symbol))
          (setq fix-the-order (not (null (member word-under-cursor symbol-names))))
          (add-to-list 'symbol-names name)
          (when fix-the-order
            (delete word-under-cursor symbol-names)
            (add-to-list 'symbol-names word-under-cursor))
          (add-to-list 'name-and-pos (cons name position))))))))

;; sort ido filelist by mtime instead of alphabetically
(defun ido-sort-mtime ()
  (unless (and (featurep 'tramp)
               (tramp-tramp-file-p ido-current-directory))
    (setq ido-temp-list
          (sort ido-temp-list
                (lambda (a b)
                  (cond
                   ((not (file-exists-p a)) nil)
                   ((not (file-exists-p b)) nil)
                   (t (time-less-p
                       (sixth (file-attributes (concat ido-current-directory b)))
                       (sixth (file-attributes (concat ido-current-directory a))))))))))
  (ido-to-end  ;; move . files to end (again)
   (--select (char-equal (string-to-char it) ?.) ido-temp-list))
  (when ido-show-dot-for-dired
    (when (caddr ido-temp-list)
      (setq ido-temp-list
            (cons "." (--remove (equal it ".") ido-temp-list))))))

(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)
