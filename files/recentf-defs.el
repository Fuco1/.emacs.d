(defun my-recentf-setup ()
  (bind-key "n" 'my-recentf-next-file recentf-dialog-mode-map)
  (bind-key "p" 'my-recentf-previous-file recentf-dialog-mode-map)
  (bind-key "C-s" 'my-recentf-isearch-forward recentf-dialog-mode-map))

(add-hook 'recentf-dialog-mode-hook 'my-recentf-setup)

(defun my-recentf-next-file (&optional arg)
  (interactive "p")
  (forward-line arg)
  (re-search-forward ".*/\\(.*?\\)$" nil t)
  (goto-char (match-beginning 1)))

(defun my-recentf-previous-file (&optional arg)
  (interactive "p")
  (forward-line (- arg))
  (re-search-forward ".*/\\(.*?\\)$" nil t)
  (goto-char (match-beginning 1)))

(defvar recentf-isearch-filter-predicate-orig nil)
(defvar recentf-isearch-filenames nil)

(defun recentf-isearch-filenames-setup ()
  "Set up isearch to search in recentf file names.
Intended to be added to `isearch-mode-hook'."
  (when recentf-isearch-filenames
    (setq isearch-message-prefix-add "filename ")
    (setq recentf-isearch-filter-predicate-orig
          (default-value 'isearch-filter-predicate))
    (setq-default isearch-filter-predicate 'recentf-isearch-filter-filenames)
    (add-hook 'isearch-mode-end-hook 'recentf-isearch-filenames-end nil t)))

(add-hook 'isearch-mode-hook 'recentf-isearch-filenames-setup)

(defun recentf-isearch-filenames-end ()
  "Clean up the Recentf file name search after terminating isearch."
  (setq isearch-message-prefix-add nil)
  (setq-default isearch-filter-predicate recentf-isearch-filter-predicate-orig)
  (setq recentf-isearch-filenames nil)
  (remove-hook 'isearch-mode-end-hook 'recentf-isearch-filenames-end t))

(defun recentf-isearch-filter-filenames (beg end)
  "Test whether the current search hit is a visible file name.
Return non-nil if the text from BEG to END is part of a file
name (has the text property `recentf-filename') and is visible."
  (and (isearch-filter-visible beg end)
       (if recentf-isearch-filenames
           (text-property-not-all (min beg end) (max beg end)
                                  'recentf-filename nil)
         t)))

(defun my-recentf-isearch-forward ()
  (interactive)
  (setq recentf-isearch-filenames t)
  (isearch-forward nil t))

;; redefine from recentf.el to add the property to filename, so we can
;; search filter
(defun recentf-open-files-item (menu-element)
  "Return a widget to display MENU-ELEMENT in a dialog buffer."
  (if (consp (cdr menu-element))
      ;; Represent a sub-menu with a tree widget
      `(tree-widget
        :open t
        :match ignore
        :node (item :tag ,(car menu-element)
                    :sample-face bold
                    :format "%{%t%}:\n")
        ,@(mapcar 'recentf-open-files-item
                  (cdr menu-element)))
    ;; Represent a single file with a link widget
    (let ((str (car menu-element)))
      ;; add the text property to help the search fitler
      (string-match ".*/\\(.*?\\)$" str)
      (put-text-property (match-beginning 1) (match-end 1)
                         'recentf-filename t str)
      (message "%s" str)
      `(link :tag ,str
             :button-prefix ""
             :button-suffix ""
             :button-face default
             :format "%[%t\n%]"
             :help-echo ,(concat "Open " (cdr menu-element))
             :action recentf-open-files-action
             ,(cdr menu-element)))))
