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
