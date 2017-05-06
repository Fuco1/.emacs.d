(require 'ox-publish)

(defun my-org-find-with-tags (match)
  "Find all headings in agenda files that match MATCH.

Return list of markers, each pointing to the respective heading."
  (let* ((re nil)
         (org--matcher-tags-todo-only nil)
         (matcher (cdr (org-make-tags-matcher match))))
    (-each (org-agenda-files)
      (lambda (file)
        (org-check-agenda-file file)
        (-when-let (buffer (if (file-exists-p file)
                               (org-get-agenda-file-buffer file)
                             (error "No such file %s" file)))
          (with-current-buffer buffer
            (unless (derived-mode-p 'org-mode)
              (error "Agenda file %s is not in `org-mode'" file))
            (save-excursion
              (save-restriction
                (widen)
                (setq re (append
                          re (org-scan-tags
                              (lambda () (point-marker))
                              matcher nil)))))))))
    re))

(defun my-org--get-subtree-string (&optional pom)
  "Get subtree as string"
  (let ((kill-ring kill-ring))
    (org-with-point-at (or pom (point))
      (shut-up (org-copy-subtree))
      (current-kill 0))))

(defun my-org-publish-property (project property)
  "Return PROJECT PROPERTY.

PROJECT is a string identifying a project in `org-publish-project-alist' and PROPERTY is a property symbol."
  (-when-let (project (assoc project org-publish-project-alist))
    (org-publish-property property project)))

(defun my-org--format-timestamp (timestamp-string format-string)
  "Format TIMESTAMP-STRING according to FORMAT-STRING.

FORMAT-STRING uses the conventions of `format-time-string'."
  (format-time-string
   format-string
   (apply 'encode-time (org-parse-time-string timestamp-string))))

(defun my-org-blog--get-filename-from-header (pom)
  "Generate filename for tree at POM."
  (org-with-point-at pom
    (-if-let (file (org-entry-get pom "BLOG_FILENAME"))
        file
      (-if-let* ((closed (org-entry-get pom "CLOSED"))
                 (heading (org-get-heading :no-tags :no-todo)))
          (concat (my-org--format-timestamp closed "%Y-%m-%d")
                  "-"
                  (replace-regexp-in-string " +" "-" heading))
        (user-error "Entry is not in CLOSED state")))))

(defun my-org-blog--replace-link (pom export-buffer match-data)
  "Fix a link pointing to entry at POM to be relative to EXPORT-BUFFER.

MATCH-DATA is the `match-data' from export buffer specifying
where the replaced link is."
  (org-with-point-at pom
    (let ((file (my-org-blog--get-filename-from-header pom))
          (heading (org-link-escape (org-get-heading :no-tags :no-todo))))
      (with-current-buffer export-buffer
        (set-match-data match-data)
        (replace-match
         (format "[[file:./%s.org::*%s]" file heading))))))

;; TODO: save the source from where this post was generated
(defun my-org-blog--create-or-update-post (pom)
  "Create or update post file for tree at POM."
  (org-with-point-at pom
    (-if-let (closed (org-entry-get pom "CLOSED"))
        (let* ((file (my-org-blog--get-filename-from-header pom))
               (base (my-org-publish-property "blog-posts" :base-directory))
               (full-file (concat base "/" file ".org"))
               (tree (my-org--get-subtree-string))
               (source-buffer (current-buffer)))
          (org-entry-put pom "BLOG_FILENAME" file)
          (make-directory (file-name-directory full-file) :parents)
          (with-temp-file full-file
            (let ((export-buffer (current-buffer)))
              (org-mode)
              (insert tree)
              (goto-char (point-min))
              (while (> (org-current-level) 1)
                (org-promote-subtree))
              (org-set-tags-to (cons "ignore" (org-get-tags-at nil :local)))
              (insert (format "#+TITLE: %s\n" (org-get-heading :no-tags :no-todo)))
              (insert (format "#+DATE: %s\n" closed))
              (insert "\n")
              ;; Fix links relative in the source files to links
              ;; relative in the output files
              (while (re-search-forward "\\[\\[\\(id:\\|file:\\|\\*\\)\\(.*?\\)\\]" nil t)
                (let ((type (match-string 1))
                      (value (match-string 2))
                      (match-data (match-data)))
                  (cond
                   ((equal type "id:")
                    (-when-let (pom (org-id-find value 'marker))
                      (my-org-blog--replace-link pom export-buffer match-data)))
                   ((equal type "file:")
                    (-let (((file header (split-string value "::*"))))
                      (with-current-buffer (find-file-noselect file)
                        (org-goto-local-search-headings header nil t)
                        (my-org-blog--replace-link (point-marker) export-buffer match-data))))
                   ((equal type "*")
                    (with-current-buffer source-buffer
                      (org-goto-local-search-headings value nil t)
                      (my-org-blog--replace-link (point-marker) export-buffer match-data)))))))))
      (user-error "Entry is not in CLOSED state"))))

(defun my-org-publish (&optional dont-publish)
  "Publish current subtree.

With DONT-PUBLISH just copy the source to its blog-post file but
do not run `org-publish'."
  (interactive "P")
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (my-org-blog--create-or-update-post (point-marker))
    (unless dont-publish (org-publish "blog"))))

(defun my-org-publish-agenda ()
  "Publish all publishable entries found in agenda."
  (interactive)
  (-each (my-org-find-with-tags "publish")
    (lambda (pom)
      (org-with-point-at pom
        (message "Publishing %s" (org-get-heading :no-tags :no-todo))
        (my-org-publish :dont-publish)))))

(setq org-publish-project-alist
      `(("blog"
         :components ("blog-posts"))
        ("blog-posts"
         :base-directory ,(expand-file-name "~/documents/blog/posts")
         :publishing-directory ,(expand-file-name "~/documents/blog/")
         :publishing-function org-html-publish-to-html

         :auto-sitemap t
         :sitemap-title "Archive"
         :sitemap-file-entry-format "%d %t"
         :sitemap-sort-files 'anti-chronologically

         :with-author t
         :with-creator nil
         :with-date t

         :with-tags nil
         :with-toc nil
         :with-todo-keywords nil
         :section-numbers nil

         ;; the following removes extra headers from HTML output -- important!
         :html-home/up-format ""
         :html-head nil ;; cleans up anything that would have been in there.
         :html-head-extra
         ,(concat
           "<link rel=\"stylesheet\" href=\"css/blog.css\" />")
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-preamble
         (lambda (&rest _ignore)
           (format
            "<a href=\"./sitemap.html\">Home</a>
<a href=\"https://github.com/Fuco1/\">GitHub</a>
<a href=\"https://www.patreon.com/user?u=3282358\">Patreon</a>
<hr />"))
         :html-postamble
         (lambda (args)
           (-let (((&plist :input-file input :publishing-directory base) args))
             (format
              "<hr />
<div style=\"text-align: left; float: left;\">Last updated at: %s</div>
<div style=\"text-align: right;\"><a href=\"%s\">Source</a></div>"
              (format-time-string "%Y-%m-%d %H:%M")
              (replace-regexp-in-string (regexp-quote base) "./" input)))))))
