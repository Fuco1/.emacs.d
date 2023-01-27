;;; my-redef-dynamic.el --- Global redefinitions of Emacs's own code

;;; Commentary:

;; This file contains redefinitions of the internals that are broken
;; on my system.  Unlike my-redef.el, this file uses dynamic and not
;; lexical scoping


(eval-after-load "org-agenda"
  '(progn
     ;; Org can properly display time (hour) ranges for scheduled
     ;; timestamps (those after SCHEDULED: cookie) but not for plain
     ;; dates (used for mettings/appointments).  In that case for some
     ;; miraculous reason the part after -- in the time range
     ;; (eg. <2015-06-06 10:00--11:30>) is ignored.  Here we add a
     ;; check to see if the timestamp has this ending part (in case
     ;; the range is not composed of two timestamps for >day long
     ;; intervals) and update the "end of task" variable accordingly.
     (el-patch-defun org-agenda-format-item (extra txt &optional with-level with-category tags dotime
                                                   remove-re habitp)
       "Format TXT to be inserted into the agenda buffer.
In particular, add the prefix and corresponding text properties.

EXTRA must be a string to replace the `%s' specifier in the prefix format.
WITH-LEVEL may be a string to replace the `%l' specifier.
WITH-CATEGORY (a string, a symbol or nil) may be used to overrule the default
category taken from local variable or file name.  It will replace the `%c'
specifier in the format.
DOTIME, when non-nil, indicates that a time-of-day should be extracted from
TXT for sorting of this entry, and for the `%t' specifier in the format.
When DOTIME is a string, this string is searched for a time before TXT is.
TAGS can be the tags of the headline.
Any match of REMOVE-RE will be removed from TXT."
       ;; We keep the org-prefix-* variable values along with a compiled
       ;; formatter, so that multiple agendas existing at the same time do
       ;; not step on each other toes.
       ;;
       ;; It was inconvenient to make these variables buffer local in
       ;; Agenda buffers, because this function expects to be called with
       ;; the buffer where item comes from being current, and not agenda
       ;; buffer
       (let* ((bindings (car org-prefix-format-compiled))
              (formatter (cadr org-prefix-format-compiled)))
         (cl-loop for (var value) in bindings
                  do (set var value))
         (save-match-data
           ;; Diary entries sometimes have extra whitespace at the beginning
           (setq txt (org-trim txt))

           ;; Fix the tags part in txt
           (setq txt (org-agenda-fix-displayed-tags
                      txt tags
                      org-agenda-show-inherited-tags
                      org-agenda-hide-tags-regexp))

           (with-no-warnings
             ;; `time', `tag', `effort' are needed for the eval of the prefix format.
             ;; Based on what I see in `org-compile-prefix-format', I added
             ;; a few more.
             (defvar breadcrumbs) (defvar category) (defvar category-icon)
             (defvar effort) (defvar extra)
             (defvar level) (defvar tag) (defvar time))
           (let* ((category (or with-category
                                (if buffer-file-name
                                    (file-name-sans-extension
                                     (file-name-nondirectory buffer-file-name))
                                  "")))
                  (category-icon (org-agenda-get-category-icon category))
                  (category-icon (if category-icon
                                     (propertize " " 'display category-icon)
                                   ""))
                  (effort (and (not (string= txt ""))
                               (get-text-property 1 'effort txt)))
                  (tag (if tags (nth (1- (length tags)) tags) ""))
                  (time-grid-trailing-characters (nth 2 org-agenda-time-grid))
                  (extra (or (and (not habitp) extra) ""))
                  time
                  (ts (when dotime (concat
                                    (if (stringp dotime) dotime "")
                                    (and org-agenda-search-headline-for-time txt))))
                  (time-of-day (and dotime (org-get-time-of-day ts)))
                  stamp plain s0 s1 s2 rtn srp l
                  duration breadcrumbs)
             (and (derived-mode-p 'org-mode) buffer-file-name
                  (add-to-list 'org-agenda-contributing-files buffer-file-name))
             (when (and dotime time-of-day)
               ;; Extract starting and ending time and move them to prefix
               (when (or (setq stamp (string-match org-stamp-time-of-day-regexp ts))
                         (setq plain (string-match org-plain-time-of-day-regexp ts)))
                 (setq s0 (match-string 0 ts)
                       srp (and stamp (match-end 3))
                       s1 (match-string (if plain 1 2) ts)
                       s2 (el-patch-wrap 1 1
                            (or (match-string (if plain 8 (if srp 4 6)) ts)
                                ;; FUCO: test if s1 is a range, and if
                                ;; so, update s2 to the end time
                                (save-match-data
                                  (when (string-match "[012][0-9]:[0-5][0-9]--?\\([012][0-9]:[0-5][0-9]\\)" s1)
                                    (match-string 1 s1))))))

                 ;; If the times are in TXT (not in DOTIMES), and the prefix will list
                 ;; them, we might want to remove them there to avoid duplication.
                 ;; The user can turn this off with a variable.
                 (when (and org-prefix-has-time
                            org-agenda-remove-times-when-in-prefix (or stamp plain)
                            (string-match (concat (regexp-quote s0) " *") txt)
                            (not (equal ?\] (string-to-char (substring txt (match-end 0)))))
                            (if (eq org-agenda-remove-times-when-in-prefix 'beg)
                                (= (match-beginning 0) 0)
                              t))
                   (setq txt (replace-match "" nil nil txt))))
               ;; Normalize the time(s) to 24 hour.
               (when s1 (setq s1 (org-get-time-of-day s1 t)))
               (when s2 (setq s2 (org-get-time-of-day s2 t)))
               ;; Try to set s2 if s1 and
               ;; `org-agenda-default-appointment-duration' are set
               (when (and s1 (not s2) org-agenda-default-appointment-duration)
                 (setq s2
                       (org-duration-from-minutes
                        (+ (org-duration-to-minutes s1 t)
                           org-agenda-default-appointment-duration)
                        nil t)))
               ;; Compute the duration
               (when s2
                 (setq duration (- (org-duration-to-minutes s2)
                                   (org-duration-to-minutes s1))))
               ;; Format S1 and S2 for display.
               (when s1 (setq s1 (format "%5s" (org-get-time-of-day s1 'overtime))))
               (when s2 (setq s2 (org-get-time-of-day s2 'overtime))))
             (when (string-match org-tag-group-re txt)
               ;; Tags are in the string
               (if (or (eq org-agenda-remove-tags t)
                       (and org-agenda-remove-tags
                            org-prefix-has-tag))
                   (setq txt (replace-match "" t t txt))
                 (setq txt (replace-match
                            (concat (make-string (max (- 50 (length txt)) 1) ?\ )
                                    (match-string 1 txt))
                            t t txt))))

             (when remove-re
               (while (string-match remove-re txt)
                 (setq txt (replace-match "" t t txt))))

             ;; Set org-heading property on `txt' to mark the start of the
             ;; heading.
             (add-text-properties 0 (length txt) '(org-heading t) txt)

             ;; Prepare the variables needed in the eval of the compiled format
             (when org-prefix-has-breadcrumbs
               (setq breadcrumbs (org-with-point-at (org-get-at-bol 'org-marker)
                                   (let ((s (org-format-outline-path (org-get-outline-path)
                                                                     (1- (frame-width))
                                                                     nil org-agenda-breadcrumbs-separator)))
                                     (if (eq "" s) "" (concat s org-agenda-breadcrumbs-separator))))))
             (setq time (cond (s2 (concat
                                   (org-agenda-time-of-day-to-ampm-maybe s1)
                                   "-" (org-agenda-time-of-day-to-ampm-maybe s2)
                                   (when org-agenda-timegrid-use-ampm " ")))
                              (s1 (concat
                                   (org-agenda-time-of-day-to-ampm-maybe s1)
                                   (if org-agenda-timegrid-use-ampm
                                       (concat time-grid-trailing-characters " ")
                                     time-grid-trailing-characters)))
                              (t ""))
                   category (if (symbolp category) (symbol-name category) category)
                   level (or with-level ""))
             (if (string-match org-link-bracket-re category)
                 (progn
                   (setq l (string-width (or (match-string 2) (match-string 1))))
                   (when (< l (or org-prefix-category-length 0))
                     (setq category (copy-sequence category))
                     (org-add-props category nil
                       'extra-space (make-string
                                     (- org-prefix-category-length l 1) ?\ ))))
               (when (and org-prefix-category-max-length
                          (>= (length category) org-prefix-category-max-length))
                 (setq category (substring category 0 (1- org-prefix-category-max-length)))))
             ;; Evaluate the compiled format
             (setq rtn (concat (eval formatter t) txt))

             ;; And finally add the text properties
             (remove-text-properties 0 (length rtn) '(line-prefix t wrap-prefix t) rtn)
             (org-add-props rtn nil
               'org-category category
               'tags tags
               'org-priority-highest org-priority-highest
               'org-priority-lowest org-priority-lowest
               'time-of-day time-of-day
               'duration duration
               'breadcrumbs breadcrumbs
               'txt txt
               'level level
               'time time
               'extra extra
               'format org-prefix-format-compiled
               'dotime dotime)))))))

(provide 'my-redef-dynamic)
;;; my-redef-dynamic.el ends here
