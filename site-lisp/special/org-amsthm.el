(defvar org-amsthm-theorem 0
  "Theorem counter.")

(defvar org-amsthm-definition 0
  "Theorem counter.")

(defun org-dblock-write:reset-counter (params)
  "Reset all or specified counters.

If argument :counters is passed, only these counters are reset."
  (setq org-amsthm-theorem 0)
  (setq org-amsthm-definition 0)
  (kill-line 1))

(defun org-dblock-write:theorem (params)
  (cl-incf org-amsthm-theorem)
  (let ((content (substring (plist-get params :content) 0 -1)))
    (insert
     (with-temp-buffer
       (insert content)
       (goto-char (point-min))
       (when (looking-at "*Propositio")
         (kill-line 1))
       (insert "*Propositio "
               (int-to-string org-amsthm-theorem)
               ".*"
               "\n")
       (buffer-string)))))

(defun org-dblock-write:definition (params)
  (cl-incf org-amsthm-definition)
  (let ((content (substring (plist-get params :content) 0 -1)))
    (insert
     (with-temp-buffer
       (insert content)
       (goto-char (point-min))
       (when (looking-at "*Definitio")
         (kill-line 1))
       (insert "*Definitio "
               (int-to-string org-amsthm-definition)
               ".*"
               "\n")
       (buffer-string)))))

(defun org-dblock-write:proof (params)
  (let ((content (substring (plist-get params :content) 0 -1)))
    (insert content)))
