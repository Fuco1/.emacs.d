;;; my-redef.el --- Global redefinitions of Emacs's own code

;;; Commentary:

;; This file contains redefinitions of the internals that are broken
;; on my system

;;; Code:

;; /usr/local/share/emacs/24.3/lisp/net/browse-url.el.gz
(eval-after-load "browse-url"
  '(defun browse-url-can-use-xdg-open ()
     "Return non-nil if the \"xdg-open\" program can be used.
xdg-open is a desktop utility that calls your preferred web browser.
This requires you to be running either Gnome, KDE, Xfce4 or LXDE."
     t))

;; redefines the silly indent of keyword lists
;; before
;;   (:foo bar
;;         :baz qux)
;; after
;;   (:foo bar
;;    :baz qux)
(eval-after-load "lisp-mode"
  '(defun lisp-indent-function (indent-point state)
     "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
     (let ((normal-indent (current-column))
           (orig-point (point)))
       (goto-char (1+ (elt state 1)))
       (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
       (cond
        ;; car of form doesn't seem to be a symbol, or is a keyword
        ((and (elt state 2)
              (or (not (looking-at "\\sw\\|\\s_"))
                  (looking-at ":")))
         (if (not (> (save-excursion (forward-line 1) (point))
                     calculate-lisp-indent-last-sexp))
             (progn (goto-char calculate-lisp-indent-last-sexp)
                    (beginning-of-line)
                    (parse-partial-sexp (point)
                                        calculate-lisp-indent-last-sexp 0 t)))
         ;; Indent under the list or under the first sexp on the same
         ;; line as calculate-lisp-indent-last-sexp.  Note that first
         ;; thing on that line has to be complete sexp since we are
         ;; inside the innermost containing sexp.
         (backward-prefix-chars)
         (current-column))
        ((and (save-excursion
                (goto-char indent-point)
                (skip-syntax-forward " ")
                (not (looking-at ":")))
              (save-excursion
                (goto-char orig-point)
                (looking-at ":")))
         (save-excursion
           (goto-char (+ 2 (elt state 1)))
           (current-column)))
        (t
         (let ((function (buffer-substring (point)
                                           (progn (forward-sexp 1) (point))))
               method)
           (setq method (or (function-get (intern-soft function)
                                          'lisp-indent-function)
                            (get (intern-soft function) 'lisp-indent-hook)))
           (cond ((or (eq method 'defun)
                      (and (null method)
                           (> (length function) 3)
                           (string-match "\\`def" function)))
                  (lisp-indent-defform state indent-point))
                 ((integerp method)
                  (lisp-indent-specform method state
                                        indent-point normal-indent))
                 (method
                  (funcall method indent-point state)))))))))

;; fix the situation when there is no table.  So far this is a hard-coded hack.
;; see org-capture.el for original
(eval-after-load "org-capture"
  '(defun org-capture-place-table-line ()
     "Place the template as a table line."
     (require 'org-table)
     (let* ((txt (org-capture-get :template))
            (target-entry-p (org-capture-get :target-entry-p))
            (table-line-pos (org-capture-get :table-line-pos))
            ind beg end)
       (cond
        ((org-capture-get :exact-position)
         (goto-char (org-capture-get :exact-position)))
        ((not target-entry-p)
         ;; Table is not necessarily under a heading
         (setq beg (point-min) end (point-max)))
        (t
         ;; WE are at a heading, limit search to the body
         (setq beg (1+ (point-at-eol))
               end (save-excursion (outline-next-heading) (point)))))
       (if (re-search-forward org-table-dataline-regexp end t)
           (let ((b (org-table-begin)) (e (org-table-end)) (case-fold-search t))
             (goto-char e)
             (if (looking-at "[ \t]*#\\+tblfm:")
                 (forward-line 1))
             (narrow-to-region b (point)))
         (goto-char end)
         (if (equal (buffer-name) "CAPTURE-water.org") ;; UGLY-HACK
             (insert "\n|------+--------|\n| Time | Amount |\n|------+--------|\n|------+--------|\n|      |        |\n|------+--------|\n#+TBLFM:@>$2 = vsum(@2..@-1);%.0f\n")
           (insert "\n|   |\n|----|\n|    |\n"))
         (narrow-to-region (1+ end) (point)))
       ;; We are narrowed to the table, or to an empty line if there was no table

       ;; Check if the template is good
       (if (not (string-match org-table-dataline-regexp txt))
           (setq txt "| %?Bad template |\n"))
       (cond
        ((and table-line-pos
              (string-match "\\(I+\\)\\([-+][0-9]\\)" table-line-pos))
         ;; we have a complex line specification
         (goto-char (point-min))
         (let ((nh (- (match-end 1) (match-beginning 1)))
               (delta (string-to-number (match-string 2 table-line-pos)))
               ll)
           ;; The user wants a special position in the table
           (org-table-get-specials)
           (setq ll (ignore-errors (aref org-table-hlines nh)))
           (unless ll (error "Invalid table line specification \"%s\""
                             table-line-pos))
           (setq ll (+ ll delta (if (< delta 0) 0 -1)))
           (org-goto-line ll)
           (org-table-insert-row 'below)
           (beginning-of-line 1)
           (delete-region (point) (1+ (point-at-eol)))
           (setq beg (point))
           (insert txt)
           (setq end (point))))
        ((org-capture-get :prepend)
         (goto-char (point-min))
         (re-search-forward org-table-hline-regexp nil t)
         (beginning-of-line 1)
         (re-search-forward org-table-dataline-regexp nil t)
         (beginning-of-line 1)
         (setq beg (point))
         (org-table-insert-row)
         (beginning-of-line 1)
         (delete-region (point) (1+ (point-at-eol)))
         (insert txt)
         (setq end (point)))
        (t
         (goto-char (point-max))
         (re-search-backward org-table-dataline-regexp nil t)
         (beginning-of-line 1)
         (org-table-insert-row 'below)
         (beginning-of-line 1)
         (delete-region (point) (1+ (point-at-eol)))
         (setq beg (point))
         (insert txt)
         (setq end (point))))
       (goto-char beg)
       (org-capture-position-for-last-stored 'table-line)
       (if (or (re-search-backward "%\\?" beg t)
               (re-search-forward "%\\?" end t))
           (replace-match ""))
       (org-table-align))))

;; Fix the annoying assumption where it grabs the FIRST line from
;; .authinfo as the user to auth with.  This in itself is not "that"
;; bad, but gmail rewrites the From: address to the address ofthe user
;; you auth against smtp with, which breaks outgoing email
(eval-after-load "smtpmail"
  '(defun smtpmail-try-auth-methods (process supported-extensions host port
                                             &optional ask-for-password)
     (setq port
           (if port
               (format "%s" port)
             "smtp"))
     (let* ((mechs (cdr-safe (assoc 'auth supported-extensions)))
            (mech (car (smtpmail-intersection mechs smtpmail-auth-supported)))
            (auth-source-creation-prompts
             '((user  . "SMTP user name for %h: ")
               (secret . "SMTP password for %u@%h: ")))
            ;; FUCO: this line was added to set smtp-user properly
            (smtpmail-smtp-user envelope-from)
            ;; FUCOEND:
            (auth-info (car
                        (auth-source-search
                         :host host
                         :port port
                         :user smtpmail-smtp-user
                         :max 1
                         :require (and ask-for-password
                                       '(:user :secret))
                         :create ask-for-password)))
            (user (plist-get auth-info :user))
            (password (plist-get auth-info :secret))
            (save-function (and ask-for-password
                                (plist-get auth-info :save-function)))
            ret)
       (when (functionp password)
         (setq password (funcall password)))
       (when (and user
                  (not password))
         ;; The user has stored the user name, but not the password, so
         ;; ask for the password, even if we're not forcing that through
         ;; `ask-for-password'.
         (setq auth-info
               (car
                (auth-source-search
                 :max 1
                 :host host
                 :port port
                 :user smtpmail-smtp-user
                 :require '(:user :secret)
                 :create t))
               password (plist-get auth-info :secret)))
       (when (functionp password)
         (setq password (funcall password)))
       (cond
        ((or (not mech)
             (not user)
             (not password))
         ;; No mechanism, or no credentials.
         mech)
        ((eq mech 'cram-md5)
         (setq ret (smtpmail-command-or-throw process "AUTH CRAM-MD5"))
         (when (eq (car ret) 334)
           (let* ((challenge (substring (cadr ret) 4))
                  (decoded (base64-decode-string challenge))
                  (hash (rfc2104-hash 'md5 64 16 password decoded))
                  (response (concat user " " hash))
                  ;; Osamu Yamane <yamane@green.ocn.ne.jp>:
                  ;; SMTP auth fails because the SMTP server identifies
                  ;; only the first part of the string (delimited by
                  ;; new line characters) as a response from the
                  ;; client, and the rest as distinct commands.

                  ;; In my case, the response string is 80 characters
                  ;; long.  Without the no-line-break option for
                  ;; `base64-encode-string', only the first 76 characters
                  ;; are taken as a response to the server, and the
                  ;; authentication fails.
                  (encoded (base64-encode-string response t)))
             (smtpmail-command-or-throw process encoded)
             (when save-function
               (funcall save-function)))))
        ((eq mech 'login)
         (smtpmail-command-or-throw process "AUTH LOGIN")
         (smtpmail-command-or-throw
          process (base64-encode-string user t))
         (smtpmail-command-or-throw process (base64-encode-string password t))
         (when save-function
           (funcall save-function)))
        ((eq mech 'plain)
         ;; We used to send an empty initial request, and wait for an
         ;; empty response, and then send the password, but this
         ;; violate a SHOULD in RFC 2222 paragraph 5.1.  Note that this
         ;; is not sent if the server did not advertise AUTH PLAIN in
         ;; the EHLO response.  See RFC 2554 for more info.
         (smtpmail-command-or-throw
          process
          (concat "AUTH PLAIN "
                  (base64-encode-string (concat "\0" user "\0" password) t))
          235)
         (when save-function
           (funcall save-function)))
        (t
         (error "Mechanism %s not implemented" mech))))))

;; Fix incorrectly returned default value when user simply hits RET
;; without doing any selection
(eval-after-load "ediff-util"
  '(defun ediff-read-file-name (prompt default-dir default-file &optional no-dirs)
     ;; hack default-dir if it is not set
     (setq default-dir
           (file-name-as-directory
            (ediff-abbreviate-file-name
             (expand-file-name (or default-dir
                                   (and default-file
                                        (file-name-directory default-file))
                                   default-directory)))))

     ;; strip the directory from default-file
     (if default-file
         (setq default-file (file-name-nondirectory default-file)))
     (if (string= default-file "")
         (setq default-file nil))

     (let ((defaults (and (fboundp 'dired-dwim-target-defaults)
                          (dired-dwim-target-defaults
                           (and default-file (list default-file))
                           default-dir)))
           f)
       (setq f (ediff-minibuffer-with-setup-hook
                (lambda () (when defaults
                             (setq minibuffer-default defaults)))
                (read-file-name
                 (format "%s%s "
                         prompt
                         (cond (default-file
                                 (concat " (default " default-file "):"))
                               (t (concat " (default " default-dir "):"))))
                 default-dir
                 nil ;; FUCO: WAS: (or default-file default-dir)
                 t          ; must match, no-confirm
                 (if default-file (file-name-directory default-file))
                 )))
       (setq f (expand-file-name f default-dir))
       ;; If user entered a directory name, expand the default file in that
       ;; directory.  This allows the user to enter a directory name for the
       ;; B-file and diff against the default-file in that directory instead
       ;; of a DIRED listing!
       (if (and (file-directory-p f) default-file)
           (setq f (expand-file-name
                    (file-name-nondirectory default-file) f)))
       (if (and no-dirs (file-directory-p f))
           (error "File %s is a directory" f))
       f)))

(eval-after-load "hi-lock"
  '(progn
     (defun hi-lock-read-face-name ()
       "Read face name from minibuffer with completion and history."
       (intern (completing-read
                "Highlight using face: "
                (mapcar 'symbol-name (face-list))
                nil
                nil
                "hi-"
                'face-name-history
                (car hi-lock-face-defaults))))))

(eval-after-load "calendar"
  '(el-patch-defun calendar-basic-setup (&optional arg nodisplay)
     "Create a three-month calendar.
If optional prefix argument ARG is non-nil, prompts for the month
and year, else uses the current date.  If NODISPLAY is non-nil, don't
display the generated calendar."
     (interactive "P")
     (let ((buff (current-buffer)))
       (set-buffer (get-buffer-create calendar-buffer))
       (calendar-mode)
       (let* ((pop-up-windows t)
              ;; Not really needed now, but means we use exactly the same
              ;; behavior as before in the non-wide case (see below).
              (split-height-threshold (el-patch-swap 1000 1))
              (split-width-threshold calendar-split-width-threshold)
              (date (if arg (calendar-read-date t)
                      (calendar-current-date)))
              (month (calendar-extract-month date))
              (year (calendar-extract-year date)))
         (calendar-increment-month month year (- calendar-offset))
         ;; Display the buffer before calling calendar-generate-window so that it
         ;; can get a chance to adjust the window sizes to the frame size.
         (unless nodisplay
           ;; We want a window configuration that looks something like
           ;; X        X | Y
           ;; -        -----
           ;; C        Z | C
           ;; where C is the calendar, and the LHS is the traditional,
           ;; non-wide frame, and the RHS is the wide frame case.
           ;; We should end up in the same state regardless of whether the
           ;; windows were initially split or not.
           ;; Previously, we only thought about the non-wide case.
           ;; We could just set split-height-threshold to 1000, relying on
           ;; the fact that the window splitting treated a single window as
           ;; a special case and would always split it (vertically).  The
           ;; same thing does not work in the wide-frame case, so now we do
           ;; the splitting by hand.
           ;; See discussion in bug#1806.
           ;; Actually, this still does not do quite the right thing in the
           ;; wide frame case if started from a configuration like the LHS.
           ;; Eg if you start with a non-wide frame, call calendar, then
           ;; make the frame wider.  This one is problematic because you
           ;; might need to split a totally unrelated window.  Oh well, it
           ;; seems unlikely, and perhaps respecting the original layout is
           ;; the right thing in that case.
           ;;
           ;; Is this a wide frame?  If so, split it horizontally.

           ;; The following doesn't sound useful: If we split horizontally
           ;; here, the subsequent `pop-to-buffer' will likely split again
           ;; horizontally and we end up with three side-by-side windows.
           (when (window-splittable-p (selected-window) t)
             (split-window-right))
           (pop-to-buffer calendar-buffer)
           ;; Has the window already been split vertically?
           (when (and (not (window-dedicated-p))
                      (window-splittable-p (selected-window))
                      (window-full-height-p))
             (let ((win (split-window-below)))
               ;; In the upper window, show whatever was visible before.
               ;; This looks better than using other-buffer.
               (switch-to-buffer buff)
               ;; Switch to the lower window with the calendar buffer.
               (select-window win))))
         (calendar-generate-window month year)
         (if (and calendar-view-diary-initially-flag
                  (calendar-date-is-visible-p date))
             ;; Do not clobber the calendar with the diary, if the diary
             ;; has previously been shown in the window that now shows the
             ;; calendar (bug#18381).
             (let ((display-buffer-overriding-action
                    '(nil . ((inhibit-same-window . t)))))
               (diary-view-entries)))))
     (if calendar-view-holidays-initially-flag
         (let* ((diary-buffer (diary-live-p))
                (diary-window (if diary-buffer (get-buffer-window diary-buffer)))
                (split-height-threshold (if diary-window 2 1000)))
           ;; FIXME display buffer?
           (calendar-list-holidays)))
     (run-hooks 'calendar-initial-window-hook)))

(eval-after-load "org"
  '(progn
     ;; What we want to achieve here is a detailed scheduling scheme
     ;; for repeating tasks.  By default, org can only plan on a "+
     ;; time interval basis", so you can't schedule things like "do
     ;; task on monday and friday every week" or specify different
     ;; hour to do the task at for each day (eg. during week when I
     ;; work I want to tidy the appartment at 19:00 when I return
     ;; home, but on weekends I want to do it when I wake up at 10:00).

     ;; We achieve this objective by adding multiple SCHEDULED (plain)
     ;; timestamps for different days/times we want to do the task.
     ;; We then set the repeater for each timestamp separately (so we
     ;; can even have a scheme like "every monday each week but on
     ;; friday only every other week").  This all works in org by
     ;; default, the problem is that when we mark the task DONE *all*
     ;; the timestamps are shifted at once, so if we have a timestamp
     ;; for each day and we mark it 7 times this week, all the
     ;; timestamps will shift 7 weeks into the future.

     ;; The fix is relatively simple: only update the *past*
     ;; timestamps and leave the future timestamps alone.  The
     ;; rationale is simple, we don't want to repeat a task which
     ;; didn't even happen yet.

     ;; One problem which can occur is that we might finish a task
     ;; early.  To solve this, either the user reschedules the task
     ;; prior to starting working on it (from the original time to
     ;; "now"), or simply leaves it scheduled and, when we DONE the
     ;; task next day (or the next interval *before* the shifted
     ;; repeat time) it will already be in the past and shift
     ;; accordingly.  In practice, this should be rare as this scheme
     ;; is mostly useful for repeating *habitual* tasks which we
     ;; rarely want to do ahead of schedule (eg. workout, language
     ;; lessons, school material review etc.)
     (defun org-auto-repeat-maybe (done-word)
       "Check if the current headline contains a repeated time-stamp.

If yes, set TODO state back to what it was and change the base date
of repeating deadline/scheduled time stamps to new date.

This function is run automatically after each state change to a DONE state."
       (let* ((repeat (org-get-repeat))
              (aa (assoc org-last-state org-todo-kwd-alist))
              (interpret (nth 1 aa))
              (head (nth 2 aa))
              (whata '(("h" . hour) ("d" . day) ("m" . month) ("y" . year)))
              (msg "Entry repeats: ")
              (org-log-done nil)
              (org-todo-log-states nil))
         (when (and repeat (not (zerop (string-to-number (substring repeat 1)))))
           (when (eq org-log-repeat t) (setq org-log-repeat 'state))
           (let ((to-state (or (org-entry-get nil "REPEAT_TO_STATE" 'selective)
                               org-todo-repeat-to-state)))
             (org-todo (cond ((and to-state (member to-state org-todo-keywords-1))
                              to-state)
                             ((eq interpret 'type) org-last-state)
                             (head)
                             (t 'none))))
           (when (or org-log-repeat (org-entry-get nil "CLOCK"))
             (org-entry-put nil "LAST_REPEAT" (format-time-string
                                               (org-time-stamp-format t t))))
           (when org-log-repeat
             (if (or (memq 'org-add-log-note (default-value 'post-command-hook))
                     (memq 'org-add-log-note post-command-hook))
                 ;; We are already setup for some record.
                 (when (eq org-log-repeat 'note)
                   ;; Make sure we take a note, not only a time stamp.
                   (setq org-log-note-how 'note))
               ;; Set up for taking a record.
               (org-add-log-setup 'state
                                  (or done-word (car org-done-keywords))
                                  org-last-state
                                  org-log-repeat)))
           (org-back-to-heading t)
           (org-add-planning-info nil nil 'closed)
           (let ((end (save-excursion (outline-next-heading) (point)))
                 (planning-re (regexp-opt
                               (list org-scheduled-string org-deadline-string))))
             (while (re-search-forward org-ts-regexp end t)
               (let* ((ts (match-string 0))
                      (planning? (org-at-planning-p))
                      (type (if (not planning?) "Plain:"
                              (save-excursion
                                (re-search-backward
                                 planning-re (line-beginning-position) t)
                                (match-string 0)))))
                 (cond
                  ;; Ignore fake time-stamps (e.g., within comments).
                  ((and (not planning?)
                        (not (org-at-property-p))
                        (not (eq 'timestamp
                                 (org-element-type (save-excursion
                                                     (backward-char)
                                                     (org-element-context)))))))
                  ;; Time-stamps without a repeater are usually skipped.
                  ;; However, a SCHEDULED time-stamp without one is
                  ;; removed, as it is considered as no longer relevant.
                  ((not (string-match "\\([.+]\\)?\\(\\+[0-9]+\\)\\([hdwmy]\\)" ts))
                   (when (equal type org-scheduled-string)
                     (org-remove-timestamp-with-keyword type)))
                  (t
                   (let ((n (string-to-number (match-string 2 ts)))
                         (what (match-string 3 ts))
                         ;; FUCO: time moved here from ##time## becase we
                         ;; need it sooner
                         (time (save-match-data (org-time-string-to-time ts))))
                     (when (time-less-p time (current-time))
                       (when (equal what "w") (setq n (* n 7) what "d"))
                       (when (and (equal what "h")
                                  (not (string-match-p "[0-9]\\{1,2\\}:[0-9]\\{2\\}"
                                                       ts)))
                         (user-error
                          "Cannot repeat in Repeat in %d hour(s) because no hour \
has been set"
                          n))
                       ;; Preparation, see if we need to modify the start
                       ;; date for the change.
                       (when (match-end 1)
                         ;; FUCO: ##time## was here
                         ;; FUCO: test if the time is in the past and
                         ;; only update then
                         (cond
                          ((equal (match-string 1 ts) ".")
                           ;; Shift starting date to today
                           (org-timestamp-change
                            (- (org-today) (time-to-days time))
                            'day))
                          ((equal (match-string 1 ts) "+")
                           (let ((nshiftmax 10)
                                 (nshift 0))
                             (while (or (= nshift 0)
                                        (not (time-less-p (current-time) time)))
                               (when (= (cl-incf nshift) nshiftmax)
                                 (or (y-or-n-p
                                      (format "%d repeater intervals were not \
enough to shift date past today.  Continue? "
                                              nshift))
                                     (user-error "Abort")))
                               (org-timestamp-change n (cdr (assoc what whata)))
                               (org-at-timestamp-p t)
                               (setq ts (match-string 1))
                               (setq time
                                     (save-match-data
                                       (org-time-string-to-time ts)))))
                           (org-timestamp-change (- n) (cdr (assoc what whata)))
                           ;; Rematch, so that we have everything in place
                           ;; for the real shift.
                           (org-at-timestamp-p t)
                           (setq ts (match-string 1))
                           (string-match "\\([.+]\\)?\\(\\+[0-9]+\\)\\([hdwmy]\\)"
                                         ts))))
                       (save-excursion
                         (org-timestamp-change n (cdr (assoc what whata)) nil t))
                       (setq msg
                             (concat
                              msg type " " org-last-changed-timestamp " ")))))))))
           (setq org-log-post-message msg)
           (message "%s" msg))))

     ;; TODO: when we find a SCHEDULED, check for *ALL* such cookies
     ;; in the current task and pick the smallest date.
     ;; (defun org-agenda-get-scheduled (&optional deadlines with-hour)
     ;;   "Return the scheduled information for agenda display.
     ;; Optional argument DEADLINES is a list of deadline items to be
     ;; displayed in agenda view.  When WITH-HOUR is non-nil, only return
     ;; scheduled items with an hour specification like [h]h:mm."
     ;;   (let* ((props (list 'org-not-done-regexp org-not-done-regexp
     ;;               'org-todo-regexp org-todo-regexp
     ;;               'org-complex-heading-regexp org-complex-heading-regexp
     ;;               'done-face 'org-agenda-done
     ;;               'mouse-face 'highlight
     ;;               'help-echo
     ;;               (format "mouse-2 or RET jump to Org file %s"
     ;;                   (abbreviate-file-name buffer-file-name))))
     ;;      (regexp (if with-hour
     ;;              org-scheduled-time-hour-regexp
     ;;            org-scheduled-time-regexp))
     ;;      (today (org-today))
     ;;      (todayp (org-agenda-today-p date)) ; DATE bound by calendar.
     ;;      (current (calendar-absolute-from-gregorian date))
     ;;      (deadline-pos
     ;;       (mapcar (lambda (d)
     ;;             (let ((m (get-text-property 0 'org-hd-marker d)))
     ;;               (and m (marker-position m))))
     ;;           deadlines))
     ;;      scheduled-items)
     ;;     (goto-char (point-min))
     ;;     (while (re-search-forward regexp nil t)
     ;;       (catch :skip
     ;;     (unless (save-match-data (org-at-planning-p)) (throw :skip nil))
     ;;     (org-agenda-skip)
     ;;     (let* ((s (match-string 1))
     ;;            (pos (1- (match-beginning 1)))
     ;;            (todo-state (save-match-data (org-get-todo-state)))
     ;;            (donep (member todo-state org-done-keywords))
     ;;            (show-all (or (eq org-agenda-repeating-timestamp-show-all t)
     ;;                  (member todo-state
     ;;                      org-agenda-repeating-timestamp-show-all)))
     ;;            (sexp? (string-prefix-p "%%" s))
     ;;            ;; SCHEDULE is the bare scheduled date, i.e., without
     ;;            ;; any repeater if non-nil, or last repeat if SHOW-ALL
     ;;            ;; is nil.  REPEAT is the closest repeat after CURRENT,
     ;;            ;; if all repeated time stamps are to be shown, or
     ;;            ;; after TODAY otherwise.  REPEAT only applies to
     ;;            ;; future dates.
     ;;            (schedule (cond
     ;;               (sexp? (org-agenda--timestamp-to-absolute s current))
     ;;               (show-all (org-agenda--timestamp-to-absolute s))
     ;;               (t (org-agenda--timestamp-to-absolute
     ;;                   s today 'past (current-buffer) pos))))
     ;;            (repeat (cond
     ;;             (sexp? schedule)
     ;;             ((< current today) schedule)
     ;;             (t
     ;;              (org-agenda--timestamp-to-absolute
     ;;               s (if show-all current today) 'future
     ;;               (current-buffer) pos))))
     ;;            (diff (- current schedule))
     ;;            (warntime (get-text-property (point) 'org-appt-warntime))
     ;;            (pastschedp (< schedule today))
     ;;            (habitp (and (fboundp 'org-is-habit-p) (org-is-habit-p)))
     ;;            (suppress-delay
     ;;         (let ((deadline (and org-agenda-skip-scheduled-delay-if-deadline
     ;;                      (org-entry-get nil "DEADLINE"))))
     ;;           (cond
     ;;            ((not deadline) nil)
     ;;            ;; The current item has a deadline date, so
     ;;            ;; evaluate its delay time.
     ;;            ((integerp org-agenda-skip-scheduled-delay-if-deadline)
     ;;             ;; Use global delay time.
     ;;             (- org-agenda-skip-scheduled-delay-if-deadline))
     ;;            ((eq org-agenda-skip-scheduled-delay-if-deadline
     ;;             'post-deadline)
     ;;             ;; Set delay to no later than DEADLINE.
     ;;             (min (- schedule
     ;;                 (org-agenda--timestamp-to-absolute deadline))
     ;;              org-scheduled-delay-days))
     ;;            (t 0))))
     ;;            (ddays
     ;;         (cond
     ;;          ;; Nullify delay when a repeater triggered already
     ;;          ;; and the delay is of the form --Xd.
     ;;          ((and (string-match-p "--[0-9]+[hdwmy]" s)
     ;;                (> current schedule))
     ;;           0)
     ;;          (suppress-delay
     ;;           (let ((org-scheduled-delay-days suppress-delay))
     ;;             (org-get-wdays s t t)))
     ;;          (t (org-get-wdays s t)))))
     ;;       ;; Display scheduled items at base date (SCHEDULE), today if
     ;;       ;; scheduled before the current date, and at any repeat past
     ;;       ;; today.  However, skip delayed items and items that have
     ;;       ;; been displayed for more than `org-scheduled-past-days'.
     ;;       (unless (and todayp
     ;;                habitp
     ;;                (bound-and-true-p org-habit-show-all-today))
     ;;         (when (or (and (> ddays 0) (< diff ddays))
     ;;               (> diff org-scheduled-past-days)
     ;;               (> schedule current)
     ;;               (and (< schedule current)
     ;;                (not todayp)
     ;;                (/= repeat current)))
     ;;           (throw :skip nil)))
     ;;       ;; Possibly skip done tasks.
     ;;       (when (and donep
     ;;              (or org-agenda-skip-scheduled-if-done
     ;;              (/= schedule current)))
     ;;         (throw :skip nil))
     ;;       ;; Skip entry if it already appears as a deadline, per
     ;;       ;; `org-agenda-skip-scheduled-if-deadline-is-shown'.  This
     ;;       ;; doesn't apply to habits.
     ;;       (when (pcase org-agenda-skip-scheduled-if-deadline-is-shown
     ;;           ((guard
     ;;             (or (not (memq (line-beginning-position 0) deadline-pos))
     ;;             habitp))
     ;;            nil)
     ;;           (`repeated-after-deadline
     ;;            (>= repeat (time-to-days (org-get-deadline-time (point)))))
     ;;           (`not-today pastschedp)
     ;;           (`t t)
     ;;           (_ nil))
     ;;         (throw :skip nil))
     ;;       ;; Skip habits if `org-habit-show-habits' is nil, or if we
     ;;       ;; only show them for today.  Also skip done habits.
     ;;       (when (and habitp
     ;;              (or donep
     ;;              (not (bound-and-true-p org-habit-show-habits))
     ;;              (and (not todayp)
     ;;                   (bound-and-true-p
     ;;                    org-habit-show-habits-only-for-today))))
     ;;         (throw :skip nil))
     ;;       (save-excursion
     ;;         (re-search-backward "^\\*+[ \t]+" nil t)
     ;;         (goto-char (match-end 0))
     ;;         (let* ((category (org-get-category))
     ;;            (inherited-tags
     ;;             (or (eq org-agenda-show-inherited-tags 'always)
     ;;             (and (listp org-agenda-show-inherited-tags)
     ;;                  (memq 'agenda org-agenda-show-inherited-tags))
     ;;             (and (eq org-agenda-show-inherited-tags t)
     ;;                  (or (eq org-agenda-use-tag-inheritance t)
     ;;                  (memq 'agenda
     ;;                        org-agenda-use-tag-inheritance)))))
     ;;            (tags (org-get-tags-at nil (not inherited-tags)))
     ;;            (level
     ;;             (make-string (org-reduced-level (org-outline-level)) ?\s))
     ;;            (head (buffer-substring (point) (line-end-position)))
     ;;            (time
     ;;             (cond
     ;;              ;; No time of day designation if it is only
     ;;              ;; a reminder.
     ;;              ((and (/= current schedule) (/= current repeat)) nil)
     ;;              ((string-match " \\([012]?[0-9]:[0-9][0-9]\\)" s)
     ;;               (concat (substring s (match-beginning 1)) " "))
     ;;              (t 'time)))
     ;;            (item
     ;;             (org-agenda-format-item
     ;;              (pcase-let ((`(,first ,next) org-agenda-scheduled-leaders))
     ;;                (cond
     ;;             ;; If CURRENT is in the future, don't use past
     ;;             ;; scheduled prefix.
     ;;             ((> current today) first)
     ;;             ;; SHOW-ALL focuses on future repeats.  If one
     ;;             ;; such repeat happens today, ignore late
     ;;             ;; schedule reminder.  However, still report
     ;;             ;; such reminders when repeat happens later.
     ;;             ((and (not show-all) (= repeat today)) first)
     ;;             ;; Initial report.
     ;;             ((= schedule current) first)
     ;;             ;; Subsequent reminders.  Count from base
     ;;             ;; schedule.
     ;;             (t (format next (1+ diff)))))
     ;;              head level category tags time nil habitp))
     ;;            (face (cond ((and (not habitp) pastschedp)
     ;;                 'org-scheduled-previously)
     ;;                    (todayp 'org-scheduled-today)
     ;;                    (t 'org-scheduled)))
     ;;            (habitp (and habitp (org-habit-parse-todo))))
     ;;           (org-add-props item props
     ;;         'undone-face face
     ;;         'face (if donep 'org-agenda-done face)
     ;;         'org-marker (org-agenda-new-marker pos)
     ;;         'org-hd-marker (org-agenda-new-marker (line-beginning-position))
     ;;         'type (if pastschedp "past-scheduled" "scheduled")
     ;;         'date (if pastschedp schedule date)
     ;;         'ts-date schedule
     ;;         'warntime warntime
     ;;         'level level
     ;;         'priority (if habitp (org-habit-get-priority habitp)
     ;;                 (+ 99 diff (org-get-priority item)))
     ;;         'org-habit-p habitp
     ;;         'todo-state todo-state)
     ;;           (push item scheduled-items))))))
     ;;     (nreverse scheduled-items)))

     ;; Add a quick binding to remove org occur highlights/overlays
     (defun org-sparse-tree (&optional arg type)
       "Create a sparse tree, prompt for the details.
This command can create sparse trees.  You first need to select the type
of match used to create the tree:

t      Show all TODO entries.
T      Show entries with a specific TODO keyword.
m      Show entries selected by a tags/property match.
p      Enter a property name and its value (both with completion on existing
       names/values) and show entries with that property.
r      Show entries matching a regular expression (`/' can be used as well).
b      Show deadlines and scheduled items before a date.
a      Show deadlines and scheduled items after a date.
d      Show deadlines due within `org-deadline-warning-days'.
D      Show deadlines and scheduled items between a date range."
       (interactive "P")
       (setq type (or type org-sparse-tree-default-date-type))
       (setq org-ts-type type)
       (message "Sparse tree: [/]regexp [t]odo [T]odo-kwd [m]atch [p]roperty
             [d]eadlines [b]efore-date [a]fter-date [D]ates range [\\]remove overlays
             [c]ycle through date types: %s"
                (case type
                  (all "all timestamps")
                  (scheduled "only scheduled")
                  (deadline "only deadline")
                  (active "only active timestamps")
                  (inactive "only inactive timestamps")
                  (scheduled-or-deadline "scheduled/deadline")
                  (closed "with a closed time-stamp")
                  (otherwise "scheduled/deadline")))
       (let ((answer (read-char-exclusive)))
         (case answer
           (?c
            (org-sparse-tree
             arg
             (cadr (memq type '(scheduled-or-deadline all scheduled deadline active
                                                      inactive closed)))))
           (?d (call-interactively #'org-check-deadlines))
           (?b (call-interactively #'org-check-before-date))
           (?a (call-interactively #'org-check-after-date))
           (?D (call-interactively #'org-check-dates-range))
           (?t (call-interactively #'org-show-todo-tree))
           (?T (org-show-todo-tree '(4)))
           (?m (call-interactively #'org-match-sparse-tree))
           ((?p ?P)
            (let* ((kwd (org-icompleting-read
                         "Property: " (mapcar #'list (org-buffer-property-keys))))
                   (value (org-icompleting-read
                           "Value: " (mapcar #'list (org-property-values kwd)))))
              (unless (string-match "\\`{.*}\\'" value)
                (setq value (concat "\"" value "\"")))
              (org-match-sparse-tree arg (concat kwd "=" value))))
           ((?r ?R ?/) (call-interactively #'org-occur))
           (?\\ (call-interactively #'org-remove-occur-highlights))
           (otherwise (user-error "No such sparse tree command \"%c\"" answer)))))

     (el-patch-defun org-babel-execute-src-block (&optional arg info params)
  "Execute the current source code block.
Insert the results of execution into the buffer.  Source code
execution and the collection and formatting of results can be
controlled through a variety of header arguments.

With prefix argument ARG, force re-execution even if an existing
result cached in the buffer would otherwise have been returned.

Optionally supply a value for INFO in the form returned by
`org-babel-get-src-block-info'.

Optionally supply a value for PARAMS which will be merged with
the header arguments specified at the front of the source code
block."
  (interactive)
  (let* ((org-babel-current-src-block-location
      (or org-babel-current-src-block-location
          (nth 5 info)
          (org-babel-where-is-src-block-head)))
     (info (if info (copy-tree info) (org-babel-get-src-block-info))))
    ;; Merge PARAMS with INFO before considering source block
    ;; evaluation since both could disagree.
    (cl-callf org-babel-merge-params (nth 2 info) params)
    (when (org-babel-check-evaluate info)
      (cl-callf org-babel-process-params (nth 2 info))
      (let* ((params (nth 2 info))
         (cache (let ((c (cdr (assq :cache params))))
              (and (not arg) c (string= "yes" c))))
         (new-hash (and cache (org-babel-sha1-hash info :eval)))
         (old-hash (and cache (org-babel-current-result-hash)))
         (current-cache (and new-hash (equal new-hash old-hash))))
    (cond
     (current-cache
      (save-excursion		;Return cached result.
        (goto-char (org-babel-where-is-src-block-result nil info))
        (forward-line)
        (skip-chars-forward " \t")
        (let ((result (org-babel-read-result)))
          (el-patch-remove (message (replace-regexp-in-string "%" "%%" (format "%S" result))))
          result)))
     ((org-babel-confirm-evaluate info)
      (let* ((lang (nth 0 info))
         (result-params (cdr (assq :result-params params)))
         ;; Expand noweb references in BODY and remove any
         ;; coderef.
         (body
          (let ((coderef (nth 6 info))
            (expand
             (if (org-babel-noweb-p params :eval)
                 (org-babel-expand-noweb-references info)
               (nth 1 info))))
            (if (not coderef) expand
              (replace-regexp-in-string
               (org-src-coderef-regexp coderef) "" expand nil nil 1))))
         (dir (cdr (assq :dir params)))
         (default-directory
           (or (and dir (file-name-as-directory (expand-file-name dir)))
               default-directory))
         (cmd (intern (concat "org-babel-execute:" lang)))
         result)
        (unless (fboundp cmd)
          (error "No org-babel-execute function for %s!" lang))
        (message "executing %s code block%s..."
             (capitalize lang)
             (let ((name (nth 4 info)))
               (if name (format " (%s)" name) "")))
        (if (member "none" result-params)
        (progn (funcall cmd body params)
               (message "result silenced"))
          (setq result
            (let ((r (funcall cmd body params)))
              (if (and (eq (cdr (assq :result-type params)) 'value)
                   (or (member "vector" result-params)
                   (member "table" result-params))
                   (not (listp r)))
              (list (list r))
            r)))
          (let ((file (and (member "file" result-params)
                   (cdr (assq :file params)))))
        ;; If non-empty result and :file then write to :file.
        (when file
          ;; If `:results' are special types like `link' or
          ;; `graphics', don't write result to `:file'.  Only
          ;; insert a link to `:file'.
          (when (and result
                 (not (or (member "link" result-params)
                      (member "graphics" result-params))))
            (with-temp-file file
              (insert (org-babel-format-result
                   result
                   (cdr (assq :sep params))))))
          (setq result file))
        ;; Possibly perform post process provided its
        ;; appropriate.  Dynamically bind "*this*" to the
        ;; actual results of the block.
        (let ((post (cdr (assq :post params))))
          (when post
            (let ((*this* (if (not file) result
                    (org-babel-result-to-file
                     file
                     (let ((desc (assq :file-desc params)))
                       (and desc (or (cdr desc) result)))))))
              (setq result (org-babel-ref-resolve post))
              (when file
            (setq result-params (remove "file" result-params))))))
        (org-babel-insert-result
         result result-params info new-hash lang)))
        (run-hooks 'org-babel-after-execute-hook)
        result)))))))
     ))

(eval-after-load "org-macs"
  '(progn
     (el-patch-defun org-mks (table title &optional prompt specials)
       "Select a member of an alist with multiple keys.

TABLE is the alist which should contain entries where the car is a string.
There should be two types of entries.

1. prefix descriptions like (\"a\" \"Description\")
   This indicates that `a' is a prefix key for multi-letter selection, and
   that there are entries following with keys like \"ab\", \"ax\"...

2. Select-able members must have more than two elements, with the first
   being the string of keys that lead to selecting it, and the second a
   short description string of the item.

The command will then make a temporary buffer listing all entries
that can be selected with a single key, and all the single key
prefixes.  When you press the key for a single-letter entry, it is selected.
When you press a prefix key, the commands (and maybe further prefixes)
under this key will be shown and offered for selection.

TITLE will be placed over the selection in the temporary buffer,
PROMPT will be used when prompting for a key.  SPECIALS is an
alist with (\"key\" \"description\") entries.  When one of these
is selected, only the bare key is returned."
       (save-window-excursion
         (let ((inhibit-quit t)
               (buffer (org-switch-to-buffer-other-window "*Org Select*"))
               (prompt (or prompt "Select: "))
               current)
           (unwind-protect
               (catch 'exit
                 (while t
                   (erase-buffer)
                   (insert title "\n\n")
                   (let ((des-keys nil)
                         (allowed-keys '("\C-g"))
                         (tab-alternatives '("\s" "\t" "\r"))
                         (cursor-type nil))
                     ;; Populate allowed keys and descriptions keys
                     ;; available with CURRENT selector.
                     (let ((re (format "\\`%s\\(.\\)\\'"
                                       (if current (regexp-quote current) "")))
                           (prefix (if current (concat current " ") "")))
                       (dolist (entry table)
                         (pcase entry
                           ;; Description.
                           (`(,(and key (pred (string-match re))) ,desc)
                            (let ((k (match-string 1 key)))
                              (push k des-keys)
                              ;; Keys ending in tab, space or RET are equivalent.
                              (if (member k tab-alternatives)
                                  (push "\t" allowed-keys)
                                (push k allowed-keys))
                              (insert prefix "[" k "]" "..." "  " desc "..." "\n")))
                           ;; Usable entry.
                           (`(,(and key (pred (string-match re))) ,desc . ,_)
                            (let ((k (match-string 1 key)))
                              (insert prefix "[" k "]" "     " desc "\n")
                              (push k allowed-keys)))
                           (_ nil))))
                     ;; Insert special entries, if any.
                     (when specials
                       (insert "----------------------------------------------------\
---------------------------\n")
                       (pcase-dolist (`(,key ,description) specials)
                         (insert (format "[%s]     %s\n" key description))
                         (push key allowed-keys)))
                     ;; Display UI and let user select an entry or
                     ;; a sub-level prefix.
                     (goto-char (point-min))
                     (unless (pos-visible-in-window-p (point-max))
                       (org-fit-window-to-buffer))
                     (let ((pressed (org--mks-read-key allowed-keys prompt)))
                       (message "pressed %S specials %S" pressed specials)
                       (setq current (concat current pressed))
                       (cond
                        ((equal pressed "\C-g") (user-error "Abort"))
                        ;; Selection is a prefix: open a new menu.
                        ((member pressed des-keys))
                        ;; Selection matches an association: return it.
                        ((let ((entry (assoc current table)))
                           (and entry (throw 'exit entry))))
                        ;; Selection matches a special entry: return the
                        ;; selection prefix.
                        ((assoc (el-patch-swap current pressed) specials)
                         (throw 'exit (el-patch-swap current pressed)))
                        (t (error "No entry available")))))))
             (when buffer (kill-buffer buffer))))))))

(eval-after-load "org-drill"
  '(progn
     (el-patch-defun org-drill-entry-status ()
       "Returns a list (STATUS DUE AGE) where DUE is the number of days overdue,
zero being due today, -1 being scheduled 1 day in the future.
AGE is the number of days elapsed since the item was created (nil if unknown).
STATUS is one of the following values:
- nil, if the item is not a drill entry, or has an empty body
- :unscheduled
- :future
- :new
- :failed
- :overdue
- :young
- :old
"
       (save-excursion
         (unless (org-at-heading-p)
           (org-back-to-heading))
         (let ((due (org-drill-entry-days-overdue))
               (age (org-drill-entry-days-since-creation t))
               (last-int (org-drill-entry-last-interval 1)))
           (list
            (cond
             ((not (org-drill-entry-p))
              nil)
             ((and (org-entry-empty-p)
                   (let* ((card-type (org-entry-get (point) "DRILL_CARD_TYPE" nil))
                          (dat (cdr (assoc card-type org-drill-card-type-alist))))
                     (or (el-patch-remove (null card-type))
                         (not (third dat)))))
              ;; body is empty, and this is not a card type where empty bodies are
              ;; meaningful, so skip it.
              nil)
             ((null due)                     ; unscheduled - usually a skipped leech
              :unscheduled)
             ;; ((eql -1 due)
             ;;  :tomorrow)
             ((minusp due)                   ; scheduled in the future
              :future)
             ;; The rest of the stati all denote 'due' items ==========================
             ((<= (org-drill-entry-last-quality 9999)
                  org-drill-failure-quality)
              ;; Mature entries that were failed last time are
              ;; FAILED, regardless of how young, old or overdue
              ;; they are.
              :failed)
             ((org-drill-entry-new-p)
              :new)
             ((org-drill-entry-overdue-p due last-int)
              ;; Overdue status overrides young versus old
              ;; distinction.
              ;; Store marker + due, for sorting of overdue entries
              :overdue)
             ((<= (org-drill-entry-last-interval 9999)
                  org-drill-days-before-old)
              :young)
             (t
              :old))
            due age))))

     (el-patch-defun org-drill-maximum-item-count-reached-p ()
       "Returns true if the current drill session has reached the
maximum number of items."
       (el-patch-wrap 2
         (let ((org-drill-maximum-items-per-session
                (or (ignore-errors (string-to-number (org-entry-get (point) "org-drill-maximum-items-per-session" t)))
                    org-drill-maximum-items-per-session)))
           (and org-drill-maximum-items-per-session
                (not *org-drill-cram-mode*)
                (>= (length *org-drill-done-entries*)
                    org-drill-maximum-items-per-session)))))))

(eval-after-load "org-attach"
  '(progn
     (el-patch-defun org-attach-annex-get-maybe (path)
       "Call git annex get PATH (via shell) if using git annex.
Signals an error if the file content is not available and it was not retrieved."
       (let* ((default-directory (expand-file-name org-attach-directory))
              (path-relative (file-relative-name path)))
         (when (and (org-attach-use-annex)
                    (not
                     ((el-patch-swap string-equal string-match-p)
                      (el-patch-swap "found" (regexp-opt '("\nfound" "")))
                      (shell-command-to-string
                       (format "git annex find --format=found --in=here %s"
                               (shell-quote-argument path-relative))))))
           (let ((should-get
                  (if (eq org-attach-annex-auto-get 'ask)
                      (y-or-n-p (format "Run git annex get %s? " path-relative))
                    org-attach-annex-auto-get)))
             (if should-get
                 (progn (message "Running git annex get \"%s\"." path-relative)
                        (call-process "git" nil nil nil "annex" "get" path-relative))
               (error "File %s stored in git annex but it is not available, and was not retrieved"
                      path))))))))

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
     (defun org-agenda-format-item (extra txt &optional level category tags dotime
                                          remove-re habitp)
       "Format TXT to be inserted into the agenda buffer.
In particular, add the prefix and corresponding text properties.

EXTRA must be a string to replace the `%s' specifier in the prefix format.
LEVEL may be a string to replace the `%l' specifier.
CATEGORY (a string, a symbol or nil) may be used to overrule the default
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

           (let* ((category (or category
                                (if (stringp org-category)
                                    org-category
                                  (and org-category (symbol-name org-category)))
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
                  ;; time, tag, effort are needed for the eval of the prefix format
                  (tag (if tags (nth (1- (length tags)) tags) ""))
                  time
                  (ts (if dotime (concat
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
                       s2 (or (match-string (if plain 8 (if srp 4 6)) ts)
                              ;; FUCO: test if s1 is a range, and if
                              ;; so, update s2 to the end time
                              (save-match-data
                                (when (string-match "[012][0-9]:[0-5][0-9]--?\\([012][0-9]:[0-5][0-9]\\)" s1)
                                  (match-string 1 s1)))))

                 ;; If the times are in TXT (not in DOTIMES), and the prefix will list
                 ;; them, we might want to remove them there to avoid duplication.
                 ;; The user can turn this off with a variable.
                 (if (and org-prefix-has-time
                          org-agenda-remove-times-when-in-prefix (or stamp plain)
                          (string-match (concat (regexp-quote s0) " *") txt)
                          (not (equal ?\] (string-to-char (substring txt (match-end 0)))))
                          (if (eq org-agenda-remove-times-when-in-prefix 'beg)
                              (= (match-beginning 0) 0)
                            t))
                     (setq txt (replace-match "" nil nil txt))))
               ;; Normalize the time(s) to 24 hour
               (if s1 (setq s1 (org-get-time-of-day s1 'string t)))
               (if s2 (setq s2 (org-get-time-of-day s2 'string t)))

               ;; Try to set s2 if s1 and `org-agenda-default-appointment-duration' are set
               (let (org-time-clocksum-use-effort-durations)
                 (when (and s1 (not s2) org-agenda-default-appointment-duration)
                   (setq s2
                         (org-minutes-to-clocksum-string
                          (+ (org-hh:mm-string-to-minutes s1)
                             org-agenda-default-appointment-duration)))))

               ;; Compute the duration
               (when s2
                 (setq duration (- (org-hh:mm-string-to-minutes s2)
                                   (org-hh:mm-string-to-minutes s1)))))

             (when (string-match "\\([ \t]+\\)\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$" txt)
               ;; Tags are in the string
               (if (or (eq org-agenda-remove-tags t)
                       (and org-agenda-remove-tags
                            org-prefix-has-tag))
                   (setq txt (replace-match "" t t txt))
                 (setq txt (replace-match
                            (concat (make-string (max (- 50 (length txt)) 1) ?\ )
                                    (match-string 2 txt))
                            t t txt))))
             (when remove-re
               (while (string-match remove-re txt)
                 (setq txt (replace-match "" t t txt))))

             ;; Set org-heading property on `txt' to mark the start of the
             ;; heading.
             (add-text-properties 0 (length txt) '(org-heading t) txt)

             ;; Prepare the variables needed in the eval of the compiled format
             (if org-prefix-has-breadcrumbs
                 (setq breadcrumbs (org-with-point-at (org-get-at-bol 'org-marker)
                                     (let ((s (org-display-outline-path nil nil "->" t)))
                                       (if (eq "" s) "" (concat s "->"))))))
             (setq time (cond (s2 (concat
                                   (org-agenda-time-of-day-to-ampm-maybe s1)
                                   "-" (org-agenda-time-of-day-to-ampm-maybe s2)
                                   (if org-agenda-timegrid-use-ampm " ")))
                              (s1 (concat
                                   (org-agenda-time-of-day-to-ampm-maybe s1)
                                   (if org-agenda-timegrid-use-ampm
                                       "........ "
                                     "......")))
                              (t ""))
                   extra (or (and (not habitp) extra) "")
                   category (if (symbolp category) (symbol-name category) category)
                   level (or level ""))
             (if (string-match org-bracket-link-regexp category)
                 (progn
                   (setq l (if (match-end 3)
                               (- (match-end 3) (match-beginning 3))
                             (- (match-end 1) (match-beginning 1))))
                   (when (< l (or org-prefix-category-length 0))
                     (setq category (copy-sequence category))
                     (org-add-props category nil
                       'extra-space (make-string
                                     (- org-prefix-category-length l 1) ?\ ))))
               (if (and org-prefix-category-max-length
                        (>= (length category) org-prefix-category-max-length))
                   (setq category (substring category 0 (1- org-prefix-category-max-length)))))
             ;; Evaluate the compiled format
             (setq rtn (concat (eval formatter) txt))

             ;; And finally add the text properties
             (remove-text-properties 0 (length rtn) '(line-prefix t wrap-prefix t) rtn)
             (org-add-props rtn nil
               'org-category category
               'tags (mapcar 'org-downcase-keep-props tags)
               'org-highest-priority org-highest-priority
               'org-lowest-priority org-lowest-priority
               'time-of-day time-of-day
               'duration duration
               'breadcrumbs breadcrumbs
               'txt txt
               'level level
               'time time
               'extra extra
               'format org-prefix-format-compiled
               'dotime dotime)))))))

(eval-after-load "org-clock"
  '(progn
     (el-patch-defun org-dblock-write:clocktable (params)
       "Write the standard clocktable."
       (setq params (org-combine-plists org-clocktable-defaults params))
       (catch 'exit
         (let* ((scope (plist-get params :scope))
                (files (pcase scope
                         (`agenda
                          (org-agenda-files t))
                         (`agenda-with-archives
                          (org-add-archive-files (org-agenda-files t)))
                         (`file-with-archives
                          (and buffer-file-name
                               (org-add-archive-files (list buffer-file-name))))
                         ((pred functionp) (funcall scope))
                         ((pred consp) scope)
                         (_ (or (buffer-file-name) (current-buffer)))))
                (block (plist-get params :block))
                (ts (plist-get params :tstart))
                (te (plist-get params :tend))
                (ws (plist-get params :wstart))
                (ms (plist-get params :mstart))
                (step (plist-get params :step))
                (formatter (or (plist-get params :formatter)
                               org-clock-clocktable-formatter
                               'org-clocktable-write-default))
                cc)
           ;; Check if we need to do steps
           (when block
             ;; Get the range text for the header
             (setq cc (org-clock-special-range block nil t ws ms)
                   ts (car cc)
                   te (nth 1 cc)))
           (when step
             ;; Write many tables, in steps
             (unless (or block (and ts te))
               (error "Clocktable `:step' can only be used with `:block' or `:tstart,:end'"))
             (org-clocktable-steps params)
             (throw 'exit nil))

           (org-agenda-prepare-buffers (if (consp files) files (list files)))

           (let ((origin (point))
                 (tables
                  (if (consp files)
                      (mapcar (lambda (file)
                                (with-current-buffer (find-buffer-visiting file)
                                  (save-excursion
                                    (save-restriction
                                      (org-clock-get-table-data file params)))))
                              files)
                    ;; Get the right restriction for the scope.
                    (save-restriction
                      (cond
                       ((not scope))	     ;use the restriction as it is now
                       ((eq scope 'file) (widen))
                       ((eq scope 'subtree) (org-narrow-to-subtree))
                       ((eq scope 'tree)
                        (while (org-up-heading-safe))
                        (org-narrow-to-subtree))
                       ((and (symbolp scope)
                             (string-match "\\`tree\\([0-9]+\\)\\'"
                                           (symbol-name scope)))
                        (let ((level (string-to-number
                                      (match-string 1 (symbol-name scope)))))
                          (catch 'exit
                            (while (org-up-heading-safe)
                              (looking-at org-outline-regexp)
                              (when (<= (org-reduced-level (funcall outline-level))
                                        level)
                                (throw 'exit nil))))
                          (org-narrow-to-subtree))))
                      (list (org-clock-get-table-data nil params)))))
                 (multifile
                  ;; Even though `file-with-archives' can consist of
                  ;; multiple files, we consider this is one extended file
                  ;; instead.
                  (and (consp files) (not (eq scope 'file-with-archives)))))

             (el-patch-add (setq tables (my-merge-clockreport-tables tables)))

             (funcall formatter
                      origin
                      tables
                      (org-combine-plists params `(:multifile ,multifile)))))))

     (el-patch-defun org-clock-get-table-data (file params)
       "Get the clocktable data for file FILE, with parameters PARAMS.
FILE is only for identification - this function assumes that
the correct buffer is current, and that the wanted restriction is
in place.
The return value will be a list with the file name and the total
file time (in minutes) as 1st and 2nd elements.  The third element
of this list will be a list of headline entries.  Each entry has the
following structure:

  (LEVEL HEADLINE TAGS TIMESTAMP TIME PROPERTIES)

LEVEL:      The level of the headline, as an integer.  This will be
            the reduced level, so 1,2,3,... even if only odd levels
            are being used.
HEADLINE:   The text of the headline.  Depending on PARAMS, this may
            already be formatted like a link.
TAGS:       The list of tags of the headline.
TIMESTAMP:  If PARAMS require it, this will be a time stamp found in the
            entry, any of SCHEDULED, DEADLINE, NORMAL, or first inactive,
            in this sequence.
TIME:       The sum of all time spend in this tree, in minutes.  This time
            will of cause be restricted to the time block and tags match
            specified in PARAMS.
PROPERTIES: The list properties specified in the `:properties' parameter
            along with their value, as an alist following the pattern
            (NAME . VALUE)."
       (let* ((maxlevel (or (plist-get params :maxlevel) 3))
              (timestamp (plist-get params :timestamp))
              (ts (plist-get params :tstart))
              (te (plist-get params :tend))
              (ws (plist-get params :wstart))
              (ms (plist-get params :mstart))
              (block (plist-get params :block))
              (link (plist-get params :link))
              (tags (plist-get params :tags))
              (match (plist-get params :match))
              (properties (plist-get params :properties))
              (inherit-property-p (plist-get params :inherit-props))
              (matcher (and match (cdr (org-make-tags-matcher match))))
              (el-patch-add path) cc st p tbl)

         (setq org-clock-file-total-minutes nil)
         (when block
           (setq cc (org-clock-special-range block nil t ws ms)
                 ts (car cc)
                 te (nth 1 cc)))
         (when (integerp ts) (setq ts (calendar-gregorian-from-absolute ts)))
         (when (integerp te) (setq te (calendar-gregorian-from-absolute te)))
         (when (and ts (listp ts))
           (setq ts (format "%4d-%02d-%02d" (nth 2 ts) (car ts) (nth 1 ts))))
         (when (and te (listp te))
           (setq te (format "%4d-%02d-%02d" (nth 2 te) (car te) (nth 1 te))))
         ;; Now the times are strings we can parse.
         (if ts (setq ts (org-matcher-time ts)))
         (if te (setq te (org-matcher-time te)))
         (save-excursion
           (org-clock-sum ts te
                          (when matcher
                            `(lambda ()
                               (let* ((todo (org-get-todo-state))
                                      (tags-list (org-get-tags))
                                      (org-scanner-tags tags-list)
                                      (org-trust-scanner-tags t))
                                 (funcall ,matcher todo tags-list nil)))))
           (goto-char (point-min))
           (setq st t)
           (while (or (and (bobp) (prog1 st (setq st nil))
                           (get-text-property (point) :org-clock-minutes)
                           (setq p (point-min)))
                      (setq p (next-single-property-change
                               (point) :org-clock-minutes)))
             (goto-char p)
             (let ((time (get-text-property p :org-clock-minutes)))
               (when (and time (> time 0) (org-at-heading-p))
                 (let ((level (org-reduced-level (org-current-level))))
                   (when (<= level maxlevel)
                     (let* ((headline (org-get-heading t t t t))
                            (hdl
                             (if (not link) headline
                               (let ((search
                                      (org-make-org-heading-search-string headline)))
                                 (org-make-link-string
                                  (if (not (buffer-file-name)) search
                                    (format "file:%s::%s" (buffer-file-name) search))
                                  ;; Prune statistics cookies.  Replace
                                  ;; links with their description, or
                                  ;; a plain link if there is none.
                                  (org-trim
                                   (org-link-display-format
                                    (replace-regexp-in-string
                                     "\\[[0-9]+%\\]\\|\\[[0-9]+/[0-9]+\\]" ""
                                     headline)))))))
                            (tgs (and tags (org-get-tags)))
                            (tsp
                             (and timestamp
                                  (cl-some (lambda (p) (org-entry-get (point) p))
                                           '("SCHEDULED" "DEADLINE" "TIMESTAMP"
                                             "TIMESTAMP_IA"))))
                            (props
                             (and properties
                                  (delq nil
                                        (mapcar
                                         (lambda (p)
                                           (let ((v (org-entry-get
                                                     (point) p inherit-property-p)))
                                             (and v (cons p v))))
                                         properties)))))
                       (el-patch-add
                         (while (and path (>= (caar path) level))
                           (pop path))
                         (push (list level headline) path))
                       (push (list level hdl tgs tsp time
                                   (el-patch-swap
                                     props
                                     (append props
                                             (list :archive-merger-path
                                               (substring-no-properties
                                                (mapconcat
                                                 #'cadr
                                                 (reverse path)
                                                 "/"))))))
                             tbl)))))))
           (list file org-clock-file-total-minutes (nreverse tbl)))))

     (defun my-merge-clockreport-tables (tables)
       "Merge the same clock entires in TABLES into one table.

Entries are identified by their full path."
       (let ((final-table (make-hash-table :test 'equal))
             (order 0)
             (total-time 0))
         (-each tables
           (-lambda ((_ file-total entries))
             (cl-incf total-time file-total)
             (-each entries
               (-lambda ((entry &as level headline tags timestamp time properties))
                 (let ((path (plist-get properties :archive-merger-path)))
                   (-if-let (final-entry (gethash path final-table))
                       ;; set 5th item because we cons the order in front
                       (setf (nth 5 final-entry)
                             (+ (nth 5 final-entry)
                                (nth 4 entry)))
                     (puthash path (cons order entry) final-table)
                     (cl-incf order)))))))
         (let* ((final-entries (hash-table-values final-table))
                (final-entries (--sort (string-collate-lessp
                                        (plist-get (nth 6 it) :archive-merger-path)
                                        (plist-get (nth 6 other) :archive-merger-path)
                                        nil
                                        :ignore-case)
                                       final-entries))
                (final-entries (-map #'cdr final-entries))
                (first-table (car tables)))
           (list (list (car first-table) total-time final-entries)))))))

(eval-after-load "windmove"
  '(progn
     ;; Call user-error instead of error
     (defun windmove-do-window-select (dir &optional arg window)
       "Move to the window at direction DIR.
DIR, ARG, and WINDOW are handled as by `windmove-other-window-loc'.
If no window is at direction DIR, an error is signaled."
       (let ((other-window (windmove-find-other-window dir arg window)))
         (cond ((null other-window)
                (user-error "No window %s from selected window" dir))
               ((and (window-minibuffer-p other-window)
                     (not (minibuffer-window-active-p other-window)))
                (user-error "Minibuffer is inactive"))
               (t
                (select-window other-window)))))))

(eval-after-load "dired"
  '(progn
     ;; Do not only join filenames with space because there can be spaces
     ;; in the paths.  So we quote the files with space with ''
     (defun dired-copy-filename-as-kill (&optional arg)
       "Copy names of marked (or next ARG) files into the kill ring.
The names are separated by a space.
With a zero prefix arg, use the absolute file name of each marked file.
With \\[universal-argument], use the file name relative to the dired buffer's
`default-directory'.  (This still may contain slashes if in a subdirectory.)

If on a subdir headerline, use absolute subdirname instead;
prefix arg and marked files are ignored in this case.

You can then feed the file name(s) to other commands with \\[yank]."
       (interactive "P")
       (let ((string
              (or (dired-get-subdir)
                  ;; FUCO: identity changed to a function which wraps in
                  ;; quotes if there is a space present
                  (mapconcat (lambda (filename)
                               (if (string-match-p " " filename)
                                   (s-wrap filename "'" "'")
                                 filename))
                             (if arg
                                 (cond ((zerop (prefix-numeric-value arg))
                                        (dired-get-marked-files))
                                       ((consp arg)
                                        (dired-get-marked-files t))
                                       (t
                                        (dired-get-marked-files
                                         'no-dir (prefix-numeric-value arg))))
                               (dired-get-marked-files 'no-dir))
                             " "))))
         (if (eq last-command 'kill-region)
             (kill-append string nil)
           (kill-new string))
         (message "%s" string)))))

(eval-after-load "latex"
  '(progn
     ;; If we still get a byte-compiled error, we also need to reload
     ;; the defcustom LaTeX-math-list, because it uses this function
     ;; as a setter callback which is inlined.
     (el-patch-defun LaTeX-math-initialize ()
       (let ((math (reverse (append LaTeX-math-list LaTeX-math-default)))
             (map LaTeX-math-keymap)
             (unicode (and LaTeX-math-menu-unicode (fboundp 'decode-char))))
         (while math
           (let* ((entry (car math))
                  (key (nth 0 entry))
                  (prefix
                   (and unicode
                        (nth 3 entry)))
                  value menu name)
             (setq math (cdr math))
             (if (and prefix
                      (setq prefix (decode-char 'ucs (nth 3 entry))))
                 (setq prefix (concat (string prefix) " \\"))
               (setq prefix "\\"))
             (if (listp (cdr entry))
                 (setq value (nth 1 entry)
                       menu (nth 2 entry))
               (setq value (cdr entry)
                     menu nil))
             (if (stringp value)
                 (progn
                   (setq name (intern (concat "LaTeX-math-" value)))
                   (fset name (list 'lambda (list 'arg) (list 'interactive "*P")
                                    (list 'LaTeX-math-insert value 'arg))))
               (setq name value))
             (if key
                 (progn
                   (setq key (cond ((numberp key) (char-to-string key))
                                   ((stringp key) (read-kbd-macro key))
                                   (t (vector key))))
                   (define-key map key name)))
             (el-patch-remove
               (if menu
                   (let ((parent LaTeX-math-menu))
                     (if (listp menu)
                         (progn
                           (while (cdr menu)
                             (let ((sub (assoc (car menu) LaTeX-math-menu)))
                               (if sub
                                   (setq parent sub)
                                 (setcdr parent (cons (list (car menu)) (cdr parent))))
                               (setq menu (cdr menu))))
                           (setq menu (car menu))))
                     (let ((sub (assoc menu parent)))
                       (if sub
                           (if (stringp value)
                               (setcdr sub (cons (vector (concat prefix value)
                                                         name t)
                                                 (cdr sub)))
                             (error "Cannot have multiple special math menu items"))
                         (setcdr parent
                                 (cons (if (stringp value)
                                           (list menu (vector (concat prefix value)
                                                              name t))
                                         (vector menu name t))
                                       (cdr parent))))))))))
         ;; Make the math prefix char available if it has not been used as a prefix.
         (unless (lookup-key map (LaTeX-math-abbrev-prefix))
           (define-key map (LaTeX-math-abbrev-prefix) 'self-insert-command))))))

(eval-after-load "json"
  '(progn
     (defun json-read-object ()
       "Read the JSON object at point."
       ;; Skip over the "{"
       (json-advance)
       (json-skip-whitespace)
       ;; read key/value pairs until "}"
       (let ((elements (json-new-object))
             key value)
         (while (not (= (json-peek) ?}))
           (json-skip-whitespace)
           (setq key (json-read-string))
           (json-skip-whitespace)
           (if (= (json-peek) ?:)
               (json-advance)
             (signal 'json-object-format (list ":" (json-peek))))
           (json-skip-whitespace)
           (when json-pre-element-read-function
             (funcall json-pre-element-read-function key))
           (setq value (json-read))
           (when json-post-element-read-function
             (funcall json-post-element-read-function))
           (setq elements (json-add-to-object elements key value))
           (json-skip-whitespace)
           (when (/= (json-peek) ?})
             (if (= (json-peek) ?,)
                 (json-advance)
               (signal 'json-object-format (list "," (json-peek))))))
         ;; Skip over the "}"
         (json-advance)
         (pcase json-object-type
           (`alist (nreverse elements))
           (`plist elements)
           (_ elements))))

     ;; ;; add `nreverse' to the very end to preserve order of keys read.
     ;; (defun json-read-object ()
     ;;   "Read the JSON object at point."
     ;;   ;; Skip over the "{"
     ;;   (json-advance)
     ;;   (json-skip-whitespace)
     ;;   ;; read key/value pairs until "}"
     ;;   (let ((elements (json-new-object))
     ;;         key value)
     ;;     (while (not (char-equal (json-peek) ?}))
     ;;       (json-skip-whitespace)
     ;;       (setq key (json-read-string))
     ;;       (json-skip-whitespace)
     ;;       (if (char-equal (json-peek) ?:)
     ;;           (json-advance)
     ;;         (signal 'json-object-format (list ":" (json-peek))))
     ;;       (setq value (json-read))
     ;;       (setq elements (json-add-to-object elements key value))
     ;;       (json-skip-whitespace)
     ;;       (unless (char-equal (json-peek) ?})
     ;;         (if (char-equal (json-peek) ?,)
     ;;             (json-advance)
     ;;           (signal 'json-object-format (list "," (json-peek))))))
     ;;     ;; Skip over the "}"
     ;;     (json-advance)
     ;;     (if (listp elements)
     ;;         (nreverse elements)
     ;;       elements)))
     ))

(provide 'my-redef)
;;; my-redef.el ends here
