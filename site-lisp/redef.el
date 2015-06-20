;; this file contains redefinitions of the internals that are broken
;; on my system

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
  '(defun calendar-basic-setup (&optional arg nodisplay)
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
              ;; (split-height-threshold 1000)
              ;; (split-width-threshold calendar-split-width-threshold)
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
           (if (window-splittable-p t) (split-window-right))
           (pop-to-buffer calendar-buffer)
           ;; Has the window already been split vertically?
           (when (and (not (window-dedicated-p))
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
             (diary-view-entries))))
     (if calendar-view-holidays-initially-flag
         (let* ((diary-buffer (get-file-buffer diary-file))
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
       "Check if the current headline contains a repeated deadline/schedule.
If yes, set TODO state back to what it was and change the base date
of repeating deadline/scheduled time stamps to new date.
This function is run automatically after each state change to a DONE state."
       ;; last-state is dynamically scoped into this function
       (let* ((repeat (org-get-repeat))
              (aa (assoc org-last-state org-todo-kwd-alist))
              (interpret (nth 1 aa))
              (head (nth 2 aa))
              (whata '(("h" . hour) ("d" . day) ("m" . month) ("y" . year)))
              (msg "Entry repeats: ")
              (org-log-done nil)
              (org-todo-log-states nil)
              re type n what ts time to-state)
         (when repeat
           (if (eq org-log-repeat t) (setq org-log-repeat 'state))
           (setq to-state (or (org-entry-get nil "REPEAT_TO_STATE")
                              org-todo-repeat-to-state))
           (unless (and to-state (member to-state org-todo-keywords-1))
             (setq to-state (if (eq interpret 'type) org-last-state head)))
           (org-todo to-state)
           (when (or org-log-repeat (org-entry-get nil "CLOCK"))
             (org-entry-put nil "LAST_REPEAT" (format-time-string
                                               (org-time-stamp-format t t))))
           (when org-log-repeat
             (if (or (memq 'org-add-log-note (default-value 'post-command-hook))
                     (memq 'org-add-log-note post-command-hook))
                 ;; OK, we are already setup for some record
                 (if (eq org-log-repeat 'note)
                     ;; make sure we take a note, not only a time stamp
                     (setq org-log-note-how 'note))
               ;; Set up for taking a record
               (org-add-log-setup 'state (or done-word (car org-done-keywords))
                                  org-last-state
                                  'findpos org-log-repeat)))
           (org-back-to-heading t)
           (org-add-planning-info nil nil 'closed)
           (setq re (concat "\\(" org-scheduled-time-regexp "\\)\\|\\("
                            org-deadline-time-regexp "\\)\\|\\("
                            org-ts-regexp "\\)"))
           (while (re-search-forward
                   re (save-excursion (outline-next-heading) (point)) t)
             (setq type (if (match-end 1) org-scheduled-string
                          (if (match-end 3) org-deadline-string "Plain:"))
                   ts (match-string (if (match-end 2) 2 (if (match-end 4) 4 0)))
                   ;; FUCO: move this up so we can use it in the future-test
                   time (save-match-data (org-time-string-to-time ts)))
             (when (and (string-match "\\([.+]\\)?\\(\\+[0-9]+\\)\\([hdwmy]\\)" ts)
                        ;; FUCO: test if the timestamp is in the past and only shift it then.
                        (time-less-p time (current-time)))
               (setq n (string-to-number (match-string 2 ts))
                     what (match-string 3 ts))
               (if (equal what "w") (setq n (* n 7) what "d"))
               (if (and (equal what "h") (not (string-match "[0-9]\\{1,2\\}:[0-9]\\{2\\}" ts)))
                   (user-error "Cannot repeat in Repeat in %d hour(s) because no hour has been set" n))
               ;; Preparation, see if we need to modify the start date for the change
               (when (match-end 1)
                 (cond
                  ((equal (match-string 1 ts) ".")
                   ;; Shift starting date to today
                   (org-timestamp-change
                    (- (org-today) (time-to-days time))
                    'day))
                  ((equal (match-string 1 ts) "+")
                   (let ((nshiftmax 10) (nshift 0))
                     (while (or (= nshift 0)
                                (<= (time-to-days time)
                                    (time-to-days (current-time))))
                       (when (= (incf nshift) nshiftmax)
                         (or (y-or-n-p (message "%d repeater intervals were not enough to shift date past today.  Continue? " nshift))
                             (error "Abort")))
                       (org-timestamp-change n (cdr (assoc what whata)))
                       (org-at-timestamp-p t)
                       (setq ts (match-string 1))
                       (setq time (save-match-data (org-time-string-to-time ts)))))
                   (org-timestamp-change (- n) (cdr (assoc what whata)))
                   ;; rematch, so that we have everything in place for the real shift
                   (org-at-timestamp-p t)
                   (setq ts (match-string 1))
                   (string-match "\\([.+]\\)?\\(\\+[0-9]+\\)\\([hdwmy]\\)" ts))))
               (save-excursion (org-timestamp-change n (cdr (assoc what whata)) nil t))
               (setq msg (concat msg type " " org-last-changed-timestamp " "))))
           (setq org-log-post-message msg)
           (message "%s" msg))))))
