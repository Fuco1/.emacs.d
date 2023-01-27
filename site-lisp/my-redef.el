;;; my-redef.el --- Global redefinitions of Emacs's own code  -*- lexical-binding: t; -*-

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

(eval-after-load "org-table"
  '(progn
     (el-patch-defun org-table-expand-lhs-ranges (equations)
       "Expand list of formulas.
If some of the RHS in the formulas are ranges or a row reference,
expand them to individual field equations for each field.  This
function assumes the table is already analyzed (i.e., using
`org-table-analyze')."
       (let (res)
         (dolist (e equations (nreverse res))
           (let ((lhs (car e))
                 (rhs (cdr e)))
             (cond
              ((string-match-p "\\`@-?[-+0-9]+\\$-?[0-9]+\\'" lhs)
               ;; This just refers to one fixed field.
               (push e res))
              ((string-match-p "\\`[a-zA-Z][_a-zA-Z0-9]*\\'" lhs)
               ;; This just refers to one fixed named field.
               (push e res))
              ((string-match-p "\\`\\$[0-9]+\\'" lhs)
               ;; Column formulas are treated specially and are not
               ;; expanded.
               (push e res))
              (el-patch-add
                ((string-match "\\`\\@[I]+\\$\\([0-9]+\\)\\'" lhs)
                 ;; Hline relative LHS formulas are expanded on the left and
                 ;; pushed
                 (let* ((current-col (string-to-number (match-string 1 lhs)))
                        (range (org-table-get-range
                                lhs org-table-current-begin-pos current-col
                                nil 'corners))
                        (r1 (org-table-line-to-dline (nth 0 range)))
                        (c1 (nth 1 range))
                        (r2 (org-table-line-to-dline (nth 2 range) 'above))
                        (c2 (nth 3 range)))
                   (push (cons (format "@%d$%d" r1 c1) rhs) res))))
              ((string-match "\\`@[0-9]+\\'" lhs)
               (dotimes (ic org-table-current-ncol)
                 (push (cons (propertize (format "%s$%d" lhs (1+ ic)) :orig-eqn e)
                             rhs)
                       res)))
              (t
               (let* ((range (org-table-get-range
                              lhs org-table-current-begin-pos 1 nil 'corners))
                      (r1 (org-table-line-to-dline (nth 0 range)))
                      (c1 (nth 1 range))
                      (r2 (org-table-line-to-dline (nth 2 range) 'above))
                      (c2 (nth 3 range)))
                 (cl-loop for ir from r1 to r2 do
                          (cl-loop for ic from c1 to c2 do
                                   (push (cons (propertize
                                                (format "@%d$%d" ir ic) :orig-eqn e)
                                               rhs)
                                         res))))))))))


     (el-patch-defun org-table-recalculate (&optional all noalign)
       "Recalculate the current table line by applying all stored formulas.

With prefix arg ALL, do this for all lines in the table.

When called with a `\\[universal-argument] \\[universal-argument]' prefix, or \
if ALL is the symbol `iterate',
recompute the table until it no longer changes.

If NOALIGN is not nil, do not re-align the table after the computations
are done.  This is typically used internally to save time, if it is
known that the table will be realigned a little later anyway."
       (interactive "P")
       (unless (memq this-command org-recalc-commands)
         (push this-command org-recalc-commands))
       (unless (org-at-table-p) (user-error "Not at a table"))
       (if (or (eq all 'iterate) (equal all '(16)))
           (org-table-iterate)
         (org-table-analyze)
         (let* ((eqlist (sort (org-table-get-stored-formulas)
                              (lambda (a b) (string< (car a) (car b)))))
                (inhibit-redisplay (not debug-on-error))
                (line-re org-table-dataline-regexp)
                (log-first-time (current-time))
                (log-last-time log-first-time)
                (cnt 0)
                beg end eqlcol eqlfield)
           ;; Insert constants in all formulas.
           (when eqlist
             (org-table-save-field
              ;; Expand equations, then split the equation list between
              ;; column formulas and field formulas.
              (dolist (eq eqlist)
                (let* ((rhs (org-table-formula-substitute-names
                             (org-table-formula-handle-first/last-rc (cdr eq))))
                       (old-lhs (car eq))
                       (lhs
                        (org-table-formula-handle-first/last-rc
                         (cond
                          (el-patch-remove
                            ((string-match "\\`@-?I+" old-lhs)
                             (user-error "Can't assign to hline relative reference")))
                          ((string-match "\\`$[<>]" old-lhs)
                           (let ((new (org-table-formula-handle-first/last-rc
                                       old-lhs)))
                             (when (assoc new eqlist)
                               (user-error "\"%s=\" formula tries to overwrite \
existing formula for column %s"
                                           old-lhs
                                           new))
                             new))
                          (t old-lhs)))))
                  (if (string-match-p "\\`\\$[0-9]+\\'" lhs)
                      (push (cons lhs rhs) eqlcol)
                    (push (cons lhs rhs) eqlfield))))
              (setq eqlcol (nreverse eqlcol))
              ;; Expand ranges in lhs of formulas
              (setq eqlfield (org-table-expand-lhs-ranges (nreverse eqlfield)))
              ;; Get the correct line range to process.
              (if all
                  (progn
                    (setq end (copy-marker (org-table-end)))
                    (goto-char (setq beg org-table-current-begin-pos))
                    (cond
                     ((re-search-forward org-table-calculate-mark-regexp end t)
                      ;; This is a table with marked lines, compute selected
                      ;; lines.
                      (setq line-re org-table-recalculate-regexp))
                     ;; Move forward to the first non-header line.
                     ((and (re-search-forward org-table-dataline-regexp end t)
                           (re-search-forward org-table-hline-regexp end t)
                           (re-search-forward org-table-dataline-regexp end t))
                      (setq beg (match-beginning 0)))
                     ;; Just leave BEG at the start of the table.
                     (t nil)))
                (setq beg (line-beginning-position)
                      end (copy-marker (line-beginning-position 2))))
              (goto-char beg)
              ;; Mark named fields untouchable.  Also check if several
              ;; field/range formulas try to set the same field.
              (remove-text-properties beg end '(:org-untouchable t))
              (let ((current-line (count-lines org-table-current-begin-pos
                                               (line-beginning-position)))
                    seen-fields)
                (dolist (eq eqlfield)
                  (let* ((name (car eq))
                         (location (assoc name org-table-named-field-locations))
                         (eq-line (or (nth 1 location)
                                      (and (string-match "\\`@\\([0-9]+\\)" name)
                                           (aref org-table-dlines
                                                 (string-to-number
                                                  (match-string 1 name))))))
                         (reference
                          (if location
                              ;; Turn field coordinates associated to NAME
                              ;; into an absolute reference.
                              (format "@%d$%d"
                                      (org-table-line-to-dline eq-line)
                                      (nth 2 location))
                            name)))
                    (when (member reference seen-fields)
                      (user-error "Several field/range formulas try to set %s"
                                  reference))
                    (push reference seen-fields)
                    (when (or all (eq eq-line current-line))
                      (org-table-goto-field name)
                      (org-table-put-field-property :org-untouchable t)))))
              ;; Evaluate the column formulas, but skip fields covered by
              ;; field formulas.
              (goto-char beg)
              (while (re-search-forward line-re end t)
                (unless (string-match "\\` *[_^!$/] *\\'" (org-table-get-field 1))
                  ;; Unprotected line, recalculate.
                  (cl-incf cnt)
                  (when all
                    (setq log-last-time
                          (org-table-message-once-per-second
                           log-last-time
                           "Re-applying formulas to full table...(line %d)" cnt)))
                  (if (markerp org-last-recalc-line)
                      (move-marker org-last-recalc-line (line-beginning-position))
                    (setq org-last-recalc-line
                          (copy-marker (line-beginning-position))))
                  (dolist (entry eqlcol)
                    (goto-char org-last-recalc-line)
                    (org-table-goto-column
                     (string-to-number (substring (car entry) 1)) nil 'force)
                    (unless (get-text-property (point) :org-untouchable)
                      (org-table-eval-formula
                       nil (cdr entry) 'noalign 'nocst 'nostore 'noanalysis)))))
              ;; Evaluate the field formulas.
              (dolist (eq eqlfield)
                (let ((reference (car eq))
                      (formula (cdr eq)))
                  (setq log-last-time
                        (org-table-message-once-per-second
                         (and all log-last-time)
                         "Re-applying formula to field: %s" (car eq)))
                  (org-table-goto-field
                   reference
                   ;; Possibly create a new column, as long as
                   ;; `org-table-formula-create-columns' allows it.
                   (let ((column-count (progn (end-of-line)
                                              (1- (org-table-current-column)))))
                     (lambda (column)
                       (when (> column 1000)
                         (user-error "Formula column target too large"))
                       (and (> column column-count)
                            (or (eq org-table-formula-create-columns t)
                                (and (eq org-table-formula-create-columns 'warn)
                                     (progn
                                       (org-display-warning
                                        "Out-of-bounds formula added columns")
                                       t))
                                (and (eq org-table-formula-create-columns 'prompt)
                                     (yes-or-no-p
                                      "Out-of-bounds formula.  Add columns? "))
                                (user-error
                                 "Missing columns in the table.  Aborting"))))))
                  (org-table-eval-formula nil formula t t t t))))
             ;; Clean up markers and internal text property.
             (remove-text-properties (point-min) (point-max) '(org-untouchable t))
             (set-marker end nil)
             (unless noalign
               (when org-table-may-need-update (org-table-align))
               (when all
                 (org-table-message-once-per-second
                  log-first-time "Re-applying formulas to %d lines... done" cnt)))
             (org-table-message-once-per-second
              (and all log-first-time) "Re-applying formulas... done")))))))

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

(eval-after-load "jsonrpc"
  '(progn
     (el-patch-defun jsonrpc--log-event (connection message &optional type)
       "Log a JSONRPC-related event.
CONNECTION is the current connection.  MESSAGE is a JSON-like
plist.  TYPE is a symbol saying if this is a client or server
originated."
       (let ((max (jsonrpc--events-buffer-scrollback-size connection)))
         (when (or (null max) (cl-plusp max))
           (with-current-buffer (jsonrpc-events-buffer connection)
             (cl-destructuring-bind (&key method id error &allow-other-keys) message
               (let* ((inhibit-read-only t)
                      (subtype (cond ((and method id)       'request)
                                     (method                'notification)
                                     (id                    'reply)
                                     (t                     'message)))
                      (type
                       (concat (format "%s" (or type 'internal))
                               (if type
                                   (format "-%s" subtype)))))
                 (goto-char (point-max))
                 (prog1
                     (let ((msg (format "[%s]%s%s %s:\n%s"
                                        type
                                        (if id (format " (id:%s)" id) "")
                                        (if error " ERROR" "")
                                        (current-time-string)
                                        (pp-to-string message))))
                       (when error
                         (setq msg (propertize msg 'face 'error)))
                       (insert-before-markers msg))
                   ;; Trim the buffer if it's too large
                   (when max
                     (save-excursion
                       (goto-char (point-min))
                       (while (> (buffer-size) max)
                         (delete-region
                          (point)
                          (el-patch-swap
                            (progn (forward-line 1)
                                   (forward-sexp 1)
                                   (forward-line 2)
                                   (point))
                            ;; PATCH: if the sexp at point is too long
                            ;; and can't be parsed, jsonrpc used to
                            ;; crash.  Here we just return first 100
                            ;; chars and bail.
                            (or (ignore-errors
                                  (progn (forward-line 1)
                                         (forward-sexp 1)
                                         (forward-line 2)
                                         (point)))
                                (min (point-max) 100))))))))))))))))

(provide 'my-redef)
;;; my-redef.el ends here
