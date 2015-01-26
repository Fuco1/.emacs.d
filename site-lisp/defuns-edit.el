(require 'thingatpt)

(declare-function org-table-p "org")
(declare-function org-beginning-of-line "org")
(declare-function org-end-of-line "org")
(declare-function cua--rectangle-top "cua-base")
(declare-function cua--rectangle-bot "cua-base")
(declare-function cua-resize-rectangle-right "cua-base")

(defun my-forward-whitespace ()
  "Like `forward-whitespace' but do not skip over a word if there
is no preceding whitespace.

 |  foo  bar =>   |foo  bar
   |foo  bar =>   |foo  bar ;; not foo  |bar"
  (interactive)
  (when (looking-at "[ \t\n]") (forward-whitespace 1)))

(defun my-kill-whitespace (&optional forward)
  "Kill all the whitespace characters backwards until hitting
non-whitespace character.  With prefix argument, kill in the
forward direction."
  (interactive "P")
  (let ((old-point (point)))
    (if forward
        (skip-syntax-forward " ")
      (skip-syntax-backward " "))
    (delete-region old-point (point))))

(defun my-kill-entire-line (&optional arg)
  (interactive "p")
  (let ((here (point)))
    (beginning-of-line)
    (kill-line arg)
    (goto-char here)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; opening new lines in various ways

(defun my-newline (&optional arg)
  "Call `newline' and autoindent according to the active mode."
  (interactive "p")
  (newline arg)
  (indent-according-to-mode)
  ;; mode-specific processing
  (cond
   ;; ((eq major-mode 'emacs-lisp-mode)) move my-emacs-lisp-open-line here
   ((eq major-mode 'php-mode)
    (cond
     ((and (nth 4 (syntax-ppss))
           (save-excursion
             (forward-line -1)
             (back-to-indentation)
             ;; TODO: simplify
             (or (looking-at "\\*")
                 (and (looking-at "/\\*\\*")
                      (progn
                        (forward-line)
                        (back-to-indentation)
                        (not (looking-at "*/")))))))
      (insert "* ")
      (indent-according-to-mode))))))


(defun my-open-line (&optional arg)
  "If point is before the beginning of \"code\", open new line,
keep the cursor at the current line and autoindent.

If point is in the middle of the line, create a blank line under
current line, move cursor to this new line and autoindent.

With raw prefix \\[universal-argument] insert newline at point
and indent next line according to mode."
  (interactive "P")
  (if (or (<= (point) (save-excursion
                        (my--back-to-indentation)
                        (point)))
          (equal arg '(4)))
      (progn
        (save-excursion
          (my-newline (if (equal arg '(4)) 1 arg)))
        (indent-according-to-mode))
    (end-of-line)
    (open-line (prefix-numeric-value arg))
    (forward-line 1)
    (indent-according-to-mode)))

(defun forward-line-and-indent (arg)
  "Move point ARG lines forward and autoindent."
  (interactive "p")
  (forward-line arg)
  (indent-according-to-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; navigation functions by "units"

(defun forward-paragraph-select ()
  "Set the active region from point to end of current paragraph"
  (interactive)
  (set-mark (point))
  (forward-paragraph))

(defun backward-paragraph-select ()
  "Set the active region from point to beginning of current paragraph"
  (interactive)
  (set-mark (point))
  (backward-paragraph))

(defun beginning-of-region ()
  "Move cursor to the beginning of active region"
  (interactive)
  (goto-char (region-beginning)))

(defun end-of-region ()
  "Move cursor to the end of active region"
  (interactive)
  (goto-char (region-end)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evaluate sexps/macros

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun expand-macro-and-replace ()
  "Replace the preceding sexp with its macroexpand."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (macroexpand (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; navigate to beg/end of current line, considering indent and
;; comments

(defun my--point-in-comment ()
  "Determine if the point is inside a comment"
  (or (sp-point-in-comment)
      ;; TODO: add this into SP?
      (-when-let (s (car (syntax-after (point))))
        (or (/= 0 (logand (lsh 1 16) s))
            (/= 0 (logand (lsh 1 17) s))
            (/= 0 (logand (lsh 1 18) s))
            (/= 0 (logand (lsh 1 19) s))))))

(defun my--back-to-indentation ()
  "Move to indentation respecting `visual-line-mode'."
  (if visual-line-mode
      (flet ((beginning-of-line (arg) (beginning-of-visual-line arg)))
        (back-to-indentation))
    (back-to-indentation)))

(defun my--move-beginning-of-line (&optional arg)
  "Move to beginning of line respecting `visual-line-mode'."
  (if visual-line-mode
      (beginning-of-visual-line arg)
    (move-beginning-of-line arg)))

(defun my--move-end-of-line (&optional arg)
  "Move to end of line respecting `visual-line-mode'."
  (if visual-line-mode
      (end-of-visual-line arg)
    (move-end-of-line arg)))

(defun my-back-to-indentation-or-beginning (&optional arg)
  "Jump back to indentation of the current line.  If already
there, jump to the beginning of current line.  If visual mode is
enabled, move according to the visual lines."
  (interactive "p")
  (cond
   ((and (functionp 'org-table-p)
         (org-table-p))
    (let ((eob (save-excursion
                 (re-search-backward "|")
                 (forward-char 1)
                 (skip-chars-forward " ")
                 (point))))
      (if (= (point) eob)
          (org-beginning-of-line)
        (goto-char eob))))
   ((eq major-mode 'dired-mode)
    (if (= (point) (save-excursion
                     (dired-move-to-filename)
                     (point)))
        (progn
          (move-beginning-of-line 1)
          (skip-syntax-forward " "))
      (dired-move-to-filename)))
   ((eq major-mode 'org-mode)
    (org-beginning-of-line))
   (t
    (if (or (/= arg 1)
            (= (point) (save-excursion
                         (my--back-to-indentation)
                         (point))))
        (progn
          (my--move-beginning-of-line arg)
          (when (/= arg 1)
            (my--back-to-indentation)))
      (my--back-to-indentation)))))

(defun my--cua-get-longest-line ()
  (-max (mapcar 'length
                (split-string
                 (buffer-substring-no-properties (cua--rectangle-top) (cua--rectangle-bot))
                 "\n"))))

(defun my-end-of-code-or-line (&optional arg)
  "Move to the end of code.  If already there, move to the end of line,
that is after the possible comment.  If at the end of line, move
to the end of code.

If the point is in org table, first go to the last non-whitespace
of the cell, then to the end of line.

If CUA rectangle is active, alternate between end of current
line, end of code, and end of the longest line in rectangle.

Example:
  (serious |code here)1 ;; useless commend2

In the example, | is the current point, 1 is the position of
point after one invocation of this funciton, 2 is position after
repeated invocation. On subsequent calls the point jumps between
1 and 2.

Comments are recognized in any mode that sets syntax-ppss
properly."
  (interactive "p")
  (cl-flet ((end-of-line-lov () (if visual-line-mode
                                    (end-of-visual-line arg)
                                  (move-end-of-line arg)))
            (beg-of-line-lov () (if visual-line-mode
                                    (beginning-of-visual-line arg)
                                  (move-beginning-of-line arg))))
    (cond
     ((and (functionp'org-table-p)
           (org-table-p))
      (let ((eoc (save-excursion
                   (re-search-forward "|")
                   (backward-char 1)
                   (skip-chars-backward " ")
                   (point))))
        (if (= (point) eoc)
            (end-of-line-lov)
          (goto-char eoc))))
     ((eq major-mode 'org-mode)
      (org-end-of-line))
     (t
      (let ((eoc (save-excursion
                   (end-of-line-lov)
                   (while (and (my--point-in-comment)
                               (not (bolp)))
                     (backward-char))
                   (skip-syntax-backward " ")
                   ;; if we skipped all the way to the beginning, that
                   ;; means there's only comment on this line, so this
                   ;; should just jump to the end.
                   (if (= (point) (save-excursion
                                    (beg-of-line-lov)
                                    (point)))
                       (progn (end-of-line-lov)
                              (point))
                     (point))))
            ;; end of rectangle in cua-rect mode
            (eor (when cua--rectangle (my--cua-get-longest-line))))
        ;; refactor this: make some "move actions" and call them in
        ;; order until point changes.
        (cond
         ((= (point) eoc)
          (if (and cua--rectangle
                   (/= (1+ (aref cua--rectangle 3)) eor))
              (cua-resize-rectangle-right (- eor (current-column) 1))
            (end-of-line-lov)))
         ((= (point) (progn (end-of-line-lov) (point)))
          (if (and cua--rectangle
                   (/= (1+ (aref cua--rectangle 3)) eor))
              (cua-resize-rectangle-right (- eor (current-column) 1))
            (goto-char eoc))) ;; asd
         (t (goto-char eoc))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stuff to do with upcasing and downcasing
;; used sparingly but when you need it it rocks!

(defun my-upcase-letter (&optional arg)
  (interactive "p")
  (upcase-region (point) (+ arg (point))))

(defun my-downcase-letter (&optional arg)
  (interactive "p")
  (downcase-region (point) (+ arg (point))))

(defun my-smart-downcase-word (&optional arg)
  "Downcase the following word in a context-aware way.

If called with raw prefix argument C-u, just `downcase-word'.

If a region is active, downcase the entire region.

If the following word is in an upper-case CamelCase, downcase just the
initial character.

Otherwise downcase the following word."
  (interactive "P")
  (cond
   ((equal arg '(4))
    (downcase-word 1))
   (t
    (setq arg (prefix-numeric-value arg))
    (cond
     ((region-active-p)
      (downcase-region (region-beginning) (region-end)))
     ;; handle camel-case
     ((save-excursion
        (my-forward-whitespace)
        (let* ((w (thing-at-point 'word))
               (case-fold-search nil))
          (string-match-p "^\\([A-Z]+[a-z]+\\)+" w)))
      (my-forward-whitespace)
      (my-downcase-letter 1)
      (forward-word))
     (t
      (downcase-word arg))))))

(defun my-smart-upcase-word (&optional arg)
  "Upcase the following word in a context-aware way.

If called with raw prefix argument C-u, just `upcase-word'.

If a region is active, upcase the entire region.

If the following word is camelCase, upcase just the initial character.

Otherwise upcase the following word."
  (interactive "P")
  (cond
   ((equal arg '(4))
    (upcase-word 1))
   (t
    (setq arg (prefix-numeric-value arg))
    (cond
     ((region-active-p)
      (upcase-region (region-beginning) (region-end)))
     ;; handle camel-case
     ((save-excursion
        (my-forward-whitespace)
        (let* ((w (thing-at-point 'word))
               (case-fold-search nil))
          (string-match-p "^[a-z]+\\([A-Z]+[a-z]+\\)+" w)))
      (my-forward-whitespace)
      (my-upcase-letter 1)
      (forward-word))
     (t
      (upcase-word arg))))))

(defun my-capitalize-word (&optional arg)
  "Capitalize the next ARG words and move over.
With negative ARG capitalize previous ARG words but not move the point.

Additionally, when looking at [ \\t]*$, capitalize backwards."
  (interactive "p")
  (when (and (looking-at-p "[ \t]*$") (> arg 0))
    (setq arg (- arg)))
  (let ((to-cap (delete-and-extract-region (progn
                                             (when (sp-point-in-symbol)
                                               (backward-word (- (cl-signum arg))))
                                             (point))
                                           (progn
                                             (forward-word arg)
                                             (point)))))
    (insert (s-titleize to-cap))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations related to lines

;; rework
(defun copy-line-with-offset (offset)
  "Save the line specified by offset (+1 = next, -1 = prev) to the kill ring,
move the current line down and yank"
  (kill-ring-save (line-beginning-position (+ offset 1))
                  (line-end-position (+ offset 1)))
  (let ((pos (point))
        (line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (beginning-of-line)
    (when (or (and (string-match "[:space:]" line)
                   (> offset 0))
              (< offset 0))
      (newline)
      (forward-line -1))
    (beginning-of-line)
    (insert (car kill-ring))
    (goto-char pos)))

(defun copy-previous-line ()
  (interactive)
  (copy-line-with-offset -1))

(defun copy-next-line ()
  (interactive)
  (copy-line-with-offset 1))

(defun kill-line-yank-newline ()
  (interactive)
  (let ((beg (line-beginning-position))
        (end (line-end-position))
        (name (buffer-name))
        (col (current-column)))
    (end-of-line)
    (newline)
    (beginning-of-line)
    (insert-buffer-substring name beg end)
    (move-to-column col t)
    (unless (eolp) (kill-sexp))))

(defun my-kill-line-yank-newline ()
  (interactive)
  (let ((beg (line-beginning-position))
        (end (line-end-position))
        (name (buffer-name))
        (col (current-column)))
    (end-of-line)
    (newline)
    (beginning-of-line)
    (insert-buffer-substring name beg end)
    (move-to-column col t)))

;; from https://github.com/skeeto/.emacs.d/blob/master/my-funcs.el
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (let ((col) (current-column) start end)
    (beginning-of-line) (setq start (point))
    (end-of-line) (forward-char) (setq end (point))
    (let ((line-text (delete-and-extract-region start end)))
      (forward-line n)
      (insert line-text)
      (forward-line -1)
      (forward-char col))))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(defun my-pull-word (&optional arg)
  "Pull the last word from the line above point to the beginning of this line.

With raw prefix \\[universal-argument] insert the word at point."
  (interactive "P")
  (-let* (((b . e) (save-excursion
                     (previous-line)
                     (end-of-line)
                     (cons (progn
                             (backward-word)
                             (point))
                           (line-end-position))))
          (text (prog1 (delete-and-extract-region b e)
                  (delete-trailing-whitespace
                   (line-beginning-position)
                   (line-end-position)))))
    (if arg (insert text)
      (save-excursion
        (back-to-indentation)
        (insert text " ")))))

(defun my-join-word (&optional arg)
  "Pull the first word on the following line and put it at the ond of current line.

With raw prefix \\[universal-argument] insert the word at point."
  (interactive "P")
  (-let* (((b . e) (save-excursion
                     (next-line)
                     (back-to-indentation)
                     (forward-word)
                     (bounds-of-thing-at-point 'word)))
          (e (save-excursion
               (goto-char e)
               (skip-syntax-forward ".")
               (point)))
          (text (save-excursion
                  (prog1 (delete-and-extract-region b e)
                    (goto-char b)
                    (delete-region
                     (point)
                     (progn
                       (skip-chars-forward " ")
                       (point)))
                    (indent-according-to-mode)))))
    (if arg (insert text)
      (end-of-line)
      (insert " " text))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert the text normally but keep the point fixed
;; useful to prepend text in e.g. `haskell-mode'

(defvar my-insert-no-move-overlay nil)
(make-variable-buffer-local 'my-insert-no-move-overlay)

(defvar my-insert-no-move-keymap (make-sparse-keymap))
(define-key my-insert-no-move-keymap (kbd "<backspace>") 'my-insert-no-move-delete-backward)
(define-key my-insert-no-move-keymap [remap self-insert-command] 'my-insert-no-move-self-insert-command)
(define-key my-insert-no-move-keymap (kbd "C-g") 'my-insert-no-move-cancel)

(defun my-insert-no-move ()
  (interactive)
  (when my-insert-no-move-overlay
    (my-insert-no-move-cancel))
  (setq my-insert-no-move-overlay (make-overlay (point) (point) nil nil t))
  (overlay-put my-insert-no-move-overlay 'keymap my-insert-no-move-keymap))

(defun my-insert-no-move-delete-backward (&optional arg)
  (interactive "p")
  (let ((s (overlay-start my-insert-no-move-overlay))
        (e (overlay-end my-insert-no-move-overlay)))
    (if (/= (point) s)
        (backward-delete-char arg)
      (goto-char e)
      (backward-delete-char arg)
      (goto-char s))))

(defun my-insert-no-move-self-insert-command (&optional arg)
  (interactive "p")
  (goto-char (overlay-end my-insert-no-move-overlay))
  (self-insert-command arg)
  (goto-char (overlay-start my-insert-no-move-overlay)))

(defun my-insert-no-move-cancel (&optional arg)
  (interactive "p")
  (delete-overlay my-insert-no-move-overlay)
  (setq my-insert-no-move-overlay nil))

;; convert dashed->camelcase->upper camelcase->underscore
(defvar my-change-identifier-style-last 's-dashed-words
  "Last transformer used to change identifier style.")

(defun my-change-identifier-style (beg end)
  "Change identifier in region or under point.

Cycle styles: dashed, lower camel case, upper camel case, snakecase."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (-let (((b . e) (bounds-of-thing-at-point 'symbol))) (list b e))))
  (unless (eq last-command 'my-change-identifier-style)
    (setq my-change-identifier-style-last 's-dashed-words))
  (cond
   ((eq my-change-identifier-style-last 's-dashed-words)
    (setq my-change-identifier-style-last 's-lower-camel-case))
   ((eq my-change-identifier-style-last 's-lower-camel-case)
    (setq my-change-identifier-style-last 's-upper-camel-case))
   ((eq my-change-identifier-style-last 's-upper-camel-case)
    (setq my-change-identifier-style-last 's-snake-case))
   ((eq my-change-identifier-style-last 's-snake-case)
    (setq my-change-identifier-style-last 's-dashed-words)))
  (let ((identifier (delete-and-extract-region beg end)))
    (insert (funcall my-change-identifier-style-last identifier))))
