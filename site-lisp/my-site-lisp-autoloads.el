;;; my-site-lisp-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "my-advices" "my-advices.el" (0 0 0 0))
;;; Generated autoloads from my-advices.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-advices" '("my-tabify-use-buffer")))

;;;***

;;;### (autoloads nil "my-bootstrap" "my-bootstrap.el" (0 0 0 0))
;;; Generated autoloads from my-bootstrap.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-bootstrap" '("my-")))

;;;***

;;;### (autoloads nil "my-defuns" "my-defuns.el" (0 0 0 0))
;;; Generated autoloads from my-defuns.el

(autoload 'my-lorem "my-defuns" "\
Insert lorem ipsum text.

\(fn)" t nil)

(autoload 'shrug "my-defuns" "\
Insert shrug: ¯\\_(ツ)_/¯

\(fn)" t nil)

(autoload 'my-push-mark-no-activate "my-defuns" "\
Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled

\(fn)" t nil)

(autoload 'my-jump-to-mark "my-defuns" "\
Jump to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument.

\(fn)" t nil)

(autoload 'my-exchange-point-and-mark-no-activate "my-defuns" "\
Identical to \\[exchange-point-and-mark] but will not activate the region.

\(fn)" t nil)

(autoload 'my-find-url "my-defuns" "\
Download URL and insert into current buffer at point.

\(fn URL)" t nil)

(autoload 'my-unfill-paragraph "my-defuns" "\
Take a multi-line paragrap and make it into a single line of text.
This is the opposite of fill-paragraph.

\(fn)" t nil)

(autoload 'my-remove-dos-eol "my-defuns" "\
Do not show ^M in files containing mixed UNIX and DOS line endings.

\(fn)" t nil)

(autoload 'my-format-synonyms-from-wiki "my-defuns" "\


\(fn)" t nil)

(autoload 'my-sprunge "my-defuns" "\
Paste TEXT to sprunge.us.

With non-nil prefix argument, ask for LANGUAGE.

\(fn TEXT &optional LANGUAGE)" t nil)

(autoload 'my-update-mpd "my-defuns" "\
Update the mpd database using the common parent of current dired listing.

\(fn)" t nil)

(autoload 'my-shuffle-lines "my-defuns" "\
Shuffle lines in the active region.

\(fn BEG END)" t nil)

(autoload 'my-shuffle-words "my-defuns" "\
Shuffle words in the active region.

\(fn BEG END)" t nil)

(autoload 'my-toggle-buffer-input-methods "my-defuns" "\
Toggle the input methods listed in `my-buffer-input-methods'.

\(fn)" t nil)

(autoload 'my-switch-buffer-LRU "my-defuns" "\
Switch to LRU buffer.

\(fn)" t nil)

(autoload 'my-occur-dwim "my-defuns" "\
Call `occur' with a sane default.

\(fn)" t nil)

(autoload 'my-insert-date-iso "my-defuns" "\
Insert timestamp.

\(fn DATE)" t nil)

(autoload 'my-visit-init-file "my-defuns" "\
Visit init file.

\(fn)" t nil)

(autoload 'my-find-file-in-home "my-defuns" "\
Find file in the home directory

\(fn)" t nil)

(autoload 'my-md5-file "my-defuns" "\
Open FILENAME, load it into a buffer and generate the md5 of its contents

\(fn FILENAME)" t nil)

(autoload 'my-dired-show-duplicates "my-defuns" "\


\(fn LIST-A LIST-B)" t nil)

(autoload 'my-sync-rsync-local-to-remote "my-defuns" "\
Sync the current file/directory with `my-rsync-remote'.

With prefix argument \\[universal-argument], sync the entire project.

\(fn &optional ENTIRE-PROJECT)" t nil)

(autoload 'my-sync-rsync-remote-to-local "my-defuns" "\
Sync remote version of this file to the local copy.

The remote is determined by `my-rsync-remote'.

\(fn)" t nil)

(autoload 'my-run-haddock "my-defuns" "\
Run haddock on current project.

\(fn)" t nil)

(autoload 'my-find-dependency "my-defuns" "\
Open dependency installed with straight.el

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-defuns" '(#("my-" 0 3 (fontified nil face font-lock-function-name-face)))))

;;;***

;;;### (autoloads nil "my-defuns-buffer" "my-defuns-buffer.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from my-defuns-buffer.el

(autoload 'create-scratch-buffer "my-defuns-buffer" "\
Create a new scratch buffer to work in. (could be *scratch* - *scratchX*)

\(fn MODE)" t nil)

(autoload 'create-scratch-buffer-current-mode "my-defuns-buffer" "\
Create a new scratch buffer to work in and set its mode to current `major-mode'.

\(fn)" t nil)

(autoload 'untabify-buffer "my-defuns-buffer" "\


\(fn)" t nil)

(autoload 'indent-buffer "my-defuns-buffer" "\


\(fn)" t nil)

(autoload 'indent-defun "my-defuns-buffer" "\
Indent the current defun.

\(fn)" t nil)

(autoload 'my-kill-this-buffer "my-defuns-buffer" "\
Kill the current buffer.
Warn me if I want to kill a scratch buffer.

\(fn)" t nil)

(autoload 'cleanup-buffer-safe "my-defuns-buffer" "\
Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad.

\(fn)" t nil)

(autoload 'cleanup-buffer "my-defuns-buffer" "\
Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save.

\(fn)" t nil)

(autoload 'my-create-directory-on-save "my-defuns-buffer" "\


\(fn &optional _)" nil nil)

(autoload 'my-kill-pp-eval-expression-window "my-defuns-buffer" "\


\(fn)" t nil)

(autoload 'my-add-font-lock-face "my-defuns-buffer" "\
Add face to region.

\(fn BEG END FACE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-defuns-buffer" '("my-")))

;;;***

;;;### (autoloads nil "my-defuns-edit" "my-defuns-edit.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from my-defuns-edit.el

(autoload 'my-forward-whitespace "my-defuns-edit" "\
Like `forward-whitespace' but do not skip over a word if there
is no preceding whitespace.

 |  foo  bar =>   |foo  bar
   |foo  bar =>   |foo  bar ;; not foo  |bar

\(fn)" t nil)

(autoload 'my-kill-whitespace "my-defuns-edit" "\
Kill all the whitespace characters backwards until hitting
non-whitespace character.  With prefix argument, kill in the
forward direction.

\(fn &optional FORWARD)" t nil)

(autoload 'my-kill-entire-line "my-defuns-edit" "\
Kill the entire line or ARG lines the point is on.

\(fn &optional ARG)" t nil)

(autoload 'my-kill-region-or-word "my-defuns-edit" "\
Kill active region or one word backward.

\(fn &optional ARG)" t nil)

(autoload 'my-newline "my-defuns-edit" "\
Call `newline' and run `my-newline-hook'.

\(fn &optional ARG)" t nil)

(autoload 'my-open-line "my-defuns-edit" "\
If point is before the beginning of \"code\", open new line,
keep the cursor at the current line and autoindent.

If point is in the middle of the line, create a blank line under
current line, move cursor to this new line and autoindent.

With raw prefix \\[universal-argument] insert newline at point
and indent next line according to mode.

\(fn &optional ARG)" t nil)

(autoload 'my-forward-line-and-indent "my-defuns-edit" "\
Move point ARG lines forward and autoindent.

\(fn &optional ARG)" t nil)

(autoload 'my-eval-and-replace "my-defuns-edit" "\
Replace the preceding sexp with its value.

\(fn)" t nil)

(autoload 'my-back-to-indentation-or-beginning "my-defuns-edit" "\
Jump back to indentation of the current line.

If already there, jump to the beginning of current line.  If
visual mode is enabled, move according to the visual lines.

If the point is inside a comment and a jump to before its opening
delimiter would occur, jump to beginning of comment text
instead.

\(fn &optional ARG)" t nil)

(autoload 'my-end-of-code-or-line "my-defuns-edit" "\
Move to the end of code.  If already there, move to the end of line,
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
properly.

\(fn &optional ARG)" t nil)

(autoload 'my-upcase-letter "my-defuns-edit" "\


\(fn &optional ARG)" t nil)

(autoload 'my-downcase-letter "my-defuns-edit" "\


\(fn &optional ARG)" t nil)

(autoload 'my-smart-downcase-word "my-defuns-edit" "\
Downcase the following word in a context-aware way.

If called with raw prefix argument C-u, just `downcase-word'.

If a region is active, downcase the entire region.

If the following word is in an upper-case CamelCase, downcase just the
initial character.

Otherwise downcase the following word.

\(fn &optional ARG)" t nil)

(autoload 'my-smart-upcase-word "my-defuns-edit" "\
Upcase the following word in a context-aware way.

If called with raw prefix argument C-u, just `upcase-word'.

If a region is active, upcase the entire region.

If the following word is camelCase, upcase just the initial character.

Otherwise upcase the following word.

\(fn &optional ARG)" t nil)

(autoload 'my-capitalize-word "my-defuns-edit" "\
Capitalize the next ARG words and move over.
With negative ARG capitalize previous ARG words but not move the point.

Additionally, when looking at [ \\t]*$, capitalize backwards.

\(fn &optional ARG)" t nil)

(autoload 'my-move-line "my-defuns-edit" "\
Move the current line up or down by N lines.

\(fn N)" t nil)

(autoload 'my-move-line-up "my-defuns-edit" "\
Move the current line up by N lines.

\(fn N)" t nil)

(autoload 'my-move-line-down "my-defuns-edit" "\
Move the current line down by N lines.

\(fn N)" t nil)

(autoload 'my-pull-word "my-defuns-edit" "\
Pull the last word from the line above point to the beginning of this line.

With raw prefix \\[universal-argument] insert the word at point.

\(fn &optional ARG)" t nil)

(autoload 'my-join-word "my-defuns-edit" "\
Pull the first word on the following line and put it at the ond of current line.

With raw prefix \\[universal-argument] insert the word at point.

\(fn &optional ARG)" t nil)

(autoload 'my-join-lines "my-defuns-edit" "\
Pull the next line up and place cursor at its beginning.

\(fn)" t nil)

(autoload 'my-change-identifier-style "my-defuns-edit" "\
Change identifier in region or under point.

Cycle styles: dashed, lower camel case, upper camel case, snakecase.

\(fn BEG END)" t nil)

(autoload 'my-forward-word "my-defuns-edit" "\
Call `forward-word' or `subword-forward'.

Repeat ARG times.

\(fn &optional ARG)" t nil)

(autoload 'my-backward-word "my-defuns-edit" "\
Call `backward-word' or `subword-backward'.

Repeat ARG times.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-defuns-edit" '("my-")))

;;;***

;;;### (autoloads nil "my-macros" "my-macros.el" (0 0 0 0))
;;; Generated autoloads from my-macros.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-macros" '("my-")))

;;;***

;;;### (autoloads nil "my-macros-tangled" "my-macros-tangled.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from my-macros-tangled.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my-macros-tangled" '("my-minibuffer-with-hook")))

;;;***

;;;### (autoloads nil nil ("my-redef.el" "vendor.el") (0 0 0 0))

;;;***

(provide 'my-site-lisp-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; my-site-lisp-autoloads.el ends here
