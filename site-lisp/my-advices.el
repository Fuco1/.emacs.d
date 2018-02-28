;;; my-advices.el --- Global advices

;;; Commentary:
;;; Code:

;; from simple.el
(defadvice kill-line (before kill-line-autoreindent activate)
  "Kill excess whitespace when joining lines.

If the next line is joined to the current line, kill the extra indent whitespace in front of the next line."
  (when (and (eolp) (not (bolp)))
    (save-excursion
      (forward-char 1)
      (just-one-space 1))))

(defadvice kill-visual-line (before kill-line-autoreindent activate)
  "Kill excess whitespace when joining lines.

If the next line is joined to the current line, kill the extra indent whitespace in front of the next line."
  (when (and (eolp) (not (bolp)))
    (save-excursion
      (forward-char 1)
      (just-one-space 1))))

(defadvice transpose-words (before fix-eob activate)
  "If at eob, transpose the two last words."
  (when (let ((str (buffer-substring-no-properties
                    (point)
                    (save-excursion
                      (forward-word)
                      (point)))))
          (not (string-match-p "\\sw" str)))
    (backward-word)))

(defadvice subword-transpose (before fix-eob activate)
  "If at eob, transpose the two last words."
  (when (let ((str (buffer-substring-no-properties
                    (point)
                    (save-excursion
                      (subword-forward)
                      (point)))))
          (not (string-match-p "\\sw" str)))
    (subword-backward)))

;; TODO: package this?
(defadvice basic-save-buffer-2 (around fix-unwritable-save-with-sudo activate)
  "When we save a buffer which is write-protected, try to sudo-save it.

When the buffer is write-protected it is usually opened in
read-only mode.  Use \\[read-only-mode] to toggle
`read-only-mode', make your changes and \\[save-buffer] to save.
Emacs will warn you that the buffer is write-protected and asks
you to confirm if you really want to save.  If you answer yes,
Emacs will use sudo tramp method to save the file and then
reverts it, making it read-only again.  The buffer stays
associated with the original non-sudo filename."
  (condition-case err
      (progn
        ad-do-it)
    (file-error
     (when (string-prefix-p
            "doing chmod: operation not permitted"
            (downcase (error-message-string err)))
       (let ((old-buffer-file-name buffer-file-name)
             (success nil))
         (unwind-protect
             (progn
               (setq buffer-file-name (concat "/sudo:localhost:" buffer-file-name))
               (save-buffer)
               (setq success t))
           (setq buffer-file-name old-buffer-file-name)
           (when success
             (revert-buffer t t))))))))

(defadvice insert-directory (around fix-collate activate)
  (let ((process-environment (cons
                              "LC_ALL=C"
                              process-environment)))
    ad-do-it))


(defun my-tabify-use-buffer (orig-fun start end &optional _arg)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end) nil)
                 (list (point-min) (point-max) nil)))
  (funcall orig-fun start end _arg))

(advice-add 'tabify :around #'my-tabify-use-buffer)

(provide 'my-advices)
;;; my-advices.el ends here
