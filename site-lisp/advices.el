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
