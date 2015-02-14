;; from simple.el
(defadvice kill-line (before kill-line-autoreindent activate)
  (when (and (eolp) (not (bolp)))
    (save-excursion
      (forward-char 1)
      (just-one-space 1))))

(defadvice kill-visual-line (before kill-line-autoreindent activate)
  (when (and (eolp) (not (bolp)))
    (save-excursion
      (forward-char 1)
      (just-one-space 1))))

(defadvice transpose-words (before fix-eob activate)
  (when (let ((str (buffer-substring-no-properties
                    (point)
                    (save-excursion
                      (forward-word)
                      (point)))))
          (not (string-match-p "\\sw" str)))
    (backward-word)))

(defadvice subword-transpose (before fix-eob activate)
  (when (let ((str (buffer-substring-no-properties
                    (point)
                    (save-excursion
                      (subword-forward)
                      (point)))))
          (not (string-match-p "\\sw" str)))
    (subword-backward)))
