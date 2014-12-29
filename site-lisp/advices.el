;; from simple.el
(defadvice kill-line (before kill-line-autoreindent activate)
  (if (member major-mode
              '(
                lisp-interaction-mode
                emacs-lisp-mode
                scheme-mode
                lisp-mode
                c-mode
                c++-mode
                latex-mode
                ))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

(defadvice kill-visual-line (before kill-line-autoreindent activate)
  (if (member major-mode
              '(
                lisp-interaction-mode
                emacs-lisp-mode
                scheme-mode
                lisp-mode
                c-mode
                c++-mode
                latex-mode
                ))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

(defadvice transpose-words (before fix-eob activate)
  (when (let ((str (buffer-substring-no-properties
                    (point)
                    (save-excursion
                      (forward-word)
                      (point)))))
          (not (string-match-p "\\sw" str)))
    (backward-word)))
