;; insert a footnote to the text. The footnote is in a format
;; [i] Text of the footnote...
;; It will search for the first [i] in the text body and place \footnote{...} there
(setq last-kbd-macro
   [?\C-\M-f ?\C-f ?\C-  ?\M-e ?\M-w ?\M-` ?\C-a ?\C-s ?\C-w ?\C-w ?\C-r ?\C-r ?\C-\] delete ?\M-x ?f ?n ?o ?t ?e ?\] return])

(defun fnote ()
  (interactive)
  (insert "\\footnote{")
  (yank)
  (insert "}"))

(setq last-kbd-macro
   [?\C-\M-f ?\C-f ?\C-  ?\M-e ?\M-w ?\M-` ?\C-a ?\C-s ?\C-\M-w ?\C-\M-w ?\C-r ?\C-r return ?\C-\] delete ?\M-x ?f ?n ?o ?t ?e return])

;; swap between two last used buffers
(fset 'swap-buffer-to-last-used
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 98 return] 0 "%d")) arg)))
(bind-key "C-x C-a" nil emacs-lisp-mode-map)
(bind-key "C-x C-a" 'swap-buffer-to-last-used)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ALIASES

(defalias 'qrr 'query-replace-regexp)
(defalias 'rs 'replace-string)
