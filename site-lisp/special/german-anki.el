(defun my-extract-buffer-substring (end-pattern)
  "Extract buffer substring between current point and END-PATTERN.

END-PATTERN should be a regular expression delimiting the
substring.  The match is extracted up until the beginning of the
first match.

The point is moved to the end of the match."
  (save-match-data
    (buffer-substring-no-properties
     (point)
     (when (re-search-forward end-pattern nil t)
       (match-beginning 0)))))

(defun my-fix-german-anki-deck-get-plural ()
  (when (and (search-forward "German\n---" nil t)
             (search-forward "noun[" nil t))
    (search-forward "," nil t 2)
    (when (looking-at "gen2")
      (search-forward "," nil t))
    (my-extract-buffer-substring "[],]")))

(defun my-fix-german-anki-deck-get-gender ()
  (when (and (search-forward "German\n---" nil t)
             (search-forward "noun[" nil t))
    (my-extract-buffer-substring ",")))

(defun my-fix-german-anki-deck ()
  (let ((case-fold-search nil)
        (curbuf (current-buffer)))
    (my-with-every-line
     (when (and (looking-at "[A-Z]")
                (progn
                  (forward-word)
                  (looking-at "\t")))
       (save-window-excursion
         (call-interactively 'wd-show-translation)
         (with-current-buffer (get-buffer-create "*Wiktionary*")
           (let ((gender (save-excursion (my-fix-german-anki-deck-get-gender)))
                 (plural (save-excursion (my-fix-german-anki-deck-get-plural))))
             (with-current-buffer curbuf
               (beginning-of-line)
               (when gender
                 (insert (cond
                          ((equal gender "m") "der ")
                          ((equal gender "f") "die ")
                          ((equal gender "n") "das "))))
               (when plural
                 (search-forward "\t" nil t)
                 (backward-char)
                 (insert ", " plural))))))))))
