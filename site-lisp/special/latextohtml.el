;; list of enviroments:
;; simpleScreenplay - tt, center, 0.5in margin (signs on buildings, landmarks...)
;; letter           - it, 0.3in margin (letters, recordings, broadcasts, thoughts of another person...)
;; diary            - it (long portions of text by another person, diary, memories...)
;; radio            - it (radio)
;; terminal         - tt, 0.3in margin (terminal notes or notes on paper, portions of books etc.)
;; song             - it, 0.3in margin, note symbol - #9834 (songs)
;; center           - make text centered
;;
;; commands
;;
;; vspace          - adds a bit of a vspace (about one empty par)
;; emph            - emphatic text. Should be enviroment-sensitive
;; bf              - bold face.
;; Scene           - scene separator
;; "<              - opening quote - &laquo;
;; ">              - closing quote - &raquo;
;; dots            - ellipsis - &hellip;
;; ---             - &mdash;
;; \%              - %
;; footnote        - footnotes, this will need some special treatment
;; levelup         - level up notification. needs to add some text:
;;                   Nota: Nuovo livello.
;;
;;                   Nuovo vantaggio: #1
;; normalchapter   - subtext, name of the chapter

;; <SUP><A HREF="#note_to_$N" NAME="#note_from_$N">$N</A></SUP>
;; e segnati il contenuto della nota. A fine capitolo cicla le note e metti
;; <P><SUP><A HREF="#note_from_$N" NAME="#note_to_$N">$N</A></SUP> Testo della nota</P>

(defun gen-footnotes-internal (n)
  "n - currently processed footnote"
  (when (search-forward "\\footnote" nil t)
    (let* ((x (point)) (fn (number-to-string n)))
      (forward-sexp)
      (let* ((y (point)) (str (buffer-substring-no-properties (+ x 1) (- y 1))))
        (progn (message str)
               (delete-region (- x 9) y)
               (insert (concat
                        "<sup><a href=\"#note_to_"
                        fn
                        "\" name=\"note_from_"
                        fn
                        "\" >["
                        fn
                        "]</a></sup>"))
               (end-of-buffer)
               (insert (concat "

<p><sup><a href=\"#note_from_"
                               fn
                               "\" name=\"note_to_"
                               fn
                               "\" >["
                               fn
                               "]</a></sup> "
                               str
                               "</p>"))
                                        ;(pop-global-mark)
               (goto-char (point-min))
               (gen-footnotes-internal (+ n 1)))))))

(defun gen-footnotes ()
  (interactive)
  (goto-char (point-min))
  (gen-footnotes-internal 1))

(defun print-frontmatter ()
  (goto-char (point-min))
  (insert "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" dir=\"ltr\" lang=\"it\" xml:lang=\"it\">
<head>
<meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\" />
<meta http-equiv=\"content-style-type\" content=\"text/css\" />
<meta http-equiv=\"content-language\" content=\"it\" />

<link rel=\"stylesheet\" href=\"style.css\" />
</head>
<body>"))

(defun print-backmatter ()
  (end-of-buffer)
  (insert "

</body>
</html>"))


(defun gen-html ()
  (interactive)
  (let* ((bn (buffer-name)) (bfn (buffer-file-name)) (nbfn (when (string-match ".tex" bfn)
                                                             (message (replace-match ".html" nil nil bfn)))))
    (progn (find-file nbfn)
           (erase-buffer)
           (insert-buffer-substring bn)))
  (end-of-buffer)
  (insert "<hr>
")
  (goto-char (point-min))
  (gen-footnotes)
  (goto-char (point-min))
  (replace-regexp "\"<" "&laquo;")
  (goto-char (point-min))
  (replace-regexp "\">" "&raquo;")
  (goto-char (point-min))
  (replace-regexp "\\\\dk{``A}" "&ldquo;A")
  (goto-char (point-min))
  (replace-regexp "\\\\dk{\"<A}" "&ldquo;A")
  (goto-char (point-min))
  (replace-regexp "``" "&ldquo;")
  (goto-char (point-min))
  (replace-regexp "''" "&rdquo;")
  (goto-char (point-min))
  (replace-regexp "\\\\textasciitilde{}" "~")
  (goto-char (point-min))
  (replace-regexp "\\\\textgreater{}" "&gt;")
  (goto-char (point-min))
  (replace-regexp "\\\\-" "")
  (goto-char (point-min))
  (replace-regexp "\\\\fussy " "")
  (goto-char (point-min))
  (replace-regexp "\\\\sloppy " "")
  (goto-char (point-min))
  (replace-regexp "\\\\newline" "<br>")
  (goto-char (point-min))
  (replace-regexp "\\\\vspace{\\(.*?\\)}" "<div style=\"height: \\1\" ></div>")
  (goto-char (point-min))
  (replace-regexp "\\\\emph{\\(.*?\\)}" "<em>\\1</em>")
  (goto-char (point-min))
  (replace-regexp "{\\\\bf \\(.*?\\)}" "<strong>\\1</strong>")
  (goto-char (point-min))
  (replace-regexp "\\\\Scene" "<div class=\"scene\" ></div>")
  (goto-char (point-min))
  (replace-regexp "\\\\dots" "&hellip;")
  (goto-char (point-min))
  (replace-regexp "---" " &mdash; ")
  (goto-char (point-min))
  (replace-regexp "\\\\%" "%")
  (goto-char (point-min))
  (replace-regexp "\\\\#" "#")
  (goto-char (point-min)) ; add newlines around begin
  (replace-regexp "\\(\\\\begin{.*?}\\)" "
\\1
")
  (goto-char (point-min)) ; add newlines around end
  (replace-regexp "\\(\\\\end{.*?}\\)" "
\\1
")
  (goto-char (point-min)) ; remove excessive newlines
  (replace-regexp "


+" "

")
  (goto-char (point-min)) ; add <p> tags
  (replace-regexp "\\(^[^<\\
].*?\\)

" "<p>\\1</p>

")
  (goto-char (point-min)) ; change begin .letter and .terminal and .song into <blockquote>
  (replace-regexp "\\\\begin{\\(\\(letter\\|terminal\\|song\\)\\)}" "<blockquote class=\"\\1\" >")
  (goto-char (point-min)) ; change end .letter and .terminal .song into </blockquote>
  (replace-regexp "\\\\end{\\(\\(letter\\|terminal\\|song\\)\\)}" "</blockquote>")
  (goto-char (point-min)) ; memoryOrb style
  (replace-regexp "\\\\begin{memoryOrb}" "<div class=\"memoryOrb\" >
<div class=\"memoryOrbOpen\" >(* * *)</div>")
  (goto-char (point-min))
  (replace-regexp "\\\\end{memoryOrb}" "<div class=\"memoryOrbClose\" >(* * *)</div>
</div>")
  (goto-char (point-min))
  (replace-regexp "<blockquote class=\"song\" >" "<blockquote class=\"song\" >
<div class=\"songnote\" >&#9834;</div>")
  (goto-char (point-min)) ; change begin to <div> element
  (replace-regexp "\\\\begin{\\(.*?\\)}" "<div class=\"\\1\" >")
  (goto-char (point-min)) ; change end to </div> element
  (replace-regexp "\\\\end{\\(.*?\\)}" "</div>")
  (goto-char (point-min)) ; fix special case where line starts with <strong|em> and doesn\t have <p>
  (replace-regexp "^\\(<\\(strong\\|em\\)>.*?\\)

" "<p>\\1</p>

")
  (goto-char (point-min)) ; fix <p>&gt; ... </p>
  (replace-regexp "<p>\\(&gt;.*?\\)</p>" "\\1")
  (goto-char (point-min))
  (replace-regexp "\\\\normalchapter\\[\\(.*?\\)\\]{\\(.*?\\)}" "<h1>\\2</h1>

<p style=\"text-align:center;\" ><em>\\1</em></p>")
  (goto-char (point-min))
  (replace-regexp "\\\\normalchapters\\[\\(.*?\\)\\]{\\(.*?\\)}{.*?}" "<h1>\\2</h1>

<p style=\"text-align:center;\" ><em>\\1</em></p>")
  (replace-regexp "\\\\levelup{\\(.*?\\)}" "<div class=\"levelup\" >
<p>Nota: nuovo livello.</p>

<p>Nuovo vantaggio: \\1</p>
</div>

")
  (print-frontmatter)
  (print-backmatter)
  (save-buffer))

;; (loop for i in (make-list 12)
;;       do
;;       (find-file (concat "d:/download/fic/foeitalian/chapter" (number-to-string i) ".tex")))

;; (loop for i in (make-list 12)
;;       do
;;       (progn (switch-to-buffer (concat "chapter" (number-to-string i) ".tex"))
;;              (gen-html)
;;              ))

                                        ; &bdquo; -- open quote „
                                        ; &ldquo; -- "close quote" “
                                        ; &#39; -- '
                                        ; &nbsp; -- " " < space
                                        ; &hellip; -- …

(defun fix-basic ()
  (interactive)
  (my-fix-reset-after-each
    (replace-regexp "…" "\\\\dots ")
    (replace-regexp "\\.\\.\\." "\\\\dots ")
    (replace-regexp "\\\\dots[[:space:]]+)" "\\\\dots)")
    (replace-regexp "[[:space:]][[:space:]]+" " ")
    (replace-regexp "[[:space:]]+--[[:space:]]+" "---")
    (replace-regexp "[[:space:]]+–[[:space:]]+" "---")
    (replace-regexp "[[:space:]]+-[[:space:]]+" "---")
    (replace-regexp "“" "``")
    (replace-regexp "„" "``")
    (replace-regexp "”" "''")
    (replace-regexp "‘" "`")
    (replace-regexp "‚" "`")
    (replace-regexp "’" "'")
    (replace-regexp "~" "\\\\textasciitilde{}")
    (replace-regexp "\\\\dots[[:space:]]+''" "\\\\dots''")
    (replace-regexp "\\([^\\]\\)%" "\\1\\\\%")
    (replace-regexp "^[[:space:]]" "")
    (replace-string "}\\emph{" "")
    (replace-string "\\emph{}" "")
    (query-replace-regexp "\\\\emph{ }" "")
    (replace-string "'' ``" "''\n\n``")
    (replace-regexp "


+" "

")
    (replace-regexp "``-\\([^-]\\)" "``---\\1")
    (replace-regexp "``---\\([^}]\\)" "\\\\dk{``---}\\1")
    (replace-regexp "\\([^-]\\)-''" "\\1---''") ; fix the hyphen at the end of quotation
    (query-replace-regexp "---''\\([^}]\\)" "\\\\dk{---''}\\1")
    (replace-regexp "\\([^-]\\)-}" "\\1---}") ; fix the hyphen before emph end
    (query-replace-regexp "- " "---")
    (replace-regexp "!!+" "!")
    (replace-regexp "\\?\\?+" "?")
    (replace-regexp "\\([^-]\\)-
" "\\1---
")
    (replace-regexp "``A\\([^}]\\)" "\\\\dk{``A}\\1")
    (query-replace-regexp " }\\(\\w\\)" "} \\1")
    (query-replace "\\emph{ " " \\emph{")
    ))


(defun fix-quotes () ; and some less trivial quotation stuff
  (interactive)
  ;; replace 'em with $$EM$$, it's too common to skip all the time,
  ;; then we replace it back
  (my-fix-reset-after-each
    (replace-string "'em" "$$EM$$")
    (query-replace-regexp "[[:space:]]'\\([[:word:]]\\)" " `\\1")
    (query-replace-regexp "``'\\([[:word:]]\\)" "```\\1")
    (query-replace-regexp "^'" "`")
    (replace-regexp ":[ ]``" ", ``")
    (query-replace-regexp "\\([[:word:]]\\)-[ ]" "\\1\\\\dots ")
    (replace-regexp "\\([[:word:]]\\)[ ]\\\\dots" "\\1\\\\dots")
    (replace-string "$$EM$$" "'em")
    (query-replace-regexp "}\\." ".}")
    (query-replace-regexp "}," ",}")
    (query-replace-regexp "}!" "!}")
    (query-replace-regexp "}\\?" "?}")
    (query-replace-regexp "!," "!")
    (query-replace-regexp "\\?," "?")
    (query-replace-regexp "\\.," ",")
    ))

(defun fix-quotes-italian (arg)
  (interactive "P")
  (my-fix-reset-after-each
    (unless arg
      (replace-regexp "``" "\"<")
      (replace-regexp "''" "\">"))
    (replace-regexp "“" "\"<")
    (replace-regexp "”" "\">")
    (replace-regexp "«" "\"<")
    (replace-regexp "»" "\">")
    ))

(defun fix-italian ()
  (interactive)
  (my-fix-reset-after-each
    (replace-string "E'" "È")
    (replace-string "un pò" "un po'")
    (replace-string "perchè" "perché"))
  )

(defun fix-quot-to-quotes (beg end)
  (interactive "r")
  (goto-char beg)
  (delete-region beg (+ beg 6))
  (insert "``")
  (goto-char (- end 4 6))
  (delete-region (- end 4 6) (- end 4))
  (insert "''")
  (goto-char (- end 8)))

;; (goto-char (point-min))
;; (replace-regexp "\\*\\*\\* \\*\\*\\* \\*\\*\\*" "\\\\Scene")

(defun fix-html (italic-class bold-class)
  (interactive (list (read-from-minibuffer "Italics class name: ")
                     (when current-prefix-arg
                       (read-from-minibuffer "Bold class name: "))))
  (my-fix-reset-after-each
    (replace-regexp "<p.*?>" "")
    (replace-regexp "</p>" "

")
    (replace-regexp
     (concat "<span class=\"\\(?:c[0-9]+ \\)*?" italic-class "\\(?: c[0-9]+\\)*?\">\\(.*?\\)</span>")
     "\\\\emph{\\1}")
    (replace-regexp
     (concat "<span class=\"\\(?:c[0-9]+ \\)*?" bold-class "\\(?: c[0-9]+\\)*?\">\\(.*?\\)</span>")
     "\\\\strong{\\1}")
    (replace-regexp " " " ")
    (replace-regexp "<span.*?>" "")
    (replace-regexp "</span>" "")
    (replace-regexp "&ndash;" "–")
    (replace-regexp "&bdquo;" "„")
    (replace-regexp "&ldquo;" "“")
    (replace-regexp "&rdquo;" "”")
    (replace-regexp "&rsquo;" "’")
    (replace-regexp "&laquo;" "«")
    (replace-regexp "&raquo;" "»")
    (replace-regexp "&#39;" "'")
    (replace-regexp "&nbsp;" " ")
    (replace-regexp "&hellip;" "…")
    (replace-regexp "&egrave;" "è")
    (replace-regexp "&agrave;" "à")
    (replace-regexp "&ugrave;" "ù")
    (replace-regexp "&ograve;" "ò")
    (replace-regexp "&igrave;" "ì")
    (replace-regexp "&eacute;" "é")
    (replace-regexp "&Aacute;" "Á")
    (replace-regexp "&uuml;" "̇ü")
    (replace-regexp "&ouml;" "ö")
    (replace-regexp "&auml;" "ä")
    (replace-regexp "&deg;" "$^\\circ$")
    (replace-regexp "<.*?>" "")
    ))

(defun fix-html-fimfic ()
  (interactive)
  (my-fix-reset-after-each
    (replace-regexp "</i><i>" "")
    (replace-regexp "<i>\\(.*?\\)</i>" "\\\\emph{\\1}")
    (replace-regexp "<br />" "
")
    (replace-regexp "<hr />" "\\\\Scene")
    (replace-regexp " " " ")
    (replace-regexp "\\\\emph{ }" " ")
    (replace-regexp "\\\\emph{}" "")
    ))


(defun fix-prepare-diff ()
  (interactive)
  (my-fix-reset-after-each
    (replace-regexp "“" "\"")
    (replace-regexp "„" "\"")
    (replace-regexp "”" "\"")
    (replace-regexp "‘" "'")
    (replace-regexp "‚" "'")
    (replace-regexp "’" "'")
    (replace-regexp "«" "\"")
    (replace-regexp "»" "\"")
    (replace-regexp "…" "...")
    (replace-regexp "—" "--")
    (replace-string "* * *" "")
    (replace-string "*** *** ***" "")
    (replace-regexp "  +" " ")
    (replace-regexp " +$" "")
    (replace-string "... " "...")
    (replace-regexp "^ +$" "")
    (replace-regexp "


+" "

")
    ))
