(when (eq system-type 'windows-nt)
  (custom-set-variables
   '(ispell-program-name "d:\\progs\\Aspell\\bin\\aspell.exe")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-math-list
   (quote
    (("C-k (" "llparenthesis" "" nil)
     ("C-k )" "rrparenthesis" "" nil)
     ("C-k k"
      (lambda nil
        (interactive)
        (insert "\\cata{}")
        (backward-char))
      "" nil)
     (79 "circ" "" nil)
     (61 "equiv" "" nil)
     ("C-k -"
      (lambda nil
        (interactive)
        (insert "\\bar{}")
        (backward-char))
      "" nil)
     ("<right>" "Rightarrow" "" nil)
     ("<left>" "Leftarrow" "" nil)
     ("<up>" "Leftrightarrow" "" nil)
     ("<f1>" "ldots" "" nil))))
 '(TeX-PDF-mode t)
 '(TeX-command-list
   (quote
    (("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "texexec --once --texutil %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "texexec %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file")
     ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command")
     ("Run Zathura" "zathura --fork -s -x \"emacsclient --no-wait +%%{line} %%{input}\" %s.pdf" TeX-run-command nil t :help "Run Zathura PDF viewer"))))
 '(TeX-output-view-style
   (quote
    (("^dvi$"
      ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$")
      "%(o?)dvips -t landscape %d -o && gv %f")
     ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f")
     ("^dvi$"
      ("^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "^landscape$")
      "%(o?)yap %dS -paper a4r -s 0 %d")
     ("^dvi$" "^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "%(o?)yap %dS -paper a4 %d")
     ("^dvi$"
      ("^\\(?:a5\\(?:comb\\|paper\\)\\)$" "^landscape$")
      "%(o?)yap %dS -paper a5r -s 0 %d")
     ("^dvi$" "^\\(?:a5\\(?:comb\\|paper\\)\\)$" "%(o?)yap %dS -paper a5 %d")
     ("^dvi$" "^b5paper$" "%(o?)yap %dS -paper b5 %d")
     ("^dvi$" "^letterpaper$" "%(o?)yap %dS -paper us %d")
     ("^dvi$" "^legalpaper$" "%(o?)yap %dS -paper legal %d")
     ("^dvi$" "^executivepaper$" "%(o?)yap %dS -paper 7.25x10.5in %d")
     ("^dvi$" "." "%(o?)yap %dS %d")
     ("^pdf$" "." "gsview32 -remote %s -raise %o %(outpage)")
     ("^html?$" "." "netscape %o"))))
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-view-program-list
   (quote
    (("View with Zathura"
      ("zathura"
       (mode-io-correlate " --synctex-forward %n:0:%b")
       " %s.pdf"))
     ("View with Sumatra"
      ("sumatra -reuse-instance"
       (mode-io-correlate " -forward-search %b %n ")
       " %o")))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and start")
     (output-dvi "Yap")
     (output-pdf "View with Zathura")
     (output-html "start"))))
 '(abm-ignore-buffers
   (quote
    ("\\.ido\\.last" "\\.git" "\\.svn" "\\.log" "Maildir" "\\*message\\*" "\\.cask" "\\.avfs")))
 '(ag-highlight-search t)
 '(allout-prefixed-keybindings
   (quote
    (("[(control ?n)]" allout-next-visible-heading)
     ("[(control ?p)]" allout-previous-visible-heading)
     ("[(control ?u)]" allout-up-current-level)
     ("[(control ?f)]" allout-forward-current-level)
     ("[(control ?b)]" allout-backward-current-level)
     ("[(control ?a)]" allout-beginning-of-current-entry)
     ("[(control ?e)]" allout-end-of-entry)
     ("[(control ?i)]" allout-show-children)
     ("[(control ?s)]" allout-show-current-subtree)
     ("[(control ?t)]" allout-toggle-current-subtree-exposure)
     ("[?h]" allout-hide-current-subtree)
     ("[(control ?o)]" allout-show-current-entry)
     ("[?!]" allout-show-all)
     ("[?x]" allout-toggle-current-subtree-encryption)
     ("[? ]" allout-open-sibtopic)
     ("[?.]" allout-open-subtopic)
     ("[?,]" allout-open-supertopic)
     ("[?']" allout-shift-in)
     ("[?>]" allout-shift-in)
     ("[?<]" allout-shift-out)
     ("[(control ?m)]" allout-rebullet-topic)
     ("[?*]" allout-rebullet-current-heading)
     ("[?#]" allout-number-siblings)
     ("[(control ?k)]" allout-kill-topic)
     ("[(meta ?k)]" allout-copy-topic-as-kill)
     ("[?@]" allout-resolve-xref)
     ("[?=?c]" allout-copy-exposed-to-buffer)
     ("[?=?i]" allout-indented-exposed-to-buffer)
     ("[?=?t]" allout-latexify-exposed)
     ("[?=?p]" allout-flatten-exposed-to-buffer)
     ("[(control ?c)]" allout-hide-bodies))))
 '(appt-audible nil)
 '(appt-display-format nil)
 '(appt-display-interval 5)
 '(appt-message-warning-time 15)
 '(auto-save-file-name-transforms nil)
 '(background-color "#002b36")
 '(background-mode dark)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(bdf-directory-list (quote ("/usr/share/emacs/fonts/bdf")))
 '(bind-key-column-widths (quote (20 . 70)))
 '(bind-key-describe-special-forms t)
 '(bjump-dired-open-command (quote dired-open-file))
 '(blink-cursor-mode nil)
 '(blink-matching-paren nil)
 '(bmkp-bmenu-commands-file "~/.emacs.d/bookmarks/emacs-bmk-bmenu-commands.el")
 '(bmkp-bmenu-image-bookmark-icon-file nil)
 '(bmkp-bmenu-state-file "~/.emacs.d/bookmarks/emacs-bmk-bmenu-state.el")
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks/bookmarks")
 '(bookmark-default-file "~/.emacs.d/bookmarks/bookmarks")
 '(bookmark-version-control t)
 '(browse-kill-ring-quit-action (quote save-and-restore))
 '(bs-configurations
   (quote
    (("all" nil nil nil nil nil)
     ("files" nil nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)
     ("files-and-scratch" "^\\*scratch" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)
     ("all-intern-last" nil nil nil nil bs-sort-buffer-interns-are-last)
     ("dired" nil
      (lambda
        (b)
        (with-current-buffer b
          (eq major-mode
              (quote dired-mode))))
      nil
      (lambda
        (b)
        t)
      nil))))
 '(calc-settings-file "~/.emacs.d/calc-settings.el")
 '(calc-undo-length 1000)
 '(calendar-latitude 49.2)
 '(calendar-longitude 16.633)
 '(calendar-mark-diary-entries-flag t)
 '(calendar-view-diary-initially-flag t)
 '(calendar-week-start-day 1)
 '(circe-active-users-timeout 21600)
 '(circe-default-nick "Fuco")
 '(circe-default-part-message "Part")
 '(circe-default-quit-message "Quit")
 '(circe-default-realname "foobar")
 '(circe-default-user "fuco")
 '(circe-fool-list (quote ("Farsus")))
 '(circe-format-action "--> {nick} {body}")
 '(circe-format-not-tracked
   (quote
    (circe-format-server-message circe-format-server-notice circe-format-server-numeric circe-format-server-topic circe-format-server-rejoin circe-format-server-lurker-activity)))
 '(circe-format-say "<{nick}> {body}")
 '(circe-format-self-action "--> {nick} {body}")
 '(circe-format-self-say "<{nick}> {body}")
 '(circe-format-server-message "--> {body}")
 '(circe-highlight-nick-type (quote all))
 '(circe-network-options
   (quote
    (("FreeNodep" :host "dasnet.cz" :port 7001 :pass my-circe-get-dasnet-irssi-passwd)
     ("BitlBeep" :host "dasnet.cz" :port 7002 :pass my-circe-get-dasnet-irssi-passwd)
     ("OFTCp" :host "dasnet.cz" :port 7003 :pass my-circe-get-dasnet-irssi-passwd))))
 '(circe-new-buffer-behavior (quote switch))
 '(circe-new-buffer-behavior-ignore-auto-joins t)
 '(circe-prompt-string ">")
 '(circe-reduce-lurker-spam t)
 '(circe-server-killed-confirmation (quote ask-and-kill-all))
 '(column-number-mode t)
 '(compilation-read-command nil)
 '(completion-ignored-extensions
   (quote
    (".cm/" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".hi")))
 '(cua-enable-cua-keys nil)
 '(cua-mode nil nil (cua-base))
 '(cursor-color "#839496")
 '(cursor-type (quote box))
 '(custom-enabled-themes (quote (my-tango-dark)))
 '(custom-safe-themes
   (quote
    ("6c0bca15239714172bf4772eb69f494b32b31dbfe42e65289ab4ed717207a603" "d162d8458661f0033ccb41806082360db0460079108dded068c29556565ba223" "5b0c30d399c03b761b319d092e050859a6d58a76fa401525368ee9f426a665a7" "1ee0d1b3c0b58b69a60ca698c2f4f76322db67c23e6a44eb199a985f7cef204d" "bba45d4eb89b3c8493fe6d3076623f2d2f89afbdbe32928d0c0bcb5c334ae90b" "7037a4e8db7ec508773a0abf6c150b6c0d18d23ab77a2ab294ac1bb19d5971e4" default)))
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(custom-unlispify-remove-prefixes t)
 '(custom-unlispify-tag-names nil)
 '(dash-enable-fontlock t)
 '(debug-on-error t)
 '(default-input-method "chinese-tonepy")
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(desktop-buffer-filter (quote my-desktop-filter))
 '(desktop-save t)
 '(diary-display-function (quote ignore))
 '(diary-file "~/org/diary")
 '(dired-avfs-archives (quote ("zip" "rar" "tar" "bz2")))
 '(dired-details-hidden-string "")
 '(dired-details-hide-extra-lines nil)
 '(dired-details-hide-link-targets t)
 '(dired-details-initially-hide t)
 '(dired-dwim-target t)
 '(dired-filter-group-saved-groups
   (quote
    (("default"
      ("Directories"
       (directory))
      ("Documents" "documents")
      ("LaTeX"
       (extension "tex" "bib"))
      ("Org"
       (extension . "org"))
      ("Archives" "archives")
      ("Media" "media"))
     ("pytest"
      ("Python"
       (extension . "py"))
      ("Symlinks"
       (symlink))))))
 '(dired-filter-mark-prefix "\\")
 '(dired-filter-prefix "/")
 '(dired-filter-saved-filters
   (quote
    (("media"
      (extension "mp3" "mp4" "MP3" "MP4" "avi" "mpg" "flv" "ogg" "wmv" "mkv" "mov" "wma"))
     ("elisp"
      (extension "el" "elc"))
     ("archives"
      (extension "zip" "rar" "gz" "bz2" "tar"))
     ("documents"
      (extension "doc" "docx" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub")))))
 '(dired-guess-shell-alist-user
   (quote
    (("\\.\\(?:djvu\\|p\\(?:df\\|s\\)\\)\\'" "zathura --fork")
     ("\\.fb2" "fbreader"))))
 '(dired-hide-details-hide-information-lines nil)
 '(dired-isearch-filenames t)
 '(dired-list-mpc-music-directory "~/media/music")
 '(dired-listing-switches "-alh")
 '(dired-narrow-exit-action (quote dired-narrow-find-file))
 '(dired-omit-extensions
   (quote
    (".o" "~" ".bin" ".bak" ".obj" ".map" ".ico" ".pif" ".lnk" ".a" ".ln" ".blg" ".bbl" ".dll" ".drv" ".vxd" ".386" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".idx" ".lof" ".lot" ".glo" ".blg" ".bbl" ".cp" ".cps" ".fn" ".fns" ".ky" ".kys" ".pg" ".pgs" ".tp" ".tps" ".vr" ".vrs" ".log" ".ilg" ".out" ".ind" ".dsc" ".hi" ".synctex.gz" ".vo" ".glob" ".meta")))
 '(dired-open-extensions
   (quote
    (("exe" . "wine")
     ("docx" . "libreoffice")
     ("doc" . "libreoffice")
     ("xlsx" . "libreoffice")
     ("xls" . "libreoffice")
     ("epub" . "fbreader"))))
 '(dired-open-functions
   (quote
    (dired-open-guess-shell-alist dired-open-by-extension dired-open-subdir)))
 '(dired-open-query-before-exit nil)
 '(dired-open-use-nohup t)
 '(display-buffer-alist
   (quote
    (((lambda
        (b _)
        (with-current-buffer b
          (eq major-mode
              (quote swb-result-mode))))
      (display-buffer-reuse-window display-buffer-pop-up-window)
      (reusable-frames)
      (window-height . my-fit-window-to-buffer))
     (".*"
      (ignore)
      (reusable-frames . t)))))
 '(display-time-24hr-format t)
 '(display-time-format "%H:%M ")
 '(display-time-string-forms
   (quote
    ((if
         (and
          (not display-time-format)
          display-time-day-and-date)
         (format-time-string "%a %b %e " now)
       "")
     (propertize
      (format-time-string
       (or display-time-format
           (if display-time-24hr-format "%H:%M" "%-I:%M%p"))
       now)
      (quote help-echo)
      (format-time-string "%a %b %e, %Y" now))
     (if mail
         (concat " "
                 (propertize display-time-mail-string
                             (quote display)
                             (\`
                              (when
                                  (and display-time-use-mail-icon
                                       (display-graphic-p))
                                (\,@ display-time-mail-icon)
                                (\,@
                                 (if
                                     (and display-time-mail-face
                                          (memq
                                           (plist-get
                                            (cdr display-time-mail-icon)
                                            :type)
                                           (quote
                                            (pbm xbm))))
                                     (let
                                         ((bg
                                           (face-attribute display-time-mail-face :background)))
                                       (if
                                           (stringp bg)
                                           (list :background bg)))))))
                             (quote face)
                             display-time-mail-face
                             (quote help-echo)
                             "You have new mail; mouse-2: Read mail"
                             (quote mouse-face)
                             (quote mode-line-highlight)
                             (quote local-map)
                             (make-mode-line-mouse-map
                              (quote mouse-2)
                              read-mail-command)))
       ""))))
 '(echo-keystrokes 0.1)
 '(ediff-diff-options "-w")
 '(ediff-merge-split-window-function (quote split-window-horizontally))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(elfeed-db-directory "~/.emacs.d/elfeed")
 '(elfeed-max-connections 5)
 '(elfeed-search-face-alist
   (quote
    ((unread elfeed-search-unread-title-face)
     (tumblr font-lock-constant-face))))
 '(elfeed-search-title-max-width 90)
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults)))
 '(elpy-rpc-backend "jedi")
 '(elpy-rpc-python-command "python3")
 '(emmet-indentation 2)
 '(emmet-preview-default nil)
 '(emms-player-list nil)
 '(enable-recursive-minibuffers t)
 '(enable-remote-dir-locals t)
 '(endless/blog-base-url "http://Fuco1.github.io/")
 '(endless/blog-dir "/home/matus/blog/")
 '(eshell-output-filter-functions
   (quote
    (eshell-handle-ansi-color eshell-postoutput-scroll-to-bottom eshell-handle-control-codes eshell-handle-ansi-color eshell-watch-for-password-prompt)))
 '(eshell-prompt-function
   (lambda nil
     (concat
      (format-time-string "[%H:%M:%S]:")
      (my-abbrev-file-name
       (eshell/pwd))
      "
"
      (if
          (=
           (user-uid)
           0)
          "># " ">$ "))))
 '(eshell-prompt-regexp "^[^#$
]*
>[#$] ")
 '(eshell-skip-prompt-function (quote my-eshell-skip-prompt))
 '(exec-path
   (quote
    ("/usr/local/sbin" "/usr/local/bin" "/usr/sbin" "/usr/bin" "/sbin" "/bin" "/usr/games" "/usr/local/games" "/usr/local/libexec/emacs/24.3/x86_64-unknown-linux-gnu" "/home/matus/bin")))
 '(find-grep-options "-qE")
 '(flycheck-disabled-checkers (quote (php-phplint)))
 '(flycheck-emacs-lisp-load-path (quote inherit))
 '(flycheck-ghc-args nil)
 '(flycheck-ghc-search-path
   (quote
    ("/home/matus/dotfiles/xmonad/.xmonad/lib" "/home/matus/dev/haskell/mpris/src/")))
 '(flycheck-gnat-include-path (quote ("/home/matus/dev/ada/whitakers-words/src")))
 '(flycheck-phpcs-standard "PW")
 '(flycheck-phpmd-rulesets (quote ("codesize" "controversial" "design")))
 '(font-latex-math-environments
   (quote
    ("display" "displaymath" "equation" "eqnarray" "gather" "multline" "align" "alignat" "xalignat" "derivation")))
 '(font-latex-quotes (quote auto))
 '(font-latex-user-keyword-classes
   (quote
    (("refs"
      (("tindex" "{")
       ("sindex" "{")
       ("index" "[{")
       ("cref" "{"))
      (:inherit
       (font-lock-constant-face))
      command))))
 '(foreground-color "#839496")
 '(free-keys-ignored-bindings
   (quote
    (("s" . "pnPNjkhl1234567890qwerb")
     ("A" . "1234567890qwer,.[]=c"))))
 '(free-keys-modifiers (quote ("" "C" "M" "C-M" "A" "H" "s")))
 '(gc-cons-threshold 20000000)
 '(global-flex-isearch-mode t)
 '(global-paren-face-mode t)
 '(global-subword-mode t)
 '(global-undo-tree-mode t)
 '(gnus-alias-default-identity "goljer")
 '(gnus-alias-identity-alist
   (quote
    (("goljer" "" "Matúš Goljer <matus.goljer@gmail.com>" ""
      (("Fcc" lambda nil "/home/matus/Maildir/Goljer/sent"))
      "" "~/.signature")
     ("dota" "" "Matúš Goljer <dota.keys@gmail.com>" ""
      (("Fcc" lambda nil "/home/matus/Maildir/Dota/sent"))
      "" "~/.signature")
     ("logio" "" "Matúš Goljer <goljer@logio.cz>" ""
      (("Fcc" lambda nil "/home/matus/Maildir/Logio/sent"))
      "" "~/.signature-logio"))))
 '(gnus-alias-identity-rules
   (quote
    (("logio-to-header"
      ("to" ".*logio.*" both)
      "logio"))))
 '(god-mod-alist (quote ((nil . "C-") ("g" . "M-") ("i" . "C-M-"))))
 '(google-this-keybind "\"g\"")
 '(guide-key-mode t)
 '(guide-key/guide-key-sequence
   (quote
    ("C-x r" "C-x 4" "C-x j" "C-x p" "C-x n" "A-x" "M-g" "M-s"
     (calc-mode "V" "v" "k" "a" "u" "j")
     (dired-mode "/" "*" "C-t" "%" "c" "\\")
     (ibuffer-mode "/" "*" "%"))))
 '(guide-key/idle-delay 0.6)
 '(guide-key/popup-window-position (quote bottom))
 '(guide-key/recursive-key-sequence-flag t)
 '(haskell-mode-hook
   (quote
    (turn-on-haskell-indentation turn-on-haskell-doc-mode)))
 '(haskell-process-args-ghci (quote ("-ferror-spans" "-i.")))
 '(haskell-program-name "ghci +RTS -M300m")
 '(helm-buffer-max-length 50)
 '(helm-buffer-skip-remote-checking t)
 '(helm-candidate-number-limit 1000)
 '(helm-for-files-preferred-list
   (quote
    (helm-source-buffers-list helm-source-recentf helm-source-bookmarks helm-source-file-cache helm-source-files-in-current-dir)))
 '(highlight-thing-limit-to-defun t)
 '(history-length 3000)
 '(ibuffer-fontification-alist
   (quote
    ((10 buffer-read-only font-lock-constant-face)
     (15
      (and buffer-file-name
           (string-match ibuffer-compressed-file-name-regexp buffer-file-name))
      font-lock-doc-face)
     (20
      (string-match "^*"
                    (buffer-name))
      font-lock-keyword-face)
     (25
      (and
       (string-match "^ "
                     (buffer-name))
       (null buffer-file-name))
      italic)
     (30
      (memq major-mode ibuffer-help-buffer-modes)
      font-lock-comment-face)
     (35
      (memq major-mode
            (quote
             (dired-mode sr-mode)))
      font-lock-function-name-face))))
 '(ibuffer-saved-filter-groups
   (quote
    (("default"
      ("Org"
       (mode . org-mode))
      ("emacs-config"
       (or
        (predicate let
                   ((bfn
                     (buffer-file-name
                      (current-buffer))))
                   (when bfn
                     (and
                      (string-match-p "\\.emacs\\.d" bfn)
                      (eq major-mode
                          (quote emacs-lisp-mode)))))))
      ("emacs"
       (or
        (mode . emacs-lisp-mode)
        (mode . lisp-interaction-mode)
        (mode . inferior-emacs-lisp-mode)))
      ("TeX"
       (or
        (mode . tex-mode)
        (mode . plain-tex-mode)
        (mode . latex-mode)))
      ("Markdown"
       (or
        (mode . markdown-mode)
        (mode . gfm-mode)))
      ("Web"
       (or
        (mode . html-mode)
        (mode . css-mode)
        (mode . php-mode)
        (mode . js-mode)))
      ("Dired"
       (mode . dired-mode))
      ("Images"
       (or
        (mode . image-dired-display-image-mode)
        (mode . image-dired-thumbnail-mode)
        (mode . image-mode)))
      ("Tramp"
       (or
        (name . "tramp")))
      ("Programming"
       (or
        (mode . c-mode)
        (mode . perl-mode)
        (mode . python-mode)
        (mode . cc-mode)))))))
 '(ibuffer-saved-filters
   (quote
    (("irc"
      ((mode . erc-mode)))
     ("dipl"
      ((filename . "_dipl")))
     ("gnus"
      ((or
        (mode . message-mode)
        (mode . mail-mode)
        (mode . gnus-group-mode)
        (mode . gnus-summary-mode)
        (mode . gnus-article-mode))))
     ("programming"
      ((or
        (mode . emacs-lisp-mode)
        (mode . cperl-mode)
        (mode . c-mode)
        (mode . java-mode)
        (mode . idl-mode)
        (mode . lisp-mode)))))))
 '(ibuffer-show-empty-filter-groups nil)
 '(ibuffer-truncate-lines nil)
 '(image-dired-cmd-create-temp-image-options
   "%p -size %wx%h \"%f\" -sample \"%wx%h>\" -strip jpeg:\"%t\"")
 '(image-dired-cmd-create-thumbnail-options
   "%p -size %wx%h \"%f\" -resize \"%wx%h>\" -strip jpeg:\"%t\"")
 '(imenu-auto-rescan t)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice (quote my-startup-screen))
 '(initial-major-mode (quote fundamental-mode))
 '(ispell-program-name "hunspell")
 '(jump-char-forward-key "m")
 '(keyfreq-autosave-mode t)
 '(keyfreq-file "~/.emacs.d/.emacs.keyfreq")
 '(keyfreq-mode t)
 '(ledger-reconcile-default-commodity "Kc")
 '(ledger-reports
   (quote
    (("portfolio" "ledger -f %(ledger-file) bal --group-by commodity --group-title-format '%-40(total_expr)' -n Assets:Broker -X Kc | grep -v '^$'")
     ("expenses" "ledger -f %(ledger-file) -p \"this month\" -X Kc --monthly --real reg ^expenses")
     ("expenses-year" "ledger -f %(ledger-file) -p \"this year\" -X Kc --monthly --real reg ^expenses")
     ("cash-flow-monthly" "ledger -f %(ledger-file) -p \"this month\" bal -X Kc ^income ^expenses")
     ("cash-flow" "ledger -f %(ledger-file) bal -X Kc ^income ^expenses")
     ("budget" "ledger -f %(ledger-file) budget -X Kc not ^Assets:Checking")
     ("expenses-budget" "ledger -f %(ledger-file) -p \"this month\" -X Kc --budget --monthly reg expenses")
     ("expenses-budget-year" "ledger -f %(ledger-file) -p \"this year\" -X Kc --budget --monthly reg expenses")
     ("equity" "ledger -f %(ledger-file) bal -X Kc ^assets ^liabilities")
     ("bal" "ledger -f %(ledger-file) -X Kc bal")
     ("reg" "ledger -f %(ledger-file) reg")
     ("payee" "ledger -f %(ledger-file) reg @%(payee)")
     ("account" "ledger -f %(ledger-file) reg %(account)"))))
 '(legalese-date-format (quote ordinal))
 '(legalese-default-copyright "Matúš Goljer")
 '(legalese-templates
   (quote
    ((emacs-lisp-mode
      (nil ";;; " legalese-file-name " --- " _ "
" "
" ";; Copyright (C) " legalese-year " " legalese-copyright "
" "
" ";; Author: " legalese-author "
" ";; Maintainer: " legalese-author "
" ";; Version: 0.0.1
" ";; Created: " legalese-date "
" ";; Package-requires: ((dash \"2.10.0\"))
" ";; Keywords: "
((legalese-elisp-keyword)
 str ", ")
& -2 "
" "
" @
(quote
 (legalese-license))
@ "
" ";;; Commentary:
" "
" ";;; Code:
" "
" "
" "(provide '" legalese-file ")
" ";;; " legalese-file-name " ends here
")))))
 '(line-number-mode t)
'(log-edit-hook
(quote
 (log-edit-insert-cvs-template log-edit-insert-changelog log-edit-show-files)))
 '(look-show-subdirs t)
 '(ls-lisp-dirs-first t)
 '(ls-lisp-use-insert-directory-program nil)
 '(ls-lisp-use-localized-time-format t)
 '(ls-lisp-verbosity (quote (uid gid)))
 '(lui-fill-column 3000)
 '(lui-highlight-keywords (quote (("^<.*?>" 0 circe-originator-face))))
 '(lui-max-buffer-size 100000)
 '(lui-time-stamp-format "[%H:%M:%S] ")
 '(lui-time-stamp-only-when-changed-p nil)
 '(lui-time-stamp-position (quote left))
 '(magit-diff-refine-hunk (quote all))
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate")))
 '(magit-push-always-verify nil)
 '(mail-envelope-from (quote header))
 '(mail-specify-envelope-from t)
 '(make-pointer-invisible t)
 '(markdown-link-space-sub-char "-")
 '(max-lisp-eval-depth 1000)
 '(max-specpdl-size 10000)
 '(message-forward-as-mime nil)
 '(message-kill-buffer-on-exit t)
 '(message-log-max 10000)
 '(message-send-mail-function (quote smtpmail-send-it))
 '(message-sendmail-envelope-from (quote header))
 '(mis-bindings-alist nil)
 '(mis-make-command "make -j2")
 '(mis-recipes-directory "~/.emacs.d/elpa/make-it-so-20141203.811/recipes/")
 '(mm-inline-large-images (quote resize))
 '(mouse-highlight nil)
 '(mouse-wheel-progressive-speed nil)
 '(notmuch-archive-tags (quote ("-inbox" "-unread" "+archived")))
 '(notmuch-fcc-dirs nil)
'(notmuch-hello-sections
(quote
 (notmuch-hello-insert-header notmuch-hello-insert-search notmuch-hello-insert-recent-searches notmuch-hello-insert-alltags notmuch-hello-insert-footer)))
 '(notmuch-search-oldest-first nil)
 '(notmuch-show-logo nil)
'(omnisharp-server-executable-path
"/home/matus/sources/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe")
 '(org-M-RET-may-split-line nil)
 '(org-adapt-indentation t)
'(org-agenda-clock-consistency-checks
(quote
 (:max-duration "10:00" :min-duration 0 :max-gap "0:20" :gap-ok-around
  ("4:00")
  :default-face
  ((:foreground "Red"))
  :overlap-face nil :gap-face nil :no-end-time-face nil :long-face nil :short-face nil)))
 '(org-agenda-compact-blocks t)
 '(org-agenda-files "~/org/.files")
'(org-agenda-finalize-hook
(quote
 (my-org-agenda-remove-empty-lists my-org-agenda-to-appt org-clock-budget-insert-into-agenda)))
 '(org-agenda-include-diary t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-function-global (quote my-org-global-skip-function))
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-skip-timestamp-if-done t)
'(org-agenda-sorting-strategy
(quote
 ((agenda time-up priority-down category-keep)
  (todo priority-down category-keep)
  (tags priority-down category-keep)
  (search category-keep))))
 '(org-agenda-span (quote day))
 '(org-agenda-start-with-clockreport-mode nil)
 '(org-agenda-sticky t)
 '(org-agenda-tags-column -140)
 '(org-agenda-tags-todo-honor-ignore-options t)
'(org-agenda-time-grid
(quote
 ((daily weekly today require-timed)
  #("----------------" 0 16
    (org-heading t))
  (800 1000 1200 1400 1600 1800 2000))))
 '(org-agenda-window-setup (quote current-window))
 '(org-babel-load-languages (quote ((emacs-lisp . t) (plantuml . t) (sh . t))))
 '(org-clock-budget-default-sort-column (quote ("BUDGET_WEEK" ratio desc)))
'(org-clock-budget-intervals
(quote
 (("BUDGET_YEAR" org-clock-budget-interval-this-year)
  ("BUDGET_WEEK" org-clock-budget-interval-this-week))))
'(org-clock-budget-ratio-faces
(quote
 ((1.0 font-lock-warning-face)
  (0.9 font-lock-variable-name-face)
  (0.0 font-lock-keyword-face))))
 '(org-clock-display-default-range (quote untilnow))
 '(org-clock-history-length 35)
 '(org-clock-in-resume t)
 '(org-clock-in-switch-to-state (quote my-org-clock-in-to-next))
 '(org-clock-into-drawer "CLOCK")
 '(org-clock-mode-line-total (quote today))
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-clock-out-when-done nil)
 '(org-clock-persist t)
 '(org-clock-persist-query-resume nil)
 '(org-clock-report-include-clocking-task t)
'(org-columns-default-format
"%50ITEM(Task) %TODO(Todo) %SCHEDULED(Scheduled) %DEADLINE(Deadline) %TAGS(Tags) %CLOCKSUM(Clock) %Effort(Effort) %BUDGET_YEAR(B/Y) %BUDGET_WEEK(B/W)")
 '(org-complete-tags-always-offer-all-agenda-tags t)
 '(org-completion-use-ido t)
 '(org-contacts-files (quote ("~/org/contacts.org")))
 '(org-cycle-emulate-tab nil)
 '(org-deadline-warning-days 30)
 '(org-default-notes-file "~/org/refile.org")
 '(org-default-priority 67)
 '(org-drill-add-random-noise-to-intervals-p t)
 '(org-drill-adjust-intervals-for-early-and-late-repetitions-p t)
'(org-drill-card-type-alist
(quote
 ((nil org-drill-present-simple-card)
  ("simple" org-drill-present-simple-card)
  ("twosided" org-drill-present-two-sided-card nil t)
  ("multisided" org-drill-present-multi-sided-card nil t)
  ("hide1cloze" org-drill-present-multicloze-hide1)
  ("hide2cloze" org-drill-present-multicloze-hide2)
  ("show1cloze" org-drill-present-multicloze-show1)
  ("show2cloze" org-drill-present-multicloze-show2)
  ("multicloze" org-drill-present-multicloze-hide1)
  ("hidefirst" org-drill-present-multicloze-hide-first)
  ("hidelast" org-drill-present-multicloze-hide-last)
  ("hide1_firstmore" org-drill-present-multicloze-hide1-firstmore)
  ("show1_lastmore" org-drill-present-multicloze-show1-lastmore)
  ("show1_firstless" org-drill-present-multicloze-show1-firstless)
  ("conjugate" org-drill-present-verb-conjugation org-drill-show-answer-verb-conjugation)
  ("decline_noun" org-drill-present-noun-declension org-drill-show-answer-noun-declension)
  ("spanish_verb" org-drill-present-spanish-verb)
  ("translate_number" org-drill-present-translate-number)
  ("twosidednocloze" org-drill-present-two-sided-card-no-cloze nil t))))
 '(org-drill-forgetting-index 50)
 '(org-drill-item-count-includes-failed-items-p t)
 '(org-drill-learn-fraction 0.55)
 '(org-drill-leech-method (quote warn))
 '(org-drill-maximum-duration nil)
 '(org-drill-maximum-items-per-session 55)
'(org-drill-optimal-factor-matrix
(quote
 ((29
   (1.3 . 1.115)
   (1.1600000000000001 . 1.072))
  (28
   (1.1600000000000001 . 1.072)
   (1.3 . 1.115))
  (27
   (1.3 . 1.115)
   (1.1600000000000001 . 1.072))
  (26
   (1.1600000000000001 . 1.072)
   (1.3 . 1.115))
  (25
   (0.98 . 1.029)
   (1.1600000000000001 . 1.072)
   (1.3 . 1.115))
  (24
   (0.98 . 1.029)
   (1.1600000000000001 . 1.072)
   (1.3 . 1.115))
  (23
   (0.98 . 1.029)
   (1.1600000000000001 . 1.072)
   (1.3 . 1.115))
  (22
   (0.98 . 1.029)
   (1.3 . 1.115)
   (1.1600000000000001 . 1.072))
  (21
   (1.26 . 1.346)
   (0.98 . 1.029)
   (1.1600000000000001 . 1.115)
   (1.3 . 1.115))
  (20
   (0.98 . 1.029)
   (1.4 . 1.4)
   (1.26 . 1.346)
   (1.3 . 1.115)
   (1.1600000000000001 . 1.072))
  (19
   (1.26 . 1.346)
   (1.4000000000000001 . 1.158)
   (0.98 . 1.029)
   (1.1600000000000001 . 1.03)
   (1.3 . 1.115))
  (18
   (0.98 . 1.029)
   (1.4 . 1.4)
   (1.26 . 1.346)
   (1.3 . 1.115)
   (1.1600000000000001 . 1.072))
  (17
   (1.4000000000000001 . 1.158)
   (1.26 . 1.346)
   (1.4 . 1.4)
   (0.98 . 1.029)
   (1.1600000000000001 . 1.072)
   (1.3 . 1.115))
  (16
   (0.98 . 1.029)
   (1.26 . 1.346)
   (1.4000000000000001 . 1.205)
   (1.1600000000000001 . 1.072)
   (1.3 . 1.115))
  (15
   (1.26 . 1.346)
   (0.98 . 1.029)
   (1.4000000000000001 . 1.158)
   (1.1600000000000001 . 1.072)
   (1.3 . 1.115))
  (14
   (0.98 . 1.029)
   (1.4000000000000001 . 1.158)
   (1.26 . 1.346)
   (1.2200000000000002 . 1.308)
   (1.1600000000000001 . 1.03)
   (1.3 . 1.115))
  (13
   (0.76 . 0.986)
   (1.4000000000000001 . 1.205)
   (1.36 . 1.442)
   (1.1600000000000001 . 1.115)
   (0.98 . 1.071)
   (1.3 . 1.16)
   (1.26 . 1.346))
  (12
   (0.98 . 1.029)
   (1.26 . 1.346)
   (1.2200000000000002 . 1.308)
   (1.5 . 1.454)
   (1.1600000000000001 . 1.072)
   (1.3 . 1.115)
   (1.4000000000000001 . 1.205))
  (11
   (1.36 . 1.442)
   (1.2200000000000002 . 1.308)
   (1.26 . 1.346)
   (1.4000000000000001 . 1.158)
   (0.98 . 1.029)
   (1.1600000000000001 . 1.115)
   (1.3 . 1.115))
  (10
   (1.0799999999999998 . 1.292)
   (1.1199999999999999 . 1.329)
   (1.24 . 1.327)
   (1.4000000000000001 . 1.325)
   (1.5 . 1.454)
   (0.98 . 1.071)
   (1.36 . 1.442)
   (1.4 . 1.4)
   (1.26 . 1.346)
   (1.3 . 1.115)
   (1.1600000000000001 . 1.115))
  (9
   (1.52 . 1.596)
   (1.02 . 1.313)
   (1.58 . 1.654)
   (1.4800000000000002 . 1.558)
   (1.48 . 1.433)
   (1.3800000000000001 . 1.461)
   (1.2 . 1.403)
   (1.66 . 1.66)
   (1.34 . 1.423)
   (1.2000000000000002 . 1.368)
   (1.38 . 1.38)
   (1.4400000000000002 . 1.519)
   (1.2200000000000002 . 1.308)
   (1.24 . 1.327)
   (1.4 . 1.4)
   (1.26 . 1.346)
   (0.76 . 0.986)
   (0.98 . 1.029)
   (1.5 . 1.454)
   (1.4000000000000001 . 1.158)
   (1.1600000000000001 . 1.03)
   (1.3 . 1.16))
  (8
   (1.5799999999999998 . 1.754)
   (1.3399999999999999 . 1.532)
   (1.6800000000000002 . 1.75)
   (1.76 . 1.827)
   (1.9 . 1.9)
   (1.86 . 1.923)
   (1.4400000000000002 . 1.59)
   (1.02 . 1.272)
   (1.16 . 1.323)
   (1.8 . 1.865)
   (1.2 . 1.473)
   (1.6600000000000001 . 1.793)
   (1.7200000000000002 . 1.682)
   (1.44 . 1.44)
   (1.4800000000000002 . 1.689)
   (1.66 . 1.66)
   (1.52 . 1.596)
   (1.62 . 1.757)
   (1.58 . 1.654)
   (1.38 . 1.38)
   (1.48 . 1.433)
   (1.36 . 1.36)
   (1.0599999999999998 . 1.274)
   (1.2000000000000002 . 1.325)
   (1.34 . 1.378)
   (1.3800000000000001 . 1.535)
   (1.24 . 1.327)
   (1.26 . 1.527)
   (1.4000000000000001 . 1.321)
   (1.4 . 1.4)
   (1.1600000000000001 . 1.223)
   (1.3 . 1.221)
   (0.98 . 1.174))
  (7
   (1.9999999999999998 . 2.141)
   (1.4 . 1.4)
   (1.5 . 1.454)
   (1.5999999999999999 . 1.772)
   (2.22 . 2.22)
   (1.44 . 1.686)
   (2.56 . 2.56)
   (2.38 . 2.417)
   (1.9599999999999997 . 2.148)
   (2.28 . 2.327)
   (2.62 . 2.617)
   (1.78 . 1.938)
   (2.42 . 2.42)
   (2.14 . 2.177)
   (2.2800000000000002 . 2.264)
   (1.6199999999999999 . 1.791)
   (2.2399999999999998 . 2.288)
   (1.5799999999999998 . 1.754)
   (2.1799999999999997 . 2.231)
   (1.7200000000000002 . 1.935)
   (2.24 . 2.222)
   (1.9400000000000002 . 2.053)
   (2.18 . 2.18)
   (2.08 . 2.135)
   (2.32 . 2.32)
   (1.72 . 1.935)
   (1.9 . 1.9)
   (1.86 . 2.012)
   (2.04 . 2.096)
   (1.82 . 1.885)
   (1.0799999999999998 . 1.292)
   (1.94 . 1.94)
   (2.0 . 2.093)
   (1.9000000000000001 . 2.015)
   (2.1399999999999997 . 2.237)
   (1.4400000000000002 . 1.741)
   (1.3399999999999999 . 1.532)
   (1.8 . 1.865)
   (1.58 . 1.811)
   (1.36 . 1.442)
   (1.0599999999999998 . 1.274)
   (1.76 . 1.724)
   (1.6600000000000001 . 1.793)
   (1.66 . 1.66)
   (1.2000000000000002 . 1.591)
   (1.2 . 1.473)
   (1.62 . 1.657)
   (1.48 . 1.721)
   (1.34 . 1.655)
   (1.52 . 1.596)
   (1.26 . 1.346)
   (1.3800000000000001 . 1.535)
   (1.4800000000000002 . 1.689)
   (1.38 . 1.38)
   (1.4000000000000001 . 1.112)
   (0.98 . 1.178)
   (1.24 . 1.327)
   (1.1600000000000001 . 1.03)
   (1.3 . 1.071))
  (6
   (2.2 . 2.392)
   (3.0 . 3.02)
   (2.7600000000000002 . 2.8)
   (3.1 . 3.116)
   (2.9 . 2.908)
   (2.86 . 2.904)
   (2.0999999999999996 . 2.304)
   (2.1999999999999997 . 2.25)
   (2.6199999999999997 . 2.688)
   (2.06 . 2.115)
   (2.48 . 2.48)
   (2.24 . 2.299)
   (1.82 . 1.885)
   (1.9200000000000002 . 1.981)
   (2.62 . 2.692)
   (2.7 . 2.7)
   (1.0799999999999998 . 1.292)
   (2.8000000000000003 . 2.804)
   (2.76 . 2.796)
   (2.8 . 2.8)
   (2.6599999999999997 . 2.692)
   (1.9600000000000002 . 2.09)
   (2.1 . 2.174)
   (2.66 . 2.696)
   (2.36 . 2.36)
   (2.2199999999999998 . 2.269)
   (2.6 . 2.6)
   (1.9 . 1.9)
   (1.4 . 1.4)
   (2.46 . 2.5)
   (2.56 . 2.596)
   (2.52 . 2.592)
   (0.8399999999999999 . 1.221)
   (2.0 . 1.973)
   (2.42 . 2.496)
   (2.28 . 2.4)
   (2.38 . 2.492)
   (2.1399999999999997 . 2.308)
   (1.9999999999999998 . 2.219)
   (2.3200000000000003 . 2.305)
   (2.1799999999999997 . 2.311)
   (1.6199999999999999 . 1.791)
   (1.2000000000000002 . 1.325)
   (1.5 . 1.5)
   (1.86 . 2.046)
   (2.22 . 2.22)
   (1.7200000000000002 . 1.968)
   (1.3599999999999999 . 1.681)
   (1.58 . 1.787)
   (1.72 . 1.935)
   (1.4800000000000002 . 1.594)
   (2.18 . 2.217)
   (2.32 . 2.404)
   (1.5799999999999998 . 1.754)
   (1.26 . 1.346)
   (2.04 . 2.132)
   (1.34 . 1.378)
   (1.9400000000000002 . 2.053)
   (2.08 . 2.135)
   (1.3399999999999999 . 1.532)
   (2.14 . 2.214)
   (1.94 . 1.94)
   (1.4400000000000002 . 1.519)
   (1.66 . 1.66)
   (1.4000000000000001 . 1.205)
   (2.2800000000000002 . 2.302)
   (1.2 . 1.473)
   (1.8 . 1.865)
   (1.48 . 1.433)
   (1.9000000000000001 . 2.05)
   (1.76 . 1.971)
   (1.6600000000000001 . 1.793)
   (0.98 . 1.029)
   (1.0599999999999998 . 1.274)
   (1.62 . 1.658)
   (1.52 . 1.596)
   (1.38 . 1.38)
   (1.24 . 1.327)
   (0.76 . 0.986)
   (1.3 . 1.115)
   (1.3800000000000001 . 1.535)
   (1.1600000000000001 . 1.03))
  (5
   (0.76 . 1.129)
   (2.38 . 2.488)
   (2.44 . 2.581)
   (2.86 . 2.86)
   (3.06 . 3.074)
   (2.24 . 2.392)
   (2.72 . 2.72)
   (2.06 . 2.035)
   (2.34 . 2.385)
   (2.0999999999999996 . 2.3)
   (1.9600000000000002 . 2.126)
   (2.2 . 2.389)
   (2.58 . 2.684)
   (2.1 . 2.154)
   (1.2000000000000002 . 1.591)
   (1.16 . 1.323)
   (1.9599999999999997 . 2.212)
   (1.5799999999999998 . 1.754)
   (1.9 . 1.9)
   (1.4400000000000002 . 1.59)
   (2.8 . 2.8)
   (1.44 . 1.686)
   (2.9 . 2.908)
   (2.48 . 2.48)
   (1.66 . 1.66)
   (1.5 . 1.5)
   (1.34 . 1.655)
   (2.4799999999999995 . 2.584)
   (3.0 . 3.02)
   (1.4 . 1.716)
   (2.76 . 2.796)
   (2.0 . 2.129)
   (2.1399999999999997 . 2.304)
   (1.26 . 1.65)
   (2.14 . 2.214)
   (2.7600000000000002 . 2.796)
   (2.6599999999999997 . 2.692)
   (1.4000000000000001 . 1.321)
   (1.9999999999999998 . 2.215)
   (2.7 . 2.696)
   (1.7200000000000002 . 1.968)
   (1.02 . 1.237)
   (1.86 . 2.047)
   (2.52 . 2.588)
   (2.8000000000000003 . 2.8)
   (2.28 . 2.396)
   (1.38 . 1.38)
   (2.66 . 2.692)
   (1.62 . 1.657)
   (2.42 . 2.492)
   (1.4800000000000002 . 1.822)
   (1.1199999999999999 . 1.468)
   (2.6 . 2.596)
   (2.46 . 2.496)
   (2.36 . 2.404)
   (1.72 . 1.917)
   (0.98 . 1.174)
   (2.2199999999999998 . 2.311)
   (2.2800000000000002 . 2.302)
   (2.56 . 2.592)
   (1.9000000000000001 . 2.049)
   (2.3200000000000003 . 2.305)
   (1.58 . 1.843)
   (1.3399999999999999 . 1.532)
   (1.54 . 1.615)
   (2.18 . 2.217)
   (2.04 . 2.132)
   (2.22 . 2.22)
   (2.32 . 2.4)
   (1.94 . 1.94)
   (1.6199999999999999 . 1.791)
   (1.0599999999999998 . 1.274)
   (1.9400000000000002 . 2.053)
   (1.2 . 1.473)
   (2.1799999999999997 . 2.308)
   (1.76 . 1.827)
   (1.48 . 1.721)
   (2.08 . 2.135)
   (1.1600000000000001 . 1.223)
   (1.24 . 1.327)
   (1.6600000000000001 . 1.793)
   (1.3 . 1.323)
   (1.8 . 1.865)
   (1.3800000000000001 . 1.535)
   (1.52 . 1.596))
  (4
   (1.36 . 1.398)
   (1.68 . 1.68)
   (1.72 . 1.72)
   (1.16 . 1.588)
   (2.02 . 2.293)
   (2.86 . 2.866)
   (2.58 . 2.615)
   (2.96 . 2.96)
   (1.9600000000000002 . 2.019)
   (1.5799999999999998 . 1.754)
   (2.14 . 2.214)
   (2.7199999999999998 . 2.75)
   (0.94 . 1.522)
   (1.86 . 2.047)
   (1.58 . 1.654)
   (2.1999999999999997 . 2.25)
   (1.9 . 1.9)
   (1.5 . 1.454)
   (2.28 . 2.396)
   (2.4799999999999995 . 2.584)
   (1.4800000000000002 . 1.822)
   (1.4000000000000001 . 1.586)
   (1.2000000000000002 . 1.591)
   (1.54 . 1.888)
   (2.2 . 2.389)
   (1.34 . 1.655)
   (2.52 . 2.588)
   (2.48 . 2.519)
   (2.8 . 2.8)
   (2.1399999999999997 . 2.304)
   (2.2800000000000002 . 2.396)
   (1.38 . 1.38)
   (2.0 . 2.129)
   (1.62 . 1.895)
   (1.66 . 1.66)
   (2.24 . 2.392)
   (0.98 . 1.466)
   (2.6599999999999997 . 2.692)
   (1.26 . 1.346)
   (2.9 . 2.908)
   (0.76 . 0.986)
   (1.9999999999999998 . 2.215)
   (1.0599999999999998 . 1.274)
   (1.3399999999999999 . 1.532)
   (2.8000000000000003 . 2.8)
   (2.7 . 2.696)
   (1.1600000000000001 . 1.527)
   (2.66 . 2.692)
   (2.42 . 2.492)
   (2.56 . 2.592)
   (1.3 . 1.276)
   (1.7200000000000002 . 1.968)
   (1.94 . 1.94)
   (2.32 . 2.4)
   (2.6 . 2.596)
   (2.04 . 2.132)
   (2.36 . 2.404)
   (2.5 . 2.5)
   (1.3800000000000001 . 1.535)
   (1.24 . 1.327)
   (1.2 . 1.473)
   (2.46 . 2.496)
   (2.1799999999999997 . 2.308)
   (2.2199999999999998 . 2.311)
   (1.48 . 1.721)
   (2.18 . 2.217)
   (1.52 . 1.596)
   (2.22 . 2.22)
   (1.9000000000000001 . 2.049)
   (1.6600000000000001 . 1.793)
   (2.08 . 2.135)
   (1.6199999999999999 . 1.791)
   (2.3200000000000003 . 2.305)
   (1.8 . 1.865)
   (1.76 . 1.971)
   (1.9400000000000002 . 2.053))
  (3
   (1.5 . 1.577)
   (1.26 . 1.346)
   (1.16 . 1.588)
   (0.76 . 1.297)
   (1.6800000000000002 . 1.964)
   (1.54 . 1.888)
   (1.9 . 1.9)
   (2.2399999999999998 . 2.277)
   (2.7600000000000002 . 2.796)
   (2.72 . 2.791)
   (2.96 . 2.976)
   (2.1 . 2.154)
   (1.5799999999999998 . 1.754)
   (1.38 . 1.38)
   (2.62 . 2.688)
   (2.86 . 2.866)
   (2.0 . 1.973)
   (1.7200000000000002 . 1.824)
   (1.4 . 1.4)
   (2.3400000000000003 . 2.326)
   (1.4400000000000002 . 1.519)
   (1.2000000000000002 . 1.591)
   (1.62 . 1.757)
   (1.44 . 1.819)
   (1.34 . 1.655)
   (2.66 . 2.692)
   (1.66 . 1.66)
   (1.4000000000000001 . 1.522)
   (2.52 . 2.588)
   (1.4800000000000002 . 1.593)
   (2.38 . 2.368)
   (2.42 . 2.492)
   (1.72 . 2.048)
   (2.14 . 2.304)
   (2.2800000000000002 . 2.302)
   (0.98 . 1.178)
   (1.1600000000000001 . 1.468)
   (1.2 . 1.473)
   (1.3 . 1.527)
   (1.0599999999999998 . 1.274)
   (1.24 . 1.327)
   (2.8000000000000003 . 2.8)
   (1.3399999999999999 . 1.532)
   (1.3800000000000001 . 1.535)
   (2.1799999999999997 . 2.308)
   (1.86 . 1.897)
   (1.52 . 1.596)
   (2.1399999999999997 . 2.192)
   (2.04 . 2.219)
   (1.6600000000000001 . 1.793)
   (1.48 . 1.433)
   (1.94 . 1.94)
   (1.6199999999999999 . 1.791)
   (1.8 . 1.865)
   (2.18 . 2.217)
   (2.7 . 2.696)
   (2.6 . 2.596)
   (2.56 . 2.592)
   (1.76 . 1.827)
   (2.5 . 2.5)
   (2.3200000000000003 . 2.305)
   (1.9400000000000002 . 2.053)
   (2.22 . 2.22)
   (2.36 . 2.404)
   (2.32 . 2.4)
   (2.46 . 2.496)
   (2.2199999999999998 . 2.311)
   (1.9000000000000001 . 2.049)
   (2.08 . 2.135))
  (2
   (1.6400000000000001 . 1.711)
   (0.76 . 1.084)
   (1.82 . 1.885)
   (2.1399999999999997 . 2.227)
   (1.54 . 1.888)
   (2.38 . 2.405)
   (2.66 . 2.66)
   (2.62 . 2.656)
   (1.9 . 1.9)
   (2.86 . 2.866)
   (1.4800000000000002 . 1.593)
   (2.24 . 2.222)
   (1.2000000000000002 . 1.591)
   (2.52 . 2.558)
   (1.44 . 1.686)
   (2.7600000000000002 . 2.762)
   (1.4000000000000001 . 1.272)
   (1.72 . 1.968)
   (1.9999999999999998 . 2.141)
   (1.58 . 1.892)
   (1.34 . 1.655)
   (2.2399999999999998 . 2.312)
   (1.66 . 1.66)
   (2.0 . 2.058)
   (2.42 . 2.409)
   (1.9599999999999997 . 2.148)
   (1.86 . 2.047)
   (2.0999999999999996 . 2.234)
   (1.62 . 1.658)
   (1.5799999999999998 . 1.754)
   (1.8199999999999998 . 2.126)
   (2.28 . 2.316)
   (2.56 . 2.592)
   (1.38 . 1.38)
   (1.1600000000000001 . 1.072)
   (1.2 . 1.473)
   (1.26 . 1.65)
   (1.0599999999999998 . 1.274)
   (1.3 . 1.115)
   (0.98 . 1.029)
   (1.24 . 1.327)
   (1.1199999999999999 . 1.468)
   (1.6800000000000002 . 1.964)
   (2.32 . 2.401)
   (1.3800000000000001 . 1.535)
   (1.52 . 1.596)
   (1.3399999999999999 . 1.532)
   (1.6600000000000001 . 1.793)
   (1.94 . 1.94)
   (2.2800000000000002 . 2.396)
   (2.3200000000000003 . 2.305)
   (1.48 . 1.721)
   (1.6199999999999999 . 1.791)
   (2.22 . 2.22)
   (1.8 . 1.865)
   (1.4 . 1.716)
   (2.18 . 2.217)
   (1.9400000000000002 . 2.053)
   (1.76 . 1.971)
   (2.08 . 2.135)
   (1.9000000000000001 . 2.049)
   (2.7 . 2.696)
   (2.1799999999999997 . 2.308)
   (2.5 . 2.5)
   (2.46 . 2.497)
   (2.2199999999999998 . 2.311)
   (2.04 . 2.132)
   (2.6 . 2.596)
   (2.36 . 2.404))
  (1
   (1.5799999999999998 . 3.692)
   (1.4400000000000002 . 3.846)
   (1.78 . 3.846)
   (2.0999999999999996 . 3.55)
   (1.9599999999999997 . 3.413)
   (1.4 . 3.538)
   (1.9600000000000002 . 3.846)
   (1.54 . 3.402)
   (1.0799999999999998 . 3.692)
   (2.18 . 4.0)
   (1.26 . 3.402)
   (2.66 . 4.0)
   (2.52 . 3.994)
   (2.76 . 3.846)
   (1.9 . 4.0)
   (1.34 . 3.413)
   (1.4000000000000001 . 3.994)
   (2.6599999999999997 . 3.846)
   (1.7200000000000002 . 3.846)
   (1.2000000000000002 . 3.282)
   (1.4800000000000002 . 3.556)
   (2.38 . 3.84)
   (1.62 . 3.698)
   (1.94 . 4.0)
   (2.1399999999999997 . 3.556)
   (2.28 . 3.698)
   (0.98 . 3.55)
   (1.8199999999999998 . 3.402)
   (2.42 . 3.846)
   (2.46 . 3.994)
   (1.66 . 4.0)
   (1.38 . 4.0)
   (2.3200000000000003 . 4.154)
   (1.1600000000000001 . 3.698)
   (1.2 . 3.55)
   (1.3 . 3.846)
   (1.24 . 3.846)
   (1.0599999999999998 . 3.692)
   (1.6800000000000002 . 3.538)
   (1.3800000000000001 . 3.698)
   (2.22 . 4.0)
   (2.32 . 3.84)
   (1.48 . 3.55)
   (1.52 . 3.846)
   (1.3399999999999999 . 3.692)
   (1.6600000000000001 . 3.698)
   (1.6199999999999999 . 3.692)
   (1.8 . 3.846)
   (1.76 . 3.846)
   (1.9400000000000002 . 3.698)
   (1.9000000000000001 . 3.413)
   (2.08 . 3.846)
   (2.04 . 3.55)
   (2.2199999999999998 . 3.698)
   (2.6 . 4.154)
   (2.1799999999999997 . 3.692)
   (2.5 . 4.0)
   (2.36 . 3.846)
   (1.7000000000000002 . 3.44)
   (1.96 . 3.538)))))
 '(org-drill-save-buffers-after-drill-sessions-p nil)
 '(org-drill-scope (quote directory))
'(org-emphasis-alist
(quote
 (("*" markup-bold)
  ("/" markup-italic)
  ("_" underline)
  ("=" markup-inline-code verbatim)
  ("~" markup-inline-code verbatim)
  ("+" markup-strike))))
 '(org-export-allow-bind-keywords t)
 '(org-fast-tag-selection-single-key (quote expert))
'(org-file-apps
(quote
 ((auto-mode . emacs)
  ("\\.mm\\'" . default)
  ("\\.x?html?\\'" . default)
  ("\\.pdf\\'" . "zathura --fork %s"))))
 '(org-footnote-auto-label (quote random))
 '(org-footnote-define-inline t)
'(org-format-latex-options
(quote
 (:foreground default :background default :scale 3.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
  ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-habit-following-days 1)
 '(org-habit-graph-column 80)
 '(org-habit-preceding-days 30)
 '(org-habit-show-habits-only-for-today nil)
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars t)
 '(org-inline-image-resolve-url (quote (org-inline-image--regexp-resolver identity)))
 '(org-latex-tables-centered nil)
 '(org-log-done (quote note))
 '(org-log-into-drawer t)
 '(org-lowest-priority 69)
'(org-modules
(quote
 (org-bbdb org-bibtex org-crypt org-gnus org-habit org-id org-info org-checklist org-drill)))
 '(org-outline-path-complete-in-steps nil)
 '(org-plantuml-jar-path "~/bin/plantuml.jar")
 '(org-priority-start-cycle-with-default nil)
 '(org-refile-allow-creating-parent-nodes (quote confirm))
 '(org-refile-target-verify-function (quote my-org-verify-refile-target))
'(org-refile-targets
(quote
 ((nil :maxlevel . 9)
  (org-agenda-files :maxlevel . 9))))
 '(org-refile-use-outline-path t)
 '(org-scheduled-delay-days 0)
 '(org-show-entry-below (quote ((default . t))))
 '(org-show-following-heading (quote ((default . t))))
 '(org-special-ctrl-a/e t)
 '(org-src-fontify-natively t)
'(org-src-lang-modes
(quote
 (("ocaml" . tuareg)
  ("elisp" . emacs-lisp)
  ("ditaa" . artist)
  ("asymptote" . asy)
  ("dot" . fundamental)
  ("sqlite" . sql)
  ("calc" . fundamental)
  ("C" . c)
  ("cpp" . c++)
  ("C++" . c++)
  ("screen" . shell-script)
  ("plantuml" . fundamental))))
 '(org-src-preserve-indentation t)
'(org-structure-template-alist
(quote
 (("s" "#+BEGIN_SRC ?

#+END_SRC" "<src lang=\"?\">

</src>")
  ("e" "#+BEGIN_EXAMPLE
?
#+END_EXAMPLE" "<example>
?
</example>")
  ("q" "#+BEGIN_QUOTE
?
#+END_QUOTE" "<quote>
?
</quote>")
  ("v" "#+BEGIN_VERSE
?
#+END_VERSE" "<verse>
?
</verse>")
  ("V" "#+BEGIN_VERBATIM
?
#+END_VERBATIM" "<verbatim>
?
</verbatim>")
  ("c" "#+BEGIN_CENTER
?
#+END_CENTER" "<center>
?
</center>")
  ("l" "#+BEGIN_LaTeX
?
#+END_LaTeX" "<literal style=\"latex\">
?
</literal>")
  ("L" "#+LaTeX: " "<literal style=\"latex\">?</literal>")
  ("h" "#+BEGIN_HTML
?
#+END_HTML" "<literal style=\"html\">
?
</literal>")
  ("H" "#+HTML: " "<literal style=\"html\">?</literal>")
  ("a" "#+BEGIN_ASCII
?
#+END_ASCII" "")
  ("A" "#+ASCII: " "")
  ("i" "#+INDEX: ?" "#+INDEX: ?")
  ("I" "#+INCLUDE: %file ?" "<include file=%file markup=\"?\">")
  ("b" "#+BEGIN: ?

#+END" "<pre>?</pre>"))))
 '(org-stuck-projects (quote ("" nil nil "")))
 '(org-support-shift-select t)
 '(org-tags-exclude-from-inheritance (quote ("folder")))
 '(org-tags-sort-function (quote string<))
'(org-time-clocksum-format
(quote
 (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))
 '(org-use-speed-commands t)
 '(package-enable-at-startup nil)
 '(paren-face-regexp "[(){}]")
 '(pj-line-width 1100)
 '(preview-scale-function 1.5)
 '(proced-auto-update-flag t)
 '(proced-auto-update-interval 1)
'(projectile-globally-ignored-directories
(quote
 (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" "elpa")))
'(projectile-project-root-files
(quote
 (".dir-locals.el" ".projectile" ".git" ".hg" ".fslckout" ".bzr" "_darcs" "rebar.config" "project.clj" "pom.xml" "build.sbt" "Gemfile" "Makefile")))
 '(proof-three-window-mode-policy (quote hybrid))
 '(rainbow-r-colors t)
 '(rcirc-fill-column (quote frame-width))
 '(rcirc-server-alist (quote (("chat.freenode.org"))))
 '(recentf-auto-cleanup (quote never))
 '(recentf-exclude (quote ("\"elpa/archives\"")) t)
 '(recentf-max-saved-items 200)
 '(recentf-save-file "~/.emacs.d/.recentf")
'(reftex-label-alist
(quote
 (("lemma" 32 "lem:" "~\\cref{%s}" nil nil)
  ("theorem" 32 "th:" "~\\cref{%s}" nil nil))))
'(safe-local-variable-values
(quote
 ((firestarter-default-type . failure)
  (flycheck-ghc-search-path . "/home/matus/dev/haskell/mpris/src/")
  (c-style-alist
   ("ledger"
    (indent-tabs-mode)
    (c-basic-offset . 2)
    (c-comment-only-line-offset 0 . 0)
    (c-hanging-braces-alist
     (substatement-open before after)
     (arglist-cont-nonempty))
    (c-offsets-alist
     (statement-block-intro . +)
     (knr-argdecl-intro . 5)
     (substatement-open . 0)
     (substatement-label . 0)
     (label . 0)
     (case-label . 0)
     (statement-case-open . 0)
     (statement-cont . +)
     (arglist-intro . +)
     (arglist-close . +)
     (inline-open . 0)
     (brace-list-open . 0)
     (topmost-intro-cont first c-lineup-topmost-intro-cont c-lineup-gnu-DEFUN-intro-cont))
    (c-special-indent-hook . c-gnu-impose-minimum)
    (c-block-comment-prefix . "")))
  (swb-database . "test")
  (swb-user . "root")
  (swb-port . "3306")
  (swb-host . "localhost")
  (firestarter . my-sync-rsync-remote)
  (firestarter quote my-sync-rsync-remote)
  (my-rsync-remote . "dasnet:/home/fuco/project/foo/")
  (eval when
        (and
         (buffer-file-name)
         (file-regular-p
          (buffer-file-name))
         (string-match-p "^[^.]"
                         (buffer-file-name)))
        (emacs-lisp-mode)
        (when
            (fboundp
             (quote flycheck-mode))
          (flycheck-mode -1))
        (unless
            (featurep
             (quote package-build))
          (let
              ((load-path
                (cons ".." load-path)))
            (require
             (quote package-build))))
        (package-build-minor-mode)
        (set
         (make-local-variable
          (quote package-build-working-dir))
         (expand-file-name "../working/"))
        (set
         (make-local-variable
          (quote package-build-archive-dir))
         (expand-file-name "../packages/"))
        (set
         (make-local-variable
          (quote package-build-recipes-dir))
         default-directory))
  (eval add-to-list
        (quote imenu-generic-expression)
        (quote
         ("Used Packages" "\\(^(use-package +\\)\\(\\_<.+\\_>\\)" 2)))
  (eval font-lock-add-keywords nil
        (quote
         (("defexamples\\|def-example-group\\| => \\| !!> \\| ~>"
           (0
            (quote font-lock-keyword-face)))
          ("(defexamples[[:blank:]]+\\(.*\\)"
           (1
            (quote font-lock-function-name-face))))))
  (eval add-to-list
        (quote imenu-generic-expression)
        (quote
         ("Sallet sources" "\\(^(sallet-defsource +\\)\\(\\_<.+?\\_>\\)" 2)))
  (eval add-to-list
        (quote imenu-generic-expression)
        (quote
         ("Sallet sources" "\\(^(sallet-defsource +\\)\\(\\_<.+\\_>\\)" 2)))
  (my-buffer-input-methods "devanagari-translit" "devanagari-kyoto-harvard")
  (my-pw-test-uuid . "test")
  (my-pw-root . "/var/www/html/devel/mg/orders-refactor")
  (my-pw-root . "/var/www/html/devel/mg/orders-refactor/")
  (my-svn-trunk . "/fw/trunk/")
  (my-svn-branch . "/fw/branches-devel/mg-orders-refactor/")
  (my-buffer-input-methods "devanagari-translit" "devanagari-aiba")
  (my-buffer-input-methods "TeX" "devanagari-aiba")
  (my-org-drill-language . "Sanskrit")
  (haskell-program-name . "ghci -lpulse")
  (eval when
        (and
         (buffer-file-name)
         (file-regular-p
          (buffer-file-name))
         (string-match-p "^[^.]"
                         (buffer-file-name)))
        (emacs-lisp-mode)
        (when
            (fboundp
             (quote flycheck-mode))
          (flycheck-mode -1))
        (unless
            (featurep
             (quote package-build))
          (let
              ((load-path
                (cons ".." load-path)))
            (require
             (quote package-build))))
        (package-build-minor-mode))
  (my-org-drill-language . "French")
  (eval font-lock-add-keywords nil
        (quote
         (("(\\(dired-filter-define\\)[[:blank:]]+\\(.+\\)"
           (1
            (quote font-lock-keyword-face))
           (2
            (quote font-lock-function-name-face))))))
  (eval font-lock-add-keywords nil
        (quote
         (("defexamples\\|def-example-group\\| => "
           (0
            (quote font-lock-keyword-face)))
          ("(defexamples[[:blank:]]+\\(.*\\)"
           (1
            (quote font-lock-function-name-face))))))
  (eval font-lock-add-keywords nil
        (quote
         (("defexamples\\|def-example-group\\| => "
           (0
            (quote font-lock-keyword-face)))
          ("(defexamples[[:blank:]]+\\(.*?\\)"
           (1
            (quote font-lock-function-name-face))))))
  (eval font-lock-add-keywords nil
        (quote
         (("defexamples\\|def-example-group\\| => "
           (0
            (quote font-lock-keyword-face)))
          ("(defexamples[:blank:]+\\(.*?\\)"
           (1
            (quote font-lock-function-name-face))))))
  (eval font-lock-add-keywords nil
        (quote
         (("defexamples\\|def-example-group\\| => "
           (0
            (quote font-lock-keyword-face)))
          ("(defexamples[ ]+\\(.*?\\)"
           (1
            (quote font-lock-function-name-face))))))
  (eval font-lock-add-keywords nil
        (quote
         (("defexamples\\|def-example-group\\| => "
           (0
            (quote font-lock-keyword-face)))
          ("(defexamples +\\(.*?\\)"
           (1
            (quote font-lock-function-name-face))))))
  (eval font-lock-add-keywords nil
        (quote
         (("defexamples\\|def-example-group\\| => "
           (0
            (quote font-lock-keyword-face))))))
  (org-refile-targets
   ("~/org/bookmarks.org" :tag . "folder"))
  (org-tags-exclude-from-inheritance "folder")
  (org-tags-exclude-from-inheritance . folder)
  (org-tags-exclude-from-inheritance . "folder")
  (eval progn
        (variable-pitch-mode 1)
        (text-scale-adjust 2)
        (overlay-put
         (make-overlay
          (point-min)
          (point-max))
         (quote face)
         (quote my-reading-face)))
  (dired-filter-stack
   (dot-files)
   (omit))
  (my-inhibit-buffer-cleanup . t)
  (eval progn
        (local-set-key
         (kbd "C-=")
         (quote my-org-add-drill-entry))
        (local-set-key
         (kbd "C-<")
         (quote my-format-russian-verb))
        (local-set-key
         (kbd "C->")
         (quote my-format-meaning)))
  (eval progn
        (local-set-key
         (kbd "C-=")
         (quote my-org-add-drill-entry))
        (local-set-key
         (kbd "C->")
         (quote my-format-meaning)))
  (eval progn
        (local-set-key
         (kbd "C-=")
         (quote my-org-add-drill-entry))
        (local-set-key
         (kbd "C->")
         (quote my-format-latin-meaning)))
  (eval progn
        (local-set-key
         (kbd "C-=")
         (quote my-org-add-drill-entry))
        (local-set-key
         (kbd "C-<")
         (quote my-format-russian-verb))
        (local-set-key
         (kbd "C->")
         (quote my-format-russian-meaning)))
  (eval progn
        (variable-pitch-mode 1)
        (text-scale-adjust 2))
  (cursor-type . bar)
  (eval progn
        (variable-pitch-mode 1)
        (text-scale-adjust 3))
  (my-org-drill-language . "Latin")
  (eval local-set-key
        (kbd "C-=")
        (quote my-org-add-drill-entry))
  (eval set-input-method "cyrillic-translit")
  (my-org-drill-language . "Russian")
  (my-org-drill-file . t)
  (my-org-drill-local-language . "Polish")
  (eval virtual-dired "d:/")
  (eval font-lock-add-keywords nil
        (\`
         (((\,
            (concat "("
                    (regexp-opt
                     (quote
                      ("wd-cond"))
                     t)
                    "\\_>"))
           1
           (quote font-lock-keyword-face)))))
  (eval push
        (file-name-directory
         (buffer-file-name))
        load-path)
  (eval font-lock-add-keywords nil
        (\`
         (((\,
            (concat "("
                    (regexp-opt
                     (quote
                      ("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl"))
                     t)
                    "\\_>"))
           1
           (quote font-lock-variable-name-face)))))
  (TeX-master . main)
  (eval font-lock-add-keywords nil
        (quote
         (("(\\(dm-defun\\)\\(?:\\s-\\)+\\(\\_<.*?\\_>\\)"
           (1 font-lock-keyword-face)
           (2 font-lock-function-name-face)))))
  (eval font-lock-add-keywords nil
        (quote
         (("defexamples\\| => "
           (0
            (quote font-lock-keyword-face))))))
  (reftex-default-bibliography "./bibliography")
  (eval allout-mode t))))
 '(sallet-buffer-sources (quote (sallet-source-buffer sallet-source-autobookmarks)))
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/.emacs-places")
 '(save-place-limit nil)
 '(scroll-preserve-screen-position 1)
'(semantic-default-submodes
(quote
 (global-semantic-idle-scheduler-mode global-semanticdb-minor-mode global-semantic-idle-summary-mode global-semantic-mru-bookmark-mode)))
 '(semantic-new-buffer-setup-functions (quote ((java-mode . wisent-malabar-java-setup))))
 '(send-mail-function (quote smtpmail-send-it))
 '(shackle-mode t)
 '(shackle-rules (quote (("*Help*" :select t))))
 '(show-smartparens-global-mode t)
 '(shr-max-image-proportion 0.9)
 '(smerge-command-prefix "\\C-.m")
 '(smex-save-file "~/.emacs.d/.smex-items")
 '(smtpmail-default-smtp-server "smtp.gmail.com")
 '(smtpmail-local-domain "herakleitos")
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 465)
 '(smtpmail-stream-type (quote ssl))
 '(sp-autoescape-string-quote-if-empty (quote (python-mode)))
 '(sp-autoinsert-if-followed-by-same 3)
 '(sp-autoinsert-quote-if-followed-by-closing-pair nil)
 '(sp-autoskip-closing-pair (quote always))
 '(sp-autoskip-opening-pair nil)
 '(sp-autowrap-region t)
 '(sp-comment-string (quote (((emacs-lisp-mode) . ";; "))))
 '(sp-hybrid-kill-excessive-whitespace nil)
'(sp-ignore-modes-list
(quote
 (image-dired-display-image-mode image-dired-thumbnail-mode ediff-mode recentf-dialog-mode google-maps-static-mode ibuffer-mode org-agenda-mode dired-mode elfeed-search-mode elfeed-show-mode notmuch-search-mode notmuch-show-mode notmuch-hello-mode circe-channel-mode)))
 '(sp-navigate-close-if-unbalanced t)
 '(sp-navigate-comments-as-sexps t)
 '(sp-navigate-consider-sgml-tags (quote (html-mode markdown-mode gfm-mode rst-mode)))
'(sp-navigate-consider-stringlike-sexp
(quote
 (ruby-mode gfm-mode emacs-lisp-mode html-mode org-mode python-mode text-mode latex-mode)))
 '(sp-navigate-consider-symbols t)
 '(sp-navigate-reindent-after-up (quote ((interactive emacs-lisp-mode))))
'(sp-navigate-skip-match
(quote
 (((ruby-mode enh-ruby-mode)
   . sp--ruby-skip-match)
  ((emacs-lisp-mode inferior-emacs-lisp-mode lisp-interaction-mode scheme-mode inferior-scheme-mode geiser-repl-mode lisp-mode eshell-mode slime-repl-mode cider-repl-mode nrepl-repl-mode clojure-mode common-lisp-mode)
   . sp--elisp-skip-match))))
 '(sp-override-key-bindings nil)
'(sp-sexp-prefix
(quote
 ((emacs-lisp-mode regexp "\\(?:,@\\|[',`]\\)")
  (latex-mode syntax "\\")
  (racket-mode regexp "#?['`,]@?"))))
'(sp-sexp-suffix
(quote
 ((ruby-mode syntax "")
  (json-mode regexp "")
  (python-mode regexp "")
  (emacs-lisp-mode regexp "'-?"))))
'(sp-show-enclosing-pair-commands
(quote
 (sp-show-enclosing-pair sp-forward-slurp-sexp sp-backward-slurp-sexp sp-forward-barf-sexp sp-backward-barf-sexp)))
 '(sp-show-pair-from-inside nil)
 '(sp-successive-kill-preserve-whitespace 2)
 '(sp-test-customize (quote ((interactive emacs-lisp-mode))))
 '(sp-undo-pairs-separately t)
 '(sp-use-subword t)
 '(sp-wrap-deactivate-smart-symbol-wrapping nil)
 '(sp-wrap-from-point nil)
 '(split-height-threshold 10)
 '(split-width-threshold 100)
'(sql-connection-alist
(quote
 (("nestle-test"
   (sql-product
    (quote mysql))
   (sql-user "root")
   (sql-server "tunel.logio.cz")
   (sql-port 30239)))))
 '(sql-pop-to-buffer-after-send-region nil)
'(stocklist-column-fontifiers
(quote
 (("payout" . stocklist--fontify-payout)
  ("yield" . stocklist--fontify-yield)
  ("eps" . stocklist--fontify-eps)
  ("pe" . stocklist--fontify-pe))))
 '(stocklist-default-sort (quote (yield . desc)))
'(stocklist-instruments
(quote
 (("AAPL" :tags
   ("tech" "growth" "owned"))
  ("KO" :tags
   ("food" "buffett" "king"))
  ("IBM" :tags
   ("tech" "buffett"))
  ("T" :tags
   ("telecom" "buffett" "tech")
   :signals
   ((ask < 34.5)))
  ("CLX")
  ("QCOM" :tags
   ("tech"))
  ("CVX" :tags
   ("oil"))
  ("PG" :tags
   ("retail" "buffett" "king"))
  ("WFC" :tags
   ("finance" "owned"))
  ("WMT" :tags
   ("retail" "buffett")
   :signals
   ((pe < 14)))
  ("EMR" :tags
   ("tech" "industry" "king")
   :signals
   ((pe < 15)
    (yield > 4)))
  ("F" :tags
   ("car"))
  ("O" :tags
   ("reit"))
  ("GM" :tags
   ("car" "owned"))
  ("LTC")
  ("JNJ" :tags
   ("healthcare" "buffett" "king"))
  ("MCD")
  ("TGT" :tags
   ("retail"))
  ("VZ" :tags
   ("telecom" "tech"))
  ("DE" :tags
   ("industry" "agro")
   :signals
   ((ask < 65.0)))
  ("UTX")
  ("AWR" :tags
   ("king" "industry"))
  ("CL" :tags
   ("healthcare" "king"))
  ("DOV" :tags
   ("king" "industry"))
  ("GPC" :tags
   ("king" "industry")
   :signals
   ((yield > 3)))
  ("LANC" :tags
   ("king" "food"))
  ("LOW" :tags
   ("king"))
  ("MMM" :tags
   ("industry" "king"))
  ("NDSN" :tags
   ("industry" "king"))
  ("PH" :tags
   ("king" "industry"))
  ("GD" :tags
   ("aerospace" "div"))
  ("ROP")
  ("DIS" :tags
   ("growth" "owned"))
  ("CINF" :tags
   ("king" "insurance" "finance"))
  ("AFL")
  ("INTC" :tags
   ("tech"))
  ("JWN" :tags
   ("owned" "growth" "retail"))
  ("HON" :tags
   ("industry" "aerospace"))
  ("RIO" :tags
   ("mining")
   :signals
   ((ask < 22)))
  ("TRP" :tags
   ("energy"))
  ("NWN" :tags
   ("gas" "energy" "king"))
  ("VVC" :tags
   ("energy" "king" "utility"))
  ("AXP" :tags
   ("finance" "buffett")
   :signals
   ((yield > 2.0)))
  ("UPS" :tags
   ("buffett" "div" "transport"))
  ("VLO" :tags
   ("oil")
   :signals
   ((ask < 58)
    (yield > 4)
    (pe < 7.5)))
  ("XOM" :tags
   ("oil")
   :signals
   ((ask < 77)))
  ("CVX" :tags
   ("oil")
   :signals
   ((yield > 5)))
  ("EOG" :tags
   ("oil")
   :signals
   ((ask < 64)))
  ("TDW" :tags
   ("oil")
   :signals
   ((ask < 5)))
  ("CLR" :tags
   ("oil")
   :signals
   ((ask < 24)))
  ("PSX" :tags
   ("oil")
   :signals
   ((ask < 80)))
  ("PXD" :tags
   ("oil")
   :signals
   ((ask < 130)))
  ("QEP" :tags
   ("oil")
   :signals
   ((ask < 10)))
  ("NMM" :face highlight :signals
   ((ask < 1.2)))
  ("TSLA" :tags
   ("tech"))
  ("FB" :tags
   ("tech" "growth")))))
'(stocklist-tag-to-face
(quote
 (("owned" . stocklist-owned)
  ("buffett" . my-stocklist-buffett))))
 '(system-time-locale "C" t)
 '(tab-always-indent (quote complete))
 '(texmathp-tex-commands (quote (("derivation" env-on))))
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(tracking-ignored-buffers (quote ("#openttd")))
 '(tramp-auto-save-directory "~/.emacs.d/tramp-autosave/")
 '(tramp-persistency-file-name nil)
 '(transient-mark-mode t)
 '(truncate-partial-width-windows nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(uniquify-strip-common-suffix t)
 '(url-queue-parallel-processes 5)
 '(url-queue-timeout 30)
 '(use-package-verbose t)
 '(user-full-name "Matúš Goljer")
 '(user-mail-address "matus.goljer@gmail.com")
 '(vc-make-backup-files t)
 '(vc-svn-diff-switches "-x -w")
 '(visible-bell nil)
 '(w3m-command nil)
 '(w3m-imagick-convert-program "c:\\cygwin\\bin\\convert.exe")
 '(which-func-maxout 100000)
 '(whitaker-program "cd /home/matus/dev/ada/whitakers-words/ && ./bin/words")
 '(windmove-wrap-around t)
 '(winner-mode t)
 '(x-select-enable-clipboard t))

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markup-header ((t (:inherit font-lock-function-name-face :background "#4e4e4e" :weight bold))) t)
 '(org-date ((t (:inherit fixed-pitch :foreground "#8cc4ff"))))
 '(org-level-1 ((((class color) (min-colors 65535)) :inherit outline-1) (((class color) (min-colors 256)) :inherit outline-1))))
