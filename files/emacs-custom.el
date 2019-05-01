;; custom variables
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
 '(abm-file "~/.emacs.d/.cache/autobookmarks/autobookmarks")
 '(abm-ignore-buffers
   (quote
    ("\\.ido\\.last" "\\.git" "\\.svn" "\\.log" "Maildir" "\\*message\\*" "\\.cask" "\\.avfs" "/tmp/crontab" "-autoloads\\.el$" "elfeed/index" "Org Src")))
 '(abm-old-bookmark-threshold 45)
 '(ag-highlight-search t)
 '(alert-default-style (quote libnotify))
 '(anzu-search-threshold 1000)
 '(appt-audible nil)
 '(appt-display-format nil)
 '(appt-display-interval 5)
 '(appt-message-warning-time 15)
 '(auto-save-file-name-transforms nil)
 '(auto-save-list-file-prefix "~/.emacs.d/.cache/auto-save-list/.saves-")
 '(autobookmarks-mode t)
 '(background-color "#002b36")
 '(background-mode dark)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/.cache/backups"))))
 '(bdf-directory-list (quote ("/usr/share/emacs/fonts/bdf")))
 '(bind-key-column-widths (quote (20 . 70)))
 '(bind-key-describe-special-forms t)
 '(bjump-dired-open-command (quote dired-open-file))
 '(blink-cursor-mode nil)
 '(blink-matching-paren nil)
 '(bookmark-default-file "~/.emacs.d/.cache/bookmarks/bookmarks")
 '(bookmark-version-control t)
 '(browse-kill-ring-quit-action (quote save-and-restore))
 '(calc-settings-file "~/.emacs.d/files/calc-settings.el")
 '(calc-undo-length 1000)
 '(calendar-latitude 49.2)
 '(calendar-longitude 16.633)
 '(calendar-mark-diary-entries-flag t)
 '(calendar-view-diary-initially-flag t)
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(company-backends
   (quote
    (company-omnisharp php-extras-company company-swb company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-cmake company-capf company-files
                       (company-dabbrev-code company-gtags company-etags company-keywords)
                       company-oddmuse company-dabbrev)))
 '(company-statistics-file "~/.emacs.d/.cache/company/company-statistics-cache.el")
 '(compilation-error-regexp-alist
   (quote
    (absoft ada aix ant bash borland python-tracebacks-and-caml comma cucumber msft edg-1 edg-2 epc ftnchek iar ibm irix java jikes-file maven jikes-line gcc-include ruby-Test::Unit gnu lcc makepp mips-1 mips-2 msft omake oracle perl php rxp sparc-pascal-file sparc-pascal-line sparc-pascal-example sun sun-ada watcom 4bsd gcov-file gcov-header gcov-nomark gcov-called-line gcov-never-called perl--Pod::Checker perl--Test perl--Test2 perl--Test::Harness weblint nette-tester)))
 '(compilation-read-command nil)
 '(compilation-scroll-output (quote first-error))
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
    ("22ff84fead97f05770fe28b52314221df14a43b0ffce5e5f9e331b5c5e45044d" "496c2c85f503ffed8081685d26653e48f211355325ecc181dee3293b0f4b501a" "1a02d018433b404de5cd48b696f2becaece559000268d7c385b9294378b476ef" "14e352ac2da10d5a6cdc6a74c1d34a8705f192f52f39886a9fff6d6255183471" "eac019f14171f37538b64b4bd37e8167702a63ec9dfce6b1e2fc991a617e3d81" "41ef21fb326cb60e26c21851eb2e06fdd65a2a0e40404a81166e3dae85107e9e" "66b84e676b3549d2108cc017bab900c763a2d4e025ffbc6203835a5d5154b6c5" "48a645c2d93a4a707cc1dc4f7859c266987b6cbd236827ab82a558d7fc61363d" "50f2c57b4309b4faa92392f02e912af829518a44b730a26652e864bee21935b5" "f2289b463590c0ecd2c4f80f06a96879a1f117eeb1fafaaeb11a8de127e5a905" "273b6863fc52b2a625fb2c1b640324996dc1eca3100aa83a749acb1e19ad77a2" "efddfa844e16b847af6c385f038db7743bc89401f26ad8eb513189f38da4b0c8" "f7bb90012e579862aed8e9463b62aeef7b766f47952d749f70c104dacc04c3c1" "40c228360e83cac0b5018d21fcc36fa01b69f3cb36bf96d4f81859a4b6d389f6" "27675ce1f696bc999ad213b347cc95466365573b1d03f824838f05b6bd88bc25" "71f3bb667b653f12e7e1631886a439a6bc53291dc39a665ff1e87fafabaa6629" "b59d50421840ea25d1ca038dfcb92f675200214c323f79fee2752a31c270d218" "85a33780b4db76e5b42f281a0b6a5106fdae544e55c382b68db2a78679c24c92" "ca101936b9943980660aa22d38be4db990a968d0ec315478ba06dd856f4fb17f" "6c0bca15239714172bf4772eb69f494b32b31dbfe42e65289ab4ed717207a603" "d162d8458661f0033ccb41806082360db0460079108dded068c29556565ba223" "5b0c30d399c03b761b319d092e050859a6d58a76fa401525368ee9f426a665a7" "1ee0d1b3c0b58b69a60ca698c2f4f76322db67c23e6a44eb199a985f7cef204d" "bba45d4eb89b3c8493fe6d3076623f2d2f89afbdbe32928d0c0bcb5c334ae90b" "7037a4e8db7ec508773a0abf6c150b6c0d18d23ab77a2ab294ac1bb19d5971e4" default)))
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(custom-unlispify-remove-prefixes t)
 '(custom-unlispify-tag-names nil)
 '(dash-enable-fontlock t)
 '(debug-on-error t)
 '(default-input-method "english-prog")
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
      (extension "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
     ("documents"
      (extension "doc" "docx" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub")))))
 '(dired-guess-shell-alist-user
   (quote
    (("\\.\\(?:MP[34]\\|avi\\|flv\\|m\\(?:kv\\|ov\\|p[34g]\\)\\|ogg\\|wm[av]\\)\\'" "vlc")
     ("\\.\\(?:djvu\\|p\\(?:df\\|s\\)\\)\\'" "zathura")
     ("\\.fb2" "fbreader")
     ("\\.html\\'" "google-chrome"))))
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
 '(dired-open-remote-file-regex-list (quote ("\\.html\\'")))
 '(dired-open-use-nohup t)
 '(diredfl-ignore-compressed-flag nil)
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
 '(docker-tramp-use-names t)
 '(echo-keystrokes 0.1)
 '(ediff-custom-diff-options "-u")
 '(ediff-diff-options "-w")
 '(ediff-merge-split-window-function (quote split-window-horizontally))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(editorconfig-mode t)
 '(elfeed-db-directory "~/.emacs.d/elfeed")
 '(elfeed-max-connections 5)
 '(elfeed-search-face-alist
   (quote
    ((unread elfeed-search-unread-title-face)
     (tumblr font-lock-constant-face))))
 '(elfeed-search-title-max-width 90)
 '(elfeed-use-curl t)
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults)))
 '(elpy-rpc-backend "jedi")
 '(elpy-rpc-python-command "python3")
 '(emmet-indentation 2)
 '(emmet-preview-default nil)
 '(enable-recursive-minibuffers t)
 '(enable-remote-dir-locals t)
 '(epg-gpg-program "gpg2")
 '(eshell-aliases-file "/home/matus/.emacs.d/etc/eshell/alias")
 '(eshell-directory-name "~/.emacs.d/.cache/eshell/")
 '(eshell-history-file-name "/home/matus/.emacs.d/.cache/eshell/history")
 '(eshell-last-dir-ring-file-name "/home/matus/.emacs.d/.cache/eshell/lastdir")
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
 '(exec-path-from-shell-shell-name "fish")
 '(explicit-shell-file-name "/usr/bin/zsh")
 '(eyebrowse-mode t)
 '(find-grep-options "-qE")
 '(flycheck-disabled-checkers (quote (php-phplint php-phpmd emacs-lisp-checkdoc)))
 '(flycheck-emacs-lisp-load-path (quote inherit))
 '(flycheck-gcc-language-standard "c++14")
 '(flycheck-ghc-args nil)
 '(flycheck-ghc-search-path
   (quote
    ("/home/matus/dotfiles/xmonad/.xmonad/lib" "/home/matus/dev/haskell/mpris/src/")))
 '(flycheck-gnat-include-path (quote ("/home/matus/dev/ada/whitakers-words/src")))
 '(flycheck-php-phpcs-executable "~/.config/composer/vendor/bin/phpcs")
 '(flycheck-phpmd-rulesets (quote ("codesize" "controversial" "design")))
 '(fold-dwim-outline-style-default (quote nested))
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
 '(geben-display-window-function (quote display-buffer))
 '(geben-temporary-file-directory "/home/matus/.emacs.d/.cache/geben/")
 '(global-flex-isearch-mode t)
 '(global-paren-face-mode t)
 '(global-subword-mode t)
 '(global-undo-tree-mode t)
 '(gnus-alias-default-identity "goljer")
 '(gnus-alias-identity-alist
   (quote
    (("goljer" "" "Matúš Goljer <matus.goljer@gmail.com>" ""
      (("Fcc" lambda nil "/home/matus/Maildir/Goljer/sent"))
      "" "~/.emacs.d/.signature")
     ("dota" "" "Matúš Goljer <dota.keys@gmail.com>" ""
      (("Fcc" lambda nil "/home/matus/Maildir/Dota/sent"))
      "" "~/.emacs.d/.signature")
     ("logio" "" "Matúš Goljer <goljer@logio.cz>" ""
      (("Fcc" lambda nil "/home/matus/Maildir/Logio/sent"))
      "" "~/.emacs.d/.signature-logio"))))
 '(gnus-alias-identity-rules
   (quote
    (("logio-to-header"
      ("to" ".*logio.*" both)
      "logio"))))
 '(god-mod-alist (quote ((nil . "C-") ("g" . "M-") ("i" . "C-M-"))))
 '(google-this-keybind "\"g\"")
 '(gud-key-prefix "")
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
 '(ido-cr+-max-items 300000)
 '(ido-save-directory-list-file "~/.emacs.d/.cache/ido/.ido.last")
 '(ido-ubiquitous-allow-on-functional-collection t)
 '(ido-ubiquitous-command-overrides
   (quote
    ((disable exact "execute-extended-command")
     (enable prefix "wl-")
     (enable-old prefix "Info-")
     (enable exact "webjump")
     (enable regexp "\\`\\(find\\|load\\|locate\\)-library\\'")
     (disable prefix "tmm-")
     (enable regexp "\\`\\(load\\|enable\\|disable\\|describe\\|custom-theme-visit\\)-theme\\'")
     (enable-old prefix "bbdb-")
     (enable-old exact "where-is")
     (disable exact "todo-add-category")
     (enable exact "find-tag")
     (enable prefix "etags-select-"))))
 '(ido-ubiquitous-default-state (quote enable))
 '(ido-ubiquitous-function-overrides
   (quote
    ((disable exact "read-file-name")
     (disable exact "read-file-name-internal")
     (disable exact "read-buffer")
     (disable exact "gnus-emacs-completing-read")
     (disable exact "gnus-iswitchb-completing-read")
     (disable exact "grep-read-files")
     (disable exact "magit-builtin-completing-read")
     (enable exact "bookmark-completing-read")
     (enable-old exact "webjump-read-choice")
     (enable-old exact "webjump-read-url-choice")
     (disable exact "isearchp-read-unicode-char")
     (enable exact "read-char-by-name")
     (disable exact "Info-read-node-name")
     (disable exact "tmm-menubar")
     (enable exact "imenu--completion-buffer")
     (enable-old exact "auto-insert")
     (enable exact "project--completing-read-strict")
     (enable-old exact "legalese-elisp-keyword"))))
 '(imenu-auto-rescan t)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines nil)
 '(inferior-ess-r-program "R")
 '(inhibit-startup-screen t)
 '(initial-buffer-choice (quote my-startup-screen))
 '(initial-major-mode (quote fundamental-mode))
 '(ispell-program-name "hunspell")
 '(ivy-height 30)
 '(js2-strict-missing-semi-warning nil)
 '(js2-strict-trailing-comma-warning nil)
 '(jump-char-forward-key "m")
 '(keyfreq-autosave-mode t)
 '(keyfreq-file "~/.emacs.d/.cache/keyfreq/.emacs.keyfreq")
 '(keyfreq-file-lock "~/.emacs.d/.cache/keyfreq/.emacs.keyfreq.lock")
 '(keyfreq-mode t)
 '(ledger-reconcile-default-commodity "Kc")
 '(ledger-reports
   (quote
    (("portfolio" "ledger -f %(ledger-file) bal --group-by commodity --group-title-format '%-40(total_expr)' -n Assets:Broker -X Kc | grep -v '^$'")
     ("expenses" "ledger -f %(ledger-file) -p \"this month\" -X Kc --monthly --real reg ^expenses")
     ("expenses-year" "ledger -f %(ledger-file) -p \"this year\" -X Kc --monthly --real reg ^expenses")
     ("cash-flow-monthly" "ledger -f %(ledger-file) -p \"this month\" bal -X Kc ^income ^expenses")
     ("cash-flow" "ledger -f %(ledger-file) bal -X Kc ^income ^expenses")
     ("budget" "ledger -f %(ledger-file) bal -X Kc '^Assets:Checking:Air Bank'")
     ("expenses-budget" "ledger -f %(ledger-file) -p \"this month\" -X Kc --monthly reg 'Assets:Checking:Air Bank:Budget:Expenses'")
     ("expenses-budget-year" "ledger -f %(ledger-file) -p \"this year\" -X Kc --monthly reg 'Assets:Checking:Air Bank:Budget:Expenses'")
     ("equity" "ledger -f %(ledger-file) bal -X Kc ^assets ^liabilities")
     ("bal" "ledger -f %(ledger-file) -X Kc bal")
     ("reg" "ledger -f %(ledger-file) reg")
     ("payee" "ledger -f %(ledger-file) reg @%(payee)")
     ("reg-account" "ledger -f %(ledger-file) reg %(account)")
     ("bal-account" "ledger -f %(ledger-file) -X Kc bal %(account)"))))
 '(legalese-date-format (quote ordinal))
 '(legalese-default-copyright "Matúš Goljer")
 '(legalese-templates
   (quote
    ((emacs-lisp-mode
      (nil ";;; " legalese-file-name " --- " _ " -*- lexical-binding: t -*-
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
 '(litable-list-file "~/.emacs.d/.cache/litable/.litable-lists.el")
 '(log-edit-hook
   (quote
    (log-edit-insert-cvs-template log-edit-insert-changelog log-edit-show-files)))
 '(look-show-subdirs t)
 '(ls-lisp-dirs-first t)
 '(ls-lisp-use-insert-directory-program nil)
 '(ls-lisp-use-localized-time-format t)
 '(ls-lisp-use-string-collate nil)
 '(ls-lisp-verbosity (quote (uid gid)))
 '(lsp-inhibit-message t)
 '(lsp-ui-flycheck-enable nil)
 '(magit-auto-revert-mode nil)
 '(magit-bury-buffer-function (quote quit-window))
 '(magit-commit-arguments nil)
 '(magit-completing-read-function (quote ido-completing-read))
 '(magit-diff-arguments (quote ("--no-ext-diff" "--stat")))
 '(magit-diff-refine-hunk (quote all))
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate")))
 '(magit-push-always-verify nil)
 '(magit-status-sections-hook
   (quote
    (magit-insert-status-headers magit-insert-merge-log magit-insert-rebase-sequence magit-insert-am-sequence magit-insert-sequencer-sequence magit-insert-bisect-output magit-insert-bisect-rest magit-insert-bisect-log magit-insert-untracked-files magit-insert-unstaged-changes magit-insert-staged-changes magit-insert-stashes magit-insert-unpushed-to-pushremote magit-insert-unpushed-to-upstream magit-insert-unpulled-from-pushremote magit-insert-unpulled-from-upstream magit-insert-modules)))
 '(magit-tag-arguments (quote ("--sort=v:refname")))
 '(mail-envelope-from (quote header))
 '(mail-specify-envelope-from t)
 '(make-pointer-invisible t)
 '(markdown-fontify-code-blocks-natively t)
 '(markdown-link-space-sub-char "-")
 '(max-lisp-eval-depth 1000)
 '(max-specpdl-size 10000)
 '(mc/list-file "~/.emacs.d/.cache/multiple-cursors/.mc-lists.el")
 '(message-forward-as-mime nil)
 '(message-kill-buffer-on-exit t)
 '(message-log-max 10000)
 '(message-send-mail-function (quote smtpmail-send-it))
 '(message-sendmail-envelope-from (quote header))
 '(mis-bindings-alist nil)
 '(mis-make-command "make -j2")
 '(mis-recipes-directory "~/.emacs.d/elpa/make-it-so-20141203.811/recipes/")
 '(mm-inline-large-images (quote resize))
 '(mocha-reporter "spec")
 '(mouse-highlight nil)
 '(mouse-wheel-progressive-speed nil)
 '(my-org-idle-task "bed6e54e-095d-477e-92c0-9a4e6f345284")
 '(notmuch-archive-tags (quote ("-inbox" "-unread" "+archived")))
 '(notmuch-fcc-dirs nil)
 '(notmuch-hello-sections
   (quote
    (notmuch-hello-insert-header notmuch-hello-insert-search notmuch-hello-insert-recent-searches notmuch-hello-insert-alltags notmuch-hello-insert-footer)))
 '(notmuch-search-oldest-first nil)
 '(notmuch-show-logo nil)
 '(nov-save-place-file "~/.emacs.d/.cache/nov/nov-places")
 '(omnisharp-server-executable-path
   "/home/matus/sources/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe")
 '(org-M-RET-may-split-line nil)
 '(org-adapt-indentation nil)
 '(org-agenda-clock-consistency-checks
   (quote
    (:max-duration "10:00" :min-duration 0 :max-gap "0:20" :gap-ok-around
     ("4:00")
     :default-face
     ((:foreground "Red"))
     :overlap-face nil :gap-face nil :no-end-time-face nil :long-face nil :short-face nil)))
 '(org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 3 :tcolumns 1)))
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
    ((daily weekly today require-timed remove-match)
     (800 1000 1200 1400 1600 1800 2000 2200)
     "......" "----------------")))
 '(org-agenda-todo-ignore-scheduled (quote all))
 '(org-agenda-window-setup (quote current-window))
 '(org-attach-auto-tag "ATTACH")
 '(org-attach-commit nil)
 '(org-attach-directory "/home/matus/data/org-attach/")
 '(org-attach-git-annex-cutoff 1)
 '(org-babel-load-languages
   (quote
    ((emacs-lisp . t)
     (plantuml . t)
     (shell . t)
     (ledger . t)
     (gnuplot . t)
     (awk . t)
     (sql . t)
     (js . t)
     (ruby . t)
     (R . t)
     (dot . t))))
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
 '(org-clock-persist-file "~/.emacs.d/.cache/org/org-clock-save.el")
 '(org-clock-persist-query-resume nil)
 '(org-clock-report-include-clocking-task t)
 '(org-columns-default-format
   "%50ITEM(Task) %TODO(Todo) %SCHEDULED(Scheduled) %DEADLINE(Deadline) %TAGS(Tags) %CLOCKSUM(Clock) %Effort(Effort) %BUDGET_YEAR(B/Y) %BUDGET_WEEK(B/W)")
 '(org-complete-tags-always-offer-all-agenda-tags t)
 '(org-completion-use-ido t)
 '(org-confirm-babel-evaluate nil)
 '(org-contacts-files (quote ("~/org/contacts.org")))
 '(org-cycle-emulate-tab nil)
 '(org-deadline-warning-days 30)
 '(org-default-notes-file "~/org/refile.org")
 '(org-default-priority 67)
 '(org-drill-add-random-noise-to-intervals-p t)
 '(org-drill-adjust-intervals-for-early-and-late-repetitions-p t)
 '(org-drill-card-type-alist
   (quote
    ((nil org-drill-present-simple-card nil t)
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
 '(org-drill-maximum-items-per-session 40)
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
 '(org-duration-format (quote ((special . h:mm))))
 '(org-email-link-description-format "%f: %.100s")
 '(org-emphasis-alist
   (quote
    (("*" markup-bold)
     ("/" markup-italic)
     ("_" underline)
     ("=" markup-inline-code verbatim)
     ("~" markup-inline-code verbatim)
     ("+" markup-strike))))
 '(org-export-allow-bind-keywords t)
 '(org-extend-today-until 4)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "zathura %s"))))
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-fontify-whole-heading-line t)
 '(org-footnote-auto-label (quote random))
 '(org-footnote-define-inline t)
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 3.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
     ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-gcal-dir "~/.emacs.d/.cache/org-gcal/")
 '(org-habit-following-days 1)
 '(org-habit-graph-column 80)
 '(org-habit-preceding-days 30)
 '(org-habit-show-habits-only-for-today nil)
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars t)
 '(org-id-locations-file "~/.emacs.d/.cache/org/.org-id-locations")
 '(org-indent-indentation-per-level 1)
 '(org-inline-image-resolve-url (quote (org-inline-image--regexp-resolver identity)))
 '(org-jira-default-jql
   "assignee = currentUser() and resolution = unresolved and \"In active sprint\" = Yes ORDER BY
  priority DESC, created ASC")
 '(org-jira-done-states (quote ("Declined" "Done")))
 '(org-jira-keywords-to-jira-status-alist
   (quote
    (("In Progress" . "NEXT")
     ("Feedback Needed" . "WAIT")
     ("Testing needed" . "WAIT")
     ("New" . "IDEA"))) t)
 '(org-jira-priority-to-org-priority-alist
   (quote
    (("Highest" . 65)
     ("High" . 66)
     ("Medium" . 67)
     ("Low" . 68)
     ("Lowest" . 69))))
 '(org-jira-priority-to-org-priority-omit-default-priority t)
 '(org-jira-project-filename-alist (quote (("SC" . "saleschamp.org"))))
 '(org-jira-reverse-comment-order t)
 '(org-jira-working-dir "~/org")
 '(org-latex-tables-centered nil)
 '(org-log-done (quote note))
 '(org-log-into-drawer t)
 '(org-lowest-priority 69)
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-crypt org-gnus org-habit org-id org-info org-inlinetask org-checklist org-depend org-drill)))
 '(org-outline-path-complete-in-steps nil)
 '(org-plantuml-jar-path "~/bin/plantuml.jar")
 '(org-priority-start-cycle-with-default nil)
 '(org-publish-timestamp-directory "~/.emacs.d/.cache/org-timestamps/")
 '(org-radiobutton-mode t t (org-radiobutton))
 '(org-refile-allow-creating-parent-nodes (quote confirm))
 '(org-refile-target-verify-function (quote my-org-verify-refile-target))
 '(org-refile-targets
   (quote
    ((nil :maxlevel . 9)
     (org-agenda-files :maxlevel . 9))))
 '(org-refile-use-cache t)
 '(org-refile-use-outline-path (quote file))
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
 '(org-src-window-setup (quote current-window))
 '(org-stuck-projects (quote ("" nil nil "")))
 '(org-super-agenda-unmatched-order 10)
 '(org-support-shift-select t)
 '(org-tags-column -80)
 '(org-tags-exclude-from-inheritance (quote ("folder" "publish")))
 '(org-tags-sort-function (quote string<))
 '(org-time-clocksum-format
   (quote
    (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))
 '(org-toggl-inherit-toggl-properties t)
 '(org-tree-slide-slide-in-effect nil)
 '(org-use-fast-tag-selection t)
 '(org-use-speed-commands t)
 '(package-enable-at-startup nil)
 '(package-selected-packages
   (quote
    (diredfl systemd org-noter org-make-toc company-lsp pomidor smart-jump lsp-ui lsp-mode js2-refactor fish-completion impatient-mode docker-tramp nov magit-annex restclient web-server org-super-agenda camcorder scala-mode cmake-mode evil pdf-tools org-brain yaml-mode world-time-mode which-key wgrep-ag web-mode visual-regexp use-package undo-tree undercover tuareg transpose-frame string-edit smex smartscan smartparens shell-pop shackle scf-mode rainbow-mode puppet-mode pos-tip php-refactor-mode php-mode php-eldoc php-boris-minor-mode password-generator parse-csv paren-face pandoc-mode pallet package-lint overseer ov outshine org-plus-contrib omnisharp ob-elixir notmuch nginx-mode multiple-cursors markdown-toc markdown-mode+ make-it-so magit macrostep legalese ledger-mode keyfreq jump-char json-mode js2-mode jdee ido-ubiquitous ibuffer-vc htmlize highlight-thing helm-gtags helm-descbinds gnus-alias gnuplot glsl-mode gitignore-mode flycheck-ledger flycheck-haskell flycheck-elixir flycheck-credo flycheck-cask flx-ido fish-mode firestarter expand-region exec-path-from-shell ess emr emmet-mode elpy elfeed eldoc-eval eimp editorconfig dockerfile-mode dired+ czech-holidays conkeror-minor-mode concurrent company-statistics clojure-mode cask-mode buttercup browse-kill-ring browse-at-remote auctex assess alchemist ag)))
 '(paren-face-regexp "[(){}]")
 '(pj-line-width 1100)
 '(preview-scale-function 1.5)
 '(proced-auto-update-flag t)
 '(proced-auto-update-interval 1)
 '(projectile-cache-file "/home/matus/.emacs.d/.cache/projectile/projectile.cache")
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" "elpa")))
 '(projectile-known-projects-file
   "/home/matus/.emacs.d/.cache/projectile/projectile-bookmarks.eld")
 '(projectile-project-root-files
   (quote
    (".dir-locals.el" ".projectile" ".git" ".hg" ".fslckout" ".bzr" "_darcs" "rebar.config" "project.clj" "pom.xml" "build.sbt" "Gemfile" "Makefile")))
 '(projectile-switch-project-action (lambda nil (dired default-directory)))
 '(proof-three-window-mode-policy (quote hybrid))
 '(rainbow-r-colors t)
 '(recentf-save-file "~/.emacs.d/.cache/recentf/recentf")
 '(reftex-label-alist
   (quote
    (("lemma" 32 "lem:" "~\\cref{%s}" nil nil)
     ("theorem" 32 "th:" "~\\cref{%s}" nil nil))))
 '(require-final-newline t)
 '(restclient-inhibit-cookies t)
 '(safe-local-variable-values
   (quote
    ((org-noter-default-notes-file-names "forecasting.org")
     (org-noter-notes-search-path "~/data/research/logistics/")
     (org-noter-notes-search-path . "~/data/research/logistics/")
     (flycheck-php-phpstan-executable . "/home/matus/dev/php/Sandbox/core/tests/phpstan/docker-phpstan")
     (ft-source-to-test-mapping
      (:path "Elsa/" :prefix "elsa")
      :path "Elsa/tests/" :prefix "test")
     (ft-source-to-test-mapping
      (:path "" :prefix "elsa")
      :path "/tests/" :prefix "test")
     (flycheck-php-phpstan-executable . "/home/matus/dev/php/Sandbox/core/tests/phpstan/docker-phpstan")
     (flycheck-phpstan-autoload . "/var/www/html/core/vendor/autoload.php")
     (flycheck-phpstan-config . "/var/www/html/core/tests/phpstan/phpstan.neon")
     (my-flycheck-get-php-source . my-pw-to-docker)
     (flycheck-php-phpstan-executable . "/home/matus/dev/php/Sandbox/core/docker-phpstan")
     (eval ispell-change-dictionary "slovak")
     (haskell-program-name . "stack exec -- ghci +RTS -M300m")
     (eval setq compile-command
           (concat "make -C "
                   (locate-dominating-file
                    (buffer-file-name)
                    ".travis.yml")))
     (eval setq compile-command
           (concat "make -C "
                   (locate-dominating-file
                    (buffer-file-name)
                    ".travis.yml")
                   " -j2"))
     (org-refile-targets
      (nil :maxlevel . 9))
     (eval font-lock-add-keywords nil
           (-map
            (-lambda
              ((face . color))
              (let
                  ((pattern
                    (concat "\\_<"
                            (symbol-name face)
                            "\\_>"))
                   (color-resolved
                    (if
                        (stringp color)
                        color
                      (cdr
                       (assq color
                             (cdar my-tango-colors))))))
                (list pattern 0
                      (\`
                       (rainbow-colorize-match
                        (\, color-resolved))))))
            (cdar my-tango-colors)))
     (eval set-input-method "slovak-prog-2")
     (default-input-method . "slovak-prog-2")
     (flycheck-php-phpstan-executable . "/home/matus/dev/salesforce-proxy/docker-phpstan")
     (flycheck-php-phpstan-executable . "/home/matus/dev/application/docker-phpstan")
     (eval when
           (and
            (buffer-file-name)
            (file-regular-p
             (buffer-file-name))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
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
     (swb-database . "ahold_ok")
     (swb-port . "30245")
     (swb-database . "nestle_live")
     (swb-port . "30238")
     (swb-host . "tunel.logio.cz")
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
 '(sallet-buffer-sources
   (quote
    (sallet-source-buffer sallet-source-similar-buffer sallet-source-autobookmarks sallet-source-bookmarks sallet-source-gtags-files sallet-source-projectile-projects sallet-source-locate)))
 '(save-place-file "~/.emacs.d/.cache/save-place/.emacs-places")
 '(save-place-limit nil)
 '(save-place-mode t nil (saveplace))
 '(savehist-file "~/.emacs.d/.cache/savehist/history")
 '(savehist-mode t)
 '(scroll-preserve-screen-position 1)
 '(select-enable-clipboard t)
 '(semantic-default-submodes
   (quote
    (global-semantic-idle-scheduler-mode global-semanticdb-minor-mode global-semantic-idle-summary-mode global-semantic-mru-bookmark-mode)))
 '(semantic-new-buffer-setup-functions (quote ((java-mode . wisent-malabar-java-setup))))
 '(semanticdb-default-save-directory "~/.emacs.d/.cache/semanticdb/")
 '(send-mail-function (quote smtpmail-send-it))
 '(server-auth-dir "~/.emacs.d/.cache/server/")
 '(shackle-mode t)
 '(shackle-rules
   (quote
    (("*Help*" :select t)
     ("[0-9]\\{5\\}/.*\\.php" :regexp t :select t :inhibit-window-quit t :same t :popup nil)
     ("magit-log\\(-popup\\)?" :regexp t :select t :inhibit-window-quit t :same t)
     ("\\`\\*?magit:" :regexp t :select t :inhibit-window-quit t :same t)
     (Man-mode :select t :inhibit-window-quit t :same t)
     ("\\*ag search" :regexp t :inhibit-window-quit t :same t)
     (messages-buffer-mode :select t :inhibit-window-quit t :same t)
     (swb-result-mode :align
       (quote below))
     ("\\*prodigy" :regexp t :select t :inhibit-window-quit t :same t))))
 '(shell-file-name "/bin/bash")
 '(shell-pop-restore-window-configuration nil)
 '(shell-pop-window-size 50)
 '(show-smartparens-global-mode t)
 '(shr-max-image-proportion 0.9)
 '(smart-jump-bind-keys nil)
 '(smerge-command-prefix "\\C-.m")
 '(smex-save-file "~/.emacs.d/.cache/smex/.smex-items")
 '(smtpmail-default-smtp-server "smtp.gmail.com")
 '(smtpmail-local-domain "herakleitos")
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 465)
 '(smtpmail-stream-type (quote ssl))
 '(sp-autoinsert-if-followed-by-same 3)
 '(sp-autoinsert-quote-if-followed-by-closing-pair nil)
 '(sp-autoskip-closing-pair (quote always))
 '(sp-autoskip-opening-pair nil)
 '(sp-autowrap-region t)
 '(sp-comment-string (quote (((emacs-lisp-mode) . ";; "))))
 '(sp-hybrid-kill-excessive-whitespace nil)
 '(sp-ignore-modes-list
   (quote
    (image-dired-display-image-mode image-dired-thumbnail-mode ediff-mode recentf-dialog-mode google-maps-static-mode ibuffer-mode org-agenda-mode dired-mode elfeed-search-mode elfeed-show-mode notmuch-search-mode notmuch-show-mode notmuch-hello-mode)))
 '(sp-navigate-close-if-unbalanced t)
 '(sp-navigate-comments-as-sexps t)
 '(sp-navigate-consider-sgml-tags (quote (html-mode markdown-mode gfm-mode rst-mode)))
 '(sp-navigate-consider-stringlike-sexp
   (quote
    (ruby-mode gfm-mode emacs-lisp-mode html-mode org-mode python-mode text-mode latex-mode)))
 '(sp-navigate-consider-symbols t)
 '(sp-navigate-interactive-always-progress-point t)
 '(sp-navigate-reindent-after-up
   (quote
    ((interactive emacs-lisp-mode clojure-mode clojurescript-mode))))
 '(sp-navigate-skip-match
   (quote
    (((ruby-mode enh-ruby-mode)
      . sp--ruby-skip-match)
     ((emacs-lisp-mode inferior-emacs-lisp-mode lisp-interaction-mode scheme-mode inferior-scheme-mode geiser-repl-mode lisp-mode eshell-mode slime-repl-mode cider-repl-mode nrepl-repl-mode clojure-mode common-lisp-mode)
      . sp--elisp-skip-match))))
 '(sp-override-key-bindings nil)
 '(sp-sexp-prefix
   (quote
    ((emacs-lisp-mode regexp "\\(?:['`]*,@?\\|[',`]\\)")
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
 '(sql-pop-to-buffer-after-send-region nil)
 '(stocklist-column-fontifiers
   (quote
    (("payout" . stocklist--fontify-payout)
     ("yield" . stocklist--fontify-yield)
     ("eps" . stocklist--fontify-eps)
     ("pe" . stocklist--fontify-pe)
     ("ask" . stocklist--fontify-ask))))
 '(stocklist-default-sort (quote (yield . desc)))
 '(stocklist-instruments
   (quote
    (("AAPL" :tags
      ("tech" "growth"))
     ("MSFT" :tags
      ("tech" "growth")
      :signals
      ((yield > 3)))
     ("KO" :tags
      ("food" "buffett" "king"))
     ("CSCO" :tags
      ("tech"))
     ("IBM" :tags
      ("tech" "buffett")
      :signals
      ((yield > 4)))
     ("T" :tags
      ("owned" "telecom" "buffett" "tech")
      :signals
      ((ask < 34.5)))
     ("QCOM" :tags
      ("tech" "div-growth")
      :signals
      ((ask < 46.5)))
     ("PG" :tags
      ("retail" "buffett" "king"))
     ("WFC" :tags
      ("finance"))
     ("WMT" :tags
      ("retail" "buffett")
      :signals
      ((pe < 14)))
     ("EMR" :tags
      ("tech" "industry" "king" "5star")
      :signals
      ((pe < 15)
       (yield > 4)
       (ask < 47)))
     ("F" :tags
      ("car"))
     ("O" :tags
      ("reit")
      :signals
      ((ask < 48)))
     ("STAG" :tags
      ("reit")
      :signals
      ((yield > 8)))
     ("GM" :tags
      ("car"))
     ("LTC" :signals
      ((pe < 22)))
     ("JNJ" :tags
      ("healthcare" "buffett" "king"))
     ("MCD")
     ("TGT" :tags
      ("retail")
      :signals
      ((close < 50.0)))
     ("VZ" :tags
      ("telecom" "tech"))
     ("CAT" :tags
      ("industry" "agro" "5star"))
     ("DE" :tags
      ("industry" "agro" "5star" "div-growth"))
     ("UTX")
     ("AWR" :tags
      ("king" "industry")
      :signals
      ((close < 40.0)))
     ("CL" :tags
      ("healthcare" "king"))
     ("DOV" :tags
      ("king" "industry"))
     ("GPC" :tags
      ("owned" "king" "industry")
      :signals
      ((yield > 3)))
     ("LOW" :tags
      ("king"))
     ("MMM" :tags
      ("industry" "king"))
     ("NDSN" :tags
      ("industry" "king"))
     ("PH" :tags
      ("king" "industry")
      :signals
      ((yield > 2.5)))
     ("GD" :tags
      ("aerospace" "div"))
     ("DIS" :tags
      ("growth" "owned"))
     ("CINF" :tags
      ("king" "insurance" "finance"))
     ("INTC" :tags
      ("tech"))
     ("HON" :tags
      ("industry" "aerospace"))
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
      ("oil" "div-growth"))
     ("XOM" :tags
      ("oil")
      :signals
      ((ask < 77)))
     ("CVX" :tags
      ("oil")
      :signals
      ((yield > 5)))
     ("EOG" :tags
      ("oil"))
     ("CLR" :tags
      ("oil")
      :signals
      ((ask < 24)))
     ("PSX" :tags
      ("oil" "buffett")
      :signals
      ((ask < 75.01)))
     ("TSLA" :tags
      ("tech"))
     ("FB" :tags
      ("tech" "growth"))
     ("HRB" :tags
      ("finance")
      :signals
      ((close < 24)))
     ("PSEC" :tags
      ("sec" "income")
      :signals
      ((yield > 13.999)))
     ("OKE" :tags
      ("income" "gas" "energy"))
     ("CMI" :tags
      ("industry" "div" "5star" "div-growth")
      :signals
      ((ask < 105))))))
 '(stocklist-tag-to-face
   (quote
    (("owned" . stocklist-owned)
     ("buffett" . my-stocklist-buffett)
     ("div-growth" . my-stocklist-div-growth))))
 '(system-time-locale "C" t)
 '(tab-always-indent (quote complete))
 '(texmathp-tex-commands (quote (("derivation" env-on))))
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(tracking-ignored-buffers (quote ("#openttd")))
 '(tramp-auto-save-directory "~/.emacs.d/.cache/tramp/autosave/" nil (tramp))
 '(tramp-persistency-file-name nil nil (tramp))
 '(transient-mark-mode t)
 '(truncate-partial-width-windows nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(uniquify-strip-common-suffix t)
 '(url-configuration-directory "~/.emacs.d/.cache/url/")
 '(url-queue-parallel-processes 5)
 '(url-queue-timeout 30)
 '(use-package-verbose t)
 '(user-full-name "Matúš Goljer")
 '(user-mail-address "matus.goljer@gmail.com")
 '(vc-follow-symlinks t)
 '(vc-make-backup-files t)
 '(vc-svn-diff-switches "-x -w")
 '(visible-bell nil)
 '(which-func-maxout 100000)
 '(which-function-mode t)
 '(which-key-mode t)
 '(whitaker-program "cd /home/matus/dev/ada/whitakers-words/ && ./bin/words")
 '(whitespace-style
   (quote
    (face trailing tabs spaces lines-tail newline empty indentation space-after-tab space-before-tab space-mark tab-mark newline-mark)))
 '(windmove-wrap-around t)
 '(winner-mode t)
 '(xref-prompt-for-identifier
   (quote
    (not xref-find-references xref-find-definitions xref-find-definitions-other-window xref-find-definitions-other-frame)))
 '(yas-prompt-functions (quote (yas-ido-prompt)))
 '(yas-snippet-dirs (quote ("~/.emacs.d/etc/yasnippet/snippets"))))

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
 '(stocklist-watched-cell ((t (:background "dim gray"))))
 '(web-mode-block-control-face ((t (:inherit font-lock-keyword-face)))))
