;;; my-tango-dark-theme.el --- Tango-based custom theme for faces

(deftheme my-tango-dark
  "Face colors using the Tango palette (dark background).  This
is extension of default `tango-dark' face.")

(let ((class '((class color) (min-colors 89)))
      ;; Tango palette colors.
      (butter-1 "#fce94f") (butter-2 "#edd400") (butter-3 "#c4a000")
      (orange-1 "#fcaf3e") (orange-2 "#f57900") (orange-3 "#ce5c00")
      (choc-1 "#e9b96e") (choc-2 "#c17d11") (choc-3 "#8f5902")
      (cham-1 "#8ae234") (cham-2 "#73d216") (cham-3 "#4e9a06")
      (blue-1 "#729fcf") (blue-2 "#3465a4") (blue-3 "#204a87")
      (plum-1 "#ad7fa8") (plum-2 "#75507b") (plum-3 "#5c3566")
      (red-1 "#ef2929")  (red-2 "#cc0000")  (red-3 "#a40000")
      (alum-1 "#eeeeec") (alum-2 "#d3d7cf") (alum-3 "#babdb6")
      (alum-4 "#888a85") (alum-5 "#555753") (alum-6 "#2e3436")
      ;; Not in Tango palette; used for better contrast.
      (cham-0 "#b4fa70") (blue-0 "#8cc4ff") (plum-0 "#e6a8df")
      (red-0 "#ff4b4b")  (alum-5.5 "#41423f") (alum-7 "#212526")
      (fixed-sys (if (eq system-type 'windows-nt) "Consolas" "Inconsolata"))
      (fixed-sys-height (if (eq system-type 'windows-nt) 100 120))
      (variable-font (if (eq system-type 'windows-nt) "Verdana" "Droid Sans")))

  (custom-theme-set-faces
   'my-tango-dark
   ;; Ensure sufficient contrast on low-color terminals.
   `(default ((,class (:foreground ,alum-1
                       :background ,alum-6
                       :family ,fixed-sys
                       :foundry "outline"
                       :height ,fixed-sys-height))))
   `(cursor ((,class (:background ,butter-1))))
   ;; Highlighting faces
   `(fringe ((,class (:background ,alum-7))))
   `(highlight ((,class (:foreground ,alum-6 :background ,butter-2))))
   ;;`(fringe ((,class (:background "grey95" :foreground "black")))) ;; my custom?
   ;;`(highlight ((,class (:background "darkseagreen2" :foreground "black")))) ;; my custom?
   `(region ((,class (:background ,alum-5))))
   `(secondary-selection ((,class (:background ,blue-3))))
   `(isearch ((,class (:foreground ,alum-1 :background ,orange-3))))
   `(lazy-highlight ((,class (:background ,choc-3))))
   `(trailing-whitespace ((,class (:background ,red-3))))
   ;; Mode line faces
   `(mode-line ((,class
                 (:background "black"
                  :foreground ,alum-2
                  :box (:line-width -1 :color ,alum-6 :style released-button)))))
   `(mode-line-buffer-id ((,class (:foreground ,orange-1 :weight bold))))
   `(mode-line-inactive ((,class
                          (:inherit mode-line
                           :background ,alum-6
                           :foreground ,alum-5
                           :box (:line-width -1 :color "black" :style released-button)
                           :weight light))))
   ;; Escape and prompt faces
   `(comint-highlight-input ((,class (:foreground ,blue-0))))
   `(comint-highlight-prompt ((,class (:foreground ,blue-0))))
   `(minibuffer-prompt ((,class (:foreground ,blue-0))))
   `(escape-glyph ((,class (:foreground ,butter-3))))
   `(error ((,class (:foreground ,red-0))))
   `(warning ((,class (:foreground ,orange-1))))
   `(success ((,class (:foreground ,cham-1))))
   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,plum-1))))
   `(font-lock-comment-face ((,class (:foreground ,cham-2))))
   `(font-lock-constant-face ((,class (:foreground ,plum-0))))
   `(font-lock-function-name-face ((,class (:foreground ,butter-1))))
   `(font-lock-keyword-face ((,class (:foreground ,cham-0))))
   `(font-lock-string-face ((,class (:foreground ,choc-1))))
   `(font-lock-type-face ((,class (:foreground ,blue-0))))
   `(font-lock-variable-name-face ((,class (:foreground ,orange-1))))
   ;; Button and link faces
   `(link ((,class (:underline t :foreground ,blue-1))))
   `(link-visited ((,class (:underline t :foreground ,blue-2))))
   ;; Gnus faces
   `(gnus-group-news-1 ((,class (:foreground ,plum-1))))
   `(gnus-group-news-1-low ((,class (:foreground ,plum-2))))
   `(gnus-group-news-2 ((,class (:foreground ,blue-1))))
   `(gnus-group-news-2-low ((,class (:foreground ,blue-2))))
   `(gnus-group-news-3 ((,class (:foreground ,cham-1))))
   `(gnus-group-news-3-low ((,class (:foreground ,cham-2))))
   `(gnus-group-news-4 ((,class (:foreground ,plum-0))))
   `(gnus-group-news-4-low ((,class (:foreground ,choc-2))))
   `(gnus-group-news-5 ((,class (:foreground ,orange-1))))
   `(gnus-group-news-5-low ((,class (:foreground ,orange-2))))
   `(gnus-group-news-low ((,class (:foreground ,butter-2))))
   `(gnus-group-mail-1 ((,class (:foreground ,plum-1))))
   `(gnus-group-mail-1-low ((,class (:foreground ,plum-2))))
   `(gnus-group-mail-2 ((,class (:foreground ,blue-1))))
   `(gnus-group-mail-2-low ((,class (:foreground ,blue-2))))
   `(gnus-group-mail-3 ((,class (:foreground ,cham-1))))
   `(gnus-group-mail-3-low ((,class (:foreground ,cham-2))))
   `(gnus-group-mail-low ((,class (:foreground ,butter-2))))
   `(gnus-header-content ((,class (:weight normal :foreground ,butter-3))))
   `(gnus-header-from ((,class (:foreground ,butter-2))))
   `(gnus-header-subject ((,class (:foreground ,cham-1))))
   `(gnus-header-name ((,class (:foreground ,blue-1))))
   `(gnus-header-newsgroups ((,class (:foreground ,choc-2))))
   ;; ERC faces
   `(erc-action-face ((,class (:foreground ,cham-2))))
   `(erc-current-nick-face ((,class (:foreground ,red-2 :weight bold))))
   `(erc-input-face ((,class (:foreground ,blue-0))))
   `(erc-my-nick-face ((,class (:foreground ,blue-0))))
   `(erc-nick-default-face ((,class (:foreground ,butter-2 :weight bold))))
   `(erc-prompt-face ((,class (:background ,alum-6 :foreground ,alum-1 :weight bold))))
   `(erc-timestamp-face ((,class nil))) ;; wtf?
   ;; Message faces
   `(message-header-name ((,class (:foreground ,blue-1))))
   `(message-header-cc ((,class (:foreground ,butter-3))))
   `(message-header-other ((,class (:foreground ,choc-2))))
   `(message-header-subject ((,class (:foreground ,cham-1))))
   `(message-header-to ((,class (:foreground ,butter-2))))
   `(message-cited-text ((,class (:foreground ,cham-1))))
   `(message-separator ((,class (:foreground ,plum-1))))
   ;; SMerge faces
   `(smerge-refined-change ((,class (:background ,blue-3))))
   ;; Ediff faces
   `(ediff-current-diff-A ((,class (:background ,alum-5))))
   `(ediff-fine-diff-A ((,class (:background ,blue-3))))
   `(ediff-even-diff-A ((,class (:background ,alum-5.5))))
   `(ediff-odd-diff-A ((,class (:background ,alum-5.5))))
   `(ediff-current-diff-B ((,class (:background ,alum-5))))
   `(ediff-fine-diff-B ((,class (:background ,choc-3))))
   `(ediff-even-diff-B ((,class (:background ,alum-5.5))))
   `(ediff-odd-diff-B ((,class (:background ,alum-5.5))))
   ;; Diff faces
   `(diff-added ((,class (:foreground ,cham-1))))
   `(diff-removed ((,class (:foreground ,red-1))))
   `(diff-changed ((,class (:foreground ,orange-1))))
   `(diff-file-header ((,class (:bold t))))
   `(diff-header ((,class (:foreground ,blue-1))))
   `(diff-refine-change ((,class (:background ,blue-3))))
   ;; Flyspell faces
   `(flyspell-duplicate ((,class (:underline ,orange-1))))
   `(flyspell-incorrect ((,class (:underline ,red-1))))
   ;; Semantic faces
   `(semantic-decoration-on-includes ((,class (:underline ,alum-4))))
   `(semantic-decoration-on-private-members-face
     ((,class (:background ,plum-3))))
   `(semantic-decoration-on-protected-members-face
     ((,class (:background ,choc-3))))
   `(semantic-decoration-on-unknown-includes
     ((,class (:background ,red-3))))
   `(semantic-decoration-on-unparsed-includes
     ((,class (:background ,alum-5.5))))
   `(semantic-tag-boundary-face ((,class (:overline ,blue-1))))
   `(semantic-unmatched-syntax-face ((,class (:underline ,red-1))))
   ;; Dired & Sunrise Commander faces
   `(dired-directory ((,class (:foreground ,cham-2))))
   `(sr-active-path-face ((,class (:background ,alum-6 :foreground ,blue-1))))
   `(sr-passive-path-face ((,class (:background ,alum-6 :foreground ,alum-4))))
   `(sr-editing-path-face ((,class (:background "red" :foreground "yellow")))) ;; what is this?
   '(diredp-dir-priv ((t (:inherit dired-directory))))
   '(diredp-exec-priv ((t nil)))
   '(diredp-file-name ((t (:inherit default))))
   '(diredp-ignored-file-name ((t (:inherit shadow))))
   '(diredp-link-priv ((t nil)))
   '(diredp-no-priv ((t nil)))
   '(diredp-number ((t (:inherit default))))
   '(diredp-other-priv ((t nil)))
   '(diredp-rare-priv ((t nil)))
   '(diredp-read-priv ((t nil)))
   '(diredp-write-priv ((t nil)))
   ;; hilight current directory path at the top
   `(sr-highlight-path-face ((,class (:background ,alum-7 :foreground ,blue-1))))
   `(sr-mirror-path-face ((,class (:background "blue" :foreground "yellow")))) ;; what is this?
   `(sr-tabs-active-face ((,class (:foreground ,red-0))))
   `(sr-tabs-inactive-face ((,class (:foreground ,orange-1))))
   ;; Ido faces
   `(ido-first-match ((,class :foreground ,choc-1 :weight bold)))
   `(ido-only-match ((,class :foreground ,cham-2)))
   `(ido-subdir ((,class :foreground ,cham-2 :weight bold)))
   ;; org-mode
   `(org-table ((,class (:inherit fixed-pitch :foreground ,blue-0))))
   '(org-block ((t (:inherit (shadow fixed-sys)))))
   '(org-block-background ((t (:inherit fixed-pitch :background "#232a2b"))))
   '(org-formula ((t (:inherit fixed-pitch :foreground "chocolate1"))))
   '(org-mode-line-clock ((t nil)) t)
   '(org-table ((t (:inherit fixed-pitch :foreground "#8cc4ff"))))
   '(org-verbatim ((t (:inherit org-code))))
   ;; Random faces
   `(dropdown-list-face ((,class (:inherit default
                                  :background ,choc-1
                                  :foreground "black"))))
   `(eshell-prompt ((,class (:foreground ,plum-2 :weight bold))))
   `(fixed-pitch ((,class (:height ,fixed-sys-height :family ,fixed-sys))))
   `(hl-line ((,class (:inherit nil :background ,alum-7))))
   `(markdown-pre-face ((,class (:inherit font-lock-constant-face :family ,fixed-sys))))
   `(markdown-inline-code-face ((,class (:inherit markdown-pre-face))))
   `(paren-face ((,class (:foreground ,alum-4))))
   `(variable-pitch ((,class (:height 144 :family ,variable-font))))

   ;; My special themes
   `(my-reading-face ((,class (:background "#808000"
                               :foreground "#000000")))))

  (custom-theme-set-variables
   'my-tango-dark
   `(ansi-color-names-vector [,alum-7 ,red-0 ,cham-0 ,butter-1
                                      ,blue-1 ,plum-1 ,blue-0 ,alum-1])))

(provide-theme 'my-tango-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; my-tango-dark-theme.el ends here
