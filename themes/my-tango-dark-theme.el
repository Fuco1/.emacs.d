;;; my-tango-dark-theme.el --- Tango-based custom theme for faces

(deftheme my-tango-dark
  "Face colors using the Tango palette (dark background).  This
is extension of default `tango-dark' face.")

;; 00, 5f, 87, af, d7, ff
(defvar my-tango-colors
  '((((class color) (min-colors 65535))
     (butter-1 . "#fce94f")
     (butter-2 . "#edd400")
     (butter-3 . "#c4a000")
     (orange-1 . "#fcaf3e")
     (orange-2 . "#f57900")
     (orange-3 . "#ce5c00")
     (choc-1   . "#e9b96e")
     (choc-2   . "#c17d11")
     (choc-3   . "#8f5902")
     (cham-0   . "#b4fa70")
     (cham-1   . "#8ae234")
     (cham-2   . "#73d216")
     (cham-3   . "#4e9a06")
     (blue-0   . "#8cc4ff")
     (blue-1   . "#729fcf")
     (blue-2   . "#3465a4")
     (blue-3   . "#204a87")
     (plum-0   . "#e6a8df")
     (plum-1   . "#ad7fa8")
     (plum-2   . "#75507b")
     (plum-3   . "#5c3566")
     (red-0    . "#ff4b4b")
     (red-1    . "#ef2929")
     (red-2    . "#cc0000")
     (red-3    . "#a40000")
     (alum-1   . "#eeeeec")
     (alum-2   . "#d3d7cf")
     (alum-3   . "#babdb6")
     (alum-4   . "#888a85")
     (alum-5   . "#555753")
     (alum-5.5 . "#41423f")
     (alum-6   . "#2e3436")
     (alum-6.5 . "#232a2b")
     (alum-7   . "#212526")
     ;; set following outline levels
     (gradient-1 . butter-1)
     (gradient-2 . orange-1)
     (gradient-3 . cham-0)
     (gradient-4 . cham-2)
     (gradient-5 . blue-0)
     (gradient-6 . plum-0)
     (gradient-7 . plum-1)
     (gradient-8 . choc-1))
    (((class color) (min-colors 256))
     (butter-1 . "#ffff5f")
     (butter-2 . "#d7d700")
     (butter-3 . "#af8700")
     (orange-1 . "#ffaf5f")
     (orange-2 . "#ff8700")
     (orange-3 . "#d75f00")
     (choc-1   . "#ffaf5f")
     (choc-2   . "#d78700")
     (choc-3   . "#875f00")
     (cham-0   . "#afff5f")
     (cham-1   . "#87ff00")
     (cham-2   . "#87d700")
     (cham-3   . "#5faf00")
     (blue-0   . "#87d7ff")
     (blue-1   . "#5f87d7")
     (blue-2   . "#5f87af")
     (blue-3   . "#005f87")
     (plum-0   . "#ffafff")
     (plum-1   . "#af87af")
     (plum-2   . "#875f87")
     (plum-3   . "#5f005f")
     (red-0    . "#ff5f5f")
     (red-1    . "#ff5f5f")
     (red-2    . "#d70000")
     (red-3    . "#af0000")
     (alum-1   . "#eeeeee")
     (alum-2   . "#dadada")
     (alum-3   . "#bcbcbc")
     (alum-4   . "#8a8a8a")
     (alum-5   . "#585858")
     (alum-5.5 . "#4e4e4e")
     (alum-6   . "#303030")
     (alum-6.5 . "#262626")
     (alum-7   . "#1c1c1c")
     ;; set following outline levels
     (gradient-1 . butter-1)
     (gradient-2 . orange-1)
     (gradient-3 . cham-0)
     (gradient-4 . cham-2)
     (gradient-5 . blue-0)
     (gradient-6 . plum-0)
     (gradient-7 . plum-1)
     (gradient-8 . choc-1))))

(defun my-tango-resolve-color (color-name &optional color-set)
  (setq color-set (or color-set (cdar my-tango-colors)))
  (let ((color (cdr (assq color-name color-set))))
    (if (stringp color) color
      (my-tango-resolve-color color color-set))))

(defun my-tango-replace-colors (face-def color-set)
  (--tree-map (if (and (symbolp it) (assq it color-set))
                  (my-tango-resolve-color it color-set)
                it) face-def))

(defun my-tango-resolve-face (face-def)
  (list '\` (cons
             (car face-def)
             (list
              (-map
               (lambda (color-set)
                 (cons (car color-set) (my-tango-replace-colors (cadr face-def) (cdr color-set))))
               my-tango-colors)))))

(defmacro my-tango-theme-set-faces (theme &rest faces)
  `(custom-theme-set-faces
    ,theme
    ,@(-map 'my-tango-resolve-face faces)))

(let ((fixed-sys (if (eq system-type 'windows-nt) "Consolas" "Inconsolata"))
      (fixed-sys-height (if (eq system-type 'windows-nt) 100 120))
      (variable-font (if (eq system-type 'windows-nt) "Verdana" "Droid Sans")))
  (my-tango-theme-set-faces
   'my-tango-dark
   (default (:foreground alum-1
             :background alum-6
             :family ,fixed-sys
             :foundry "outline"
             :height ,fixed-sys-height))
   (cursor (:background butter-1))

   ;; Highlighting faces
   (fringe (:background alum-7))
   (highlight (:foreground alum-6 :background butter-2))
   (region (:background alum-5))
   (secondary-selection (:background blue-3))
   (isearch (:foreground alum-1 :background orange-3))
   (lazy-highlight (:background choc-3))
   (trailing-whitespace (:background red-3))

   ;; Mode line faces
   (mode-line (:background "black"
               :foreground alum-2
               :box (:line-width -1 :color alum-6 :style released-button)))
   (mode-line-buffer-id (:foreground orange-1 :weight bold))
   (mode-line-inactive (:inherit mode-line
                        :background alum-6
                        :foreground alum-5
                        :box (:line-width -1 :color "black" :style released-button)
                        :weight light))

   ;; Escape and prompt faces
   (comint-highlight-input (:foreground blue-0))
   (comint-highlight-prompt (:foreground blue-0))
   (minibuffer-prompt (:foreground blue-0))
   (escape-glyph (:foreground butter-3))
   (error (:foreground red-0))
   (warning (:foreground orange-1))
   (success (:foreground cham-1))

   ;; Font lock faces
   (font-lock-builtin-face (:foreground plum-1))
   (font-lock-comment-face (:foreground cham-2))
   (font-lock-constant-face (:foreground plum-0))
   (font-lock-function-name-face (:foreground butter-1))
   (font-lock-keyword-face (:foreground cham-0))
   (font-lock-string-face (:foreground choc-1))
   (font-lock-type-face (:foreground blue-0))
   (font-lock-variable-name-face (:foreground orange-1))

   ;; Button and link faces
   (link (:underline t :foreground blue-1))
   (link-visited (:underline t :foreground blue-2))

   ;; Circe
   (circe-highlight-nick-face (:foreground red-1))
   (circe-my-message-face (:foreground blue-0))
   (circe-originator-face (:foreground butter-2))
   (lui-button-face (:inherit link))
   (lui-time-stamp-face (:foreground alum-5))

   ;; Message faces
   (message-header-name (:foreground blue-1))
   (message-header-cc (:foreground butter-3))
   (message-header-other (:foreground choc-2))
   (message-header-subject (:foreground cham-1))
   (message-header-to (:foreground butter-2))
   (message-cited-text (:foreground cham-1))
   (message-separator (:foreground plum-1))

   ;; SMerge faces
   (smerge-refined-change (:background blue-3))

   ;; Ediff faces
   (ediff-current-diff-A (:background alum-5))
   (ediff-fine-diff-A (:background blue-3))
   (ediff-even-diff-A (:background alum-5.5))
   (ediff-odd-diff-A (:background alum-5.5))
   (ediff-current-diff-B (:background alum-5))
   (ediff-fine-diff-B (:background choc-3))
   (ediff-even-diff-B (:background alum-5.5))
   (ediff-odd-diff-B (:background alum-5.5))

   ;; Diff faces
   (diff-added (:foreground cham-1))
   (diff-removed (:foreground red-1))
   (diff-changed (:foreground orange-1))
   (diff-file-header (:bold t))
   (diff-header (:foreground blue-1))
   (diff-refine-change (:background blue-3))

   ;; Flyspell faces
   (flyspell-duplicate (:underline orange-1))
   (flyspell-incorrect (:underline red-1))

   ;; Semantic faces
   (semantic-decoration-on-includes (:underline alum-4))
   (semantic-decoration-on-private-members-face (:background plum-3))
   (semantic-decoration-on-protected-members-face (:background choc-3))
   (semantic-decoration-on-unknown-includes (:background red-3))
   (semantic-decoration-on-unparsed-includes (:background alum-5.5))
   (semantic-tag-boundary-face (:overline blue-1))
   (semantic-unmatched-syntax-face (:underline red-1))

   ;; Dired faces
   (dired-directory (:foreground cham-2))
   (diredp-dir-priv (:inherit dired-directory))
   (diredp-exec-priv nil)
   (diredp-file-name (:inherit default))
   (diredp-ignored-file-name (:inherit shadow))
   (diredp-link-priv nil)
   (diredp-no-priv nil)
   (diredp-number (:inherit default))
   (diredp-other-priv nil)
   (diredp-rare-priv nil)
   (diredp-read-priv nil)
   (diredp-write-priv nil)

   ;; Ido faces
   (ido-first-match (:foreground choc-1 :weight bold))
   (ido-only-match (:foreground cham-2))
   (ido-subdir (:foreground cham-2 :weight bold))

   ;; org-mode
   (org-table (:inherit fixed-pitch :foreground blue-0))
   (org-block (:inherit (shadow fixed-sys)))
   (org-block-background (:inherit fixed-pitch :background alum-6.5))
   (org-formula (:inherit fixed-pitch :foreground orange-2))
   (org-mode-line-clock nil t)
   (org-table (:inherit fixed-pitch :foreground blue-0))
   (org-verbatim (:inherit org-code))

   ;; Random faces
   (dropdown-list-face (:inherit default :background choc-1 :foreground "black"))
   (eshell-prompt (:foreground plum-2 :weight bold))
   (fixed-pitch (:height ,fixed-sys-height :family ,fixed-sys))
   (hl-line (:inherit nil :background alum-7))
   (markdown-pre-face (:inherit font-lock-constant-face :family ,fixed-sys))
   (markdown-inline-code-face (:inherit markdown-pre-face))
   (paren-face (:foreground alum-4))
   (variable-pitch (:height 144 :family ,variable-font))
   (sp-pair-overlay-face (:background blue-1 :foreground alum-7))

   ;; My special themes
   (my-reading-face (:background "#808000" :foreground "#000000")))

  (custom-theme-set-variables
   'my-tango-dark
   `(ansi-color-names-vector ,(apply 'vector (mapcar 'my-tango-resolve-color [alum-7 red-0 cham-0 butter-1 blue-1 plum-1 blue-0 alum-1])))))

(provide-theme 'my-tango-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; my-tango-dark-theme.el ends here
