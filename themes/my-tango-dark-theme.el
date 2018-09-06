;;; my-tango-dark-theme.el --- Tango-based custom theme for faces

(require 'dash)

(deftheme my-tango-dark
  "Face colors using the Tango palette (dark background).  This
is extension of default `tango-dark' face.")

(defface markup-header nil
  "Face for header markup.")
(defface markup-header-1 nil
  "Face for header markup level 1")
(defface markup-header-2 nil
  "Face for header markup level 2")
(defface markup-header-3 nil
  "Face for header markup level 3")
(defface markup-header-4 nil
  "Face for header markup level 4")
(defface markup-header-5 nil
  "Face for header markup level 5")
(defface markup-header-6 nil
  "Face for header markup level 6")
(defface markup-header-7 nil
  "Face for header markup level 7")
(defface markup-header-8 nil
  "Face for header markup level 8")

(defface markup-italic nil
  "Face for italic markup.")
(defface markup-bold nil
  "Face for bold markup.")
(defface markup-underline nil
  "Face for underline markup.")
(defface markup-strike nil
  "Face for strike markup.")
(defface markup-inline-code nil
  "Face for inline-code markup.")
(defface markup-math nil
  "Face for math markup.")
(defface markup-string nil
  "Face for string markup.")

(defface markup-url nil
  "Face for url markup.")
(defface markup-link nil
  "Face for link markup.")
(defface markup-missing-link nil
  "Face for missing-link markup.")

(defface markup-footnote nil
  "Face for footnote markup.")
(defface markup-alias nil
  "Face for alias markup.")
(defface markup-reference nil
  "Face for reference markup.")

(defface markup-list-ordered nil
  "Face for ordered list markup.")
(defface markup-list-unordered nil
  "Face for unordered list markup.")

(defface markup-blockquote nil
  "Face for blockquote markup.")
(defface markup-code-block nil
  "Face for code block.

A code block is a block that should have fixed width font and a
background color to distinguish it form the surroundings.  The
foreground fontification is supposed to be done natively by the
respective major mode.  Think org SRC blocks.")
(defface markup-pre nil
  "Face for pre markup.")
(defface markup-comment nil
  "Face for comment markup.")
(defface markup-metadata-key nil
  "Face for metadata key markup.")
(defface markup-metadata-value nil
  "Face for metadata value markup.")

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
      (variable-font (if (eq system-type 'windows-nt) "Verdana" "Source Sans Pro")))
  (my-tango-theme-set-faces
   'my-tango-dark
   (default (:foreground alum-1
             :background alum-6
             :height 120
             :family ,fixed-sys))
   (cursor (:background butter-1))

   ;; Highlighting faces
   (fringe (:background alum-7))
   (highlight (:foreground alum-6 :background butter-2))
   (highlight-thing (:background alum-5))
   (isearch (:foreground alum-1 :background orange-3))
   (italic (:slant italic))
   (lazy-highlight (:background choc-3))
   (region (:background alum-5))
   (secondary-selection (:background blue-3))
   (shadow (:foreground alum-4))
   (trailing-whitespace (:background red-3))
   (whitespace-tab (:foreground alum-7 :background butter-2))
   (whitespace-indentation (:background plum-1))

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
   (font-lock-negation-char-face (:foreground red-0))

   ;; sh-mode
   (sh-heredoc (:inherit font-lock-string-face :weight bold))
   (sh-quoted-exec (:inherit markup-math))

   ;; Outline
   (outline-1 (:foreground gradient-1))
   (outline-2 (:foreground gradient-2))
   (outline-3 (:foreground gradient-3))
   (outline-4 (:foreground gradient-4))
   (outline-5 (:foreground gradient-5))
   (outline-6 (:foreground gradient-6))
   (outline-7 (:foreground gradient-7))

   ;; Button and link faces
   (link (:foreground blue-1))
   (link-visited (:underline t :foreground blue-2))
   (w3m-anchor (:inherit link))
   (w3m-arrived-anchor (:inherit link-visited))

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

   ;; Info faces
   (Info-quoted (:inherit markup-inline-code))

   ;; SMerge faces
   (smerge-refined-change (:background blue-3))

   ;; Ediff faces
   (ediff-current-diff-A (:background alum-5))
   (ediff-current-diff-B (:background alum-5))
   (ediff-current-diff-C (:background alum-5))
   (ediff-fine-diff-A (:background blue-3))
   (ediff-fine-diff-B (:background plum-3))
   (ediff-fine-diff-C (:background choc-3))
   (ediff-even-diff-A (:background alum-6.5))
   (ediff-even-diff-B (:background alum-6.5))
   (ediff-even-diff-C (:background alum-6.5))
   (ediff-odd-diff-A (:background alum-6.5))
   (ediff-odd-diff-B (:background alum-6.5))
   (ediff-odd-diff-C (:background alum-6.5))

   ;; Diff faces
   (diff-added (:foreground cham-1))
   (diff-removed (:foreground red-1))
   (diff-changed (:foreground orange-1))
   (diff-file-header (:bold t))
   (diff-header (:foreground blue-1))
   (diff-refine-change (:background blue-3))

   ;; Magit faces
   (magit-section-heading (:inherit header-line))
   (magit-section-highlight (:background alum-5.5))

   (magit-hash (:foreground red-0))

   (magit-branch-local (:foreground blue-0))
   (magit-branch-current (:box (:line-width -1 :style nil) :inherit magit-branch-local))
   (magit-branch-remote (:foreground cham-0 :box (:line-width -1)))
   (magit-tag (:foreground butter-1 :box (:line-width -1)))
   (magit-head (:foreground "white" :box (:line-width -1)))

   (magit-diff-removed (:inherit diff-removed))
   (magit-diff-added (:inherit diff-added))
   (magit-diff-context-highlight (:background blue-3))
   (magit-diff-removed-highlight (:inherit (diff-removed magit-diff-context-highlight)))
   (magit-diff-added-highlight (:inherit (diff-added magit-diff-context-highlight)))

   (magit-diff-lines-boundary (:background "black"))

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
   (diredp-date-time (:foreground blue-1))
   (diredp-dir-name (:inherit dired-directory))
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

   ;; Faces for various markup modes.  This is ment to be an
   ;; equivalent to font-lock faces.  We define faces for common
   ;; markup elements which can then be reused by other packages
   (markup-header (:inherit font-lock-function-name-face :weight bold))
   (markup-header-1 (:height 1.1 :inherit markup-header-2))
   (markup-header-2 (:height 1.1 :inherit markup-header-3))
   (markup-header-3 (:height 1.1 :inherit markup-header-4))
   (markup-header-4 (:height 1.1 :inherit markup-header-5))
   (markup-header-5 (:inherit markup-header-6))
   (markup-header-6 (:inherit markup-header-7))
   (markup-header-7 (:inherit markup-header-8))
   (markup-header-8 (:inherit markup-header))

   (markup-italic (:inherit font-lock-variable-name-face :slant italic))
   (markup-bold (:inherit font-lock-variable-name-face :weight bold))
   (markup-underline (:underline t))
   (markup-strike (:inherit shadow :strike-through "red"))
   (markup-inline-code (:family ,fixed-sys :inherit font-lock-constant-face))
   (markup-math (:foreground orange-2))
   (markup-string (:inherit font-lock-string-face))

   (markup-url (:inherit font-lock-string-face))
   (markup-link (:inherit link))
   (markup-missing-link (:inherit font-lock-warning-face))

   (markup-footnote (:inherit font-lock-constant-face))
   (markup-alias (:inherit font-lock-type-face))
   (markup-reference (:inherit font-lock-keyword-face))

   (markup-list-ordered (:inherit font-lock-type-face :weight bold))
   (markup-list-unordered (:inherit font-lock-builtin-face))

   (markup-blockquote (:inherit font-lock-doc-face))
   (markup-code-block (:inherit fixed-pitch :background alum-6.5))
   (markup-pre (:inherit markup-code-block :foreground plum-0))
   (markup-comment (:inherit font-lock-comment-face))
   (markup-metadata-key (:inherit font-lock-keyword-face))
   (markup-metadata-value (:foreground butter-2))

   ;; rst
   (rst-level-1 (:inherit markup-header-1))
   (rst-level-2 (:inherit markup-header-2))
   (rst-level-3 (:inherit markup-header-3))
   (rst-level-4 (:inherit markup-header-4))
   (rst-level-5 (:inherit markup-header-5))

   (rst-emphasis1 (:inherit markup-italic))
   (rst-emphasis2 (:inherit markup-bold))
   (rst-literal (:inherit markup-inline-code))

   (rst-directive (:inherit markup-inline-code :foreground plum-1))
   (rst-reference (:inherit markup-link))

   ;; Markdown
   (markdown-header-face-1 (:inherit markup-header-1))
   (markdown-header-face-2 (:inherit markup-header-2))
   (markdown-header-face-3 (:inherit markup-header-3))
   (markdown-header-face-4 (:inherit markup-header-4))
   (markdown-header-face-5 (:inherit markup-header-5))

   (markdown-italic-face (:inherit markup-italic))
   (markdown-bold-face (:inherit markup-bold))
   (markdown-inline-code-face (:inherit markup-inline-code))

   (markdown-footnote-face (:inherit markup-reference))
   (markdown-reference-face (:inherit markup-reference))

   (markdown-pre-face (:inherit markup-pre))
   (markdown-list-face (:inherit markup-list-ordered))
   (markdown-code-face (:inherit markup-code-block))
   (markdown-table-face (:inherit fixed-pitch :foreground blue-0))
   (markdown-blockquote-face (:inherit markup-blockquote))

   (markdown-link-face (:inherit markup-link))

   (markdown-metadata-key-face (:inherit markup-metadata-key))
   (markdown-metadata-value-face (:inherit markup-metadata-value))

   ;; Latex
   (font-latex-sectioning-1-face (:inherit markup-header-1))
   (font-latex-sectioning-2-face (:inherit markup-header-2))
   (font-latex-sectioning-3-face (:inherit markup-header-3))
   (font-latex-sectioning-4-face (:inherit markup-header-4))
   (font-latex-sectioning-5-face (:inherit markup-header-5))

   (font-latex-italic-face (:inherit markup-italic))
   (font-latex-bold-face (:inherit markup-bold))
   (font-latex-string-face (:inherit markup-string))
   (font-latex-math-face (:inherit markup-math))

   (font-latex-verbatim-face (:inherit markup-pre))

   ;; org-mode
   (org-level-1 (:inherit outline-1))
   (org-level-2 (:inherit outline-2))
   (org-level-3 (:inherit outline-3))
   (org-level-4 (:inherit outline-4))
   (org-level-5 (:inherit outline-5))

   ;; italic, bold, underline, strike, inline-code are handled in
   ;; `org-emphasis-alist'

   (my-org-math (:foreground choc-1))

   (org-link (:inherit markup-link))

   (org-footnote (:inherit markup-reference))

   (org-code (:inherit (shadow fixed-pitch)))
   (org-block-background (:inherit fixed-pitch :background alum-6.5))
   (org-block (:inherit fixed-pitch :background alum-6.5))
   (org-verbatim (:inherit org-code))
   (org-quote (:inherit markup-blockquote :background alum-6.5))

   (org-date (:inherit fixed-pitch :foreground blue-0))
   (org-table (:inherit fixed-pitch :foreground blue-0))
   (org-formula (:inherit fixed-pitch :foreground orange-2))

   (org-special-keyword (:inherit markup-metadata-key))
   (org-property-value (:inherit markup-metadata-value))

   (org-mode-line-clock nil t)

   (org-headline-done (:inherit shadow))
   (org-list-dt (:inherit (markup-list-ordered fixed-pitch)))
   (org-checkbox (:inherit org-list-dt))

   ;; Textile
   (textile-h1-face (:inherit markup-header-1))
   (textile-h2-face (:inherit markup-header-2))
   (textile-h3-face (:inherit markup-header-3))
   (textile-h4-face (:inherit markup-header-4))
   (textile-h5-face (:inherit markup-header-5))

   (textile-emph-face (:inherit markup-italic))
   (textile-strong-face (:inherit markup-bold))
   (textile-inserted-face (:inherit markup-underline))
   (textile-deleted-face (:inherit markup-strike))
   (textile-inline-code-face (:inherit markup-inline-code))

   (textile-ul-bullet-face (:inherit markup-list-unordered))
   (textile-ol-bullet-face (:inherit markup-list-ordered))

   (textile-blockquote-face (:inherit markup-blockquote))
   (textile-pre-face (:inherit markup-pre))
   (textile-code-face (:inherit markup-code-block))

   (textile-link-face (:inherit markup-link))
   (textile-url-face (:inherit markup-url))
   (textile-footnotemark-face (:inherit markup-reference))
   (textile-footnote-face (:inherit markup-blockquote))
   (textile-lang-face (:inherit markup-alias))

   ;; Ido faces
   (ido-first-match (:foreground choc-1 :weight bold))
   (ido-only-match (:foreground cham-2))
   (ido-subdir (:foreground cham-2 :weight bold))

   ;; Helm
   (helm-buffer-directory (:inherit ido-subdir))
   (helm-source-header (:height 1.3 :family ,variable-font :weight bold :background blue-3))
   (helm-selection (:underline t :background alum-7))
   (helm-visible-mark (:background alum-5))
   (helm-candidate-number (:background plum-3))
   (helm-ff-directory (:inherit helm-buffer-directory))
   (helm-ff-file (:inherit default))
   (helm-grep-file (:inherit font-lock-type-face))

   ;; Sallet
   (sallet-source-header (:height 1.3 :family ,variable-font :weight bold :background blue-3))

   ;; Hydra
   (hydra-face-amaranth (:weight bold :foreground plum-2))
   (hydra-face-red (:weight bold :foreground red-2))
   (hydra-face-blue (:weight bold :foreground blue-1))

   ;; Company
   (company-preview-common (:foreground alum-4))

   ;; Visible mark
   (visible-mark-face1 (:background alum-3))
   (visible-mark-face2 (:background alum-4))

   ;; Ledger
   (ledger-occur-xact-face (:background "black"))

   (ledger-font-posting-account-face (:foreground choc-1))
   (ledger-font-posting-date-face (:foreground blue-0))
   (ledger-font-posting-amount-face (:foreground cham-1))

   (ledger-font-auto-xact-face (:foreground butter-3))
   (ledger-font-periodic-xact-face (:foreground cham-3))

   (ledger-font-payee-cleared-face (:foreground gradient-1))
   (ledger-font-payee-uncleared-face (:foreground red-1))

   ;; js2-mode
   (js2-function-param (:inherit font-lock-variable-name-face))
   (js2-external-variable (:inherit font-lock-keyword-face))

   (js2-jsdoc-type (:inherit font-lock-type-face))
   (js2-jsdoc-tag (:inherit font-lock-constant-face))
   (js2-jsdoc-value (:inherit font-lock-variable-name-face))

   (rjsx-tag (:inherit web-mode-html-tag-face))
   (rjsx-attr (:inherit web-mode-html-attr-name-face))

   ;; (ansi-)term
   (term-color-black (:foreground alum-4 :background alum-7))
   (term-color-blue (:foreground blue-1 :background blue-1))
   (term-color-cyan (:foreground blue-0 :background blue-0))
   (term-color-green (:foreground cham-1 :background cham-1))
   (term-color-magenta (:foreground plum-1 :background plum-1))
   (term-color-red (:foreground red-0 :background red-0))
   (term-color-white (:foreground alum-1 :background alum-1))
   (term-color-yellow (:foreground butter-1 :background butter-1))
   (term-bold nil)

   ;; lsp-ui
   (lsp-ui-doc-background (:background "black"))
   (lsp-ui-sideline-global (:background "black"))

   ;; prodigy
   (prodigy-green-face (:inherit font-lock-comment-face))
   (prodigy-red-face (:inherit font-lock-warning-face))
   (prodigy-yellow-face (:inherit font-lock-function-name-face))

   ;; Random faces
   (bmkp-local-directory (:inherit dired-directory))
   (dropdown-list-face (:inherit default :background choc-1 :foreground "black"))
   (eldoc-highlight-function-argument (:inherit bold :foreground cham-3))
   (eshell-prompt (:foreground cham-2 :weight bold))
   (fixed-pitch (:family ,fixed-sys))
   (guide-key/key-face (:foreground cham-0))
   (header-line (:inherit mode-line))
   (hl-line (:inherit nil :background alum-7))
   (indent-guide-face (:inherit paren-face))
   (sp-pair-overlay-face (:background blue-3 :foreground alum-1))
   (variable-pitch (:height 144 :family ,variable-font))
   (wgrep-delete-face (:inherit font-lock-warning-face))
   (fold-this-overlay (:inherit sp-pair-overlay-face))
   (highlight-thing (:background "black"))
   (which-func (:inherit font-lock-function-name-face))
   (anzu-mode-line (:inherit font-lock-function-name-face :weight bold))
   (yaml-tab-face (:inherit default))

   ;; My special themes
   (my-reading-face (:background "#808000" :foreground "#000000")))

  (custom-theme-set-variables
   'my-tango-dark
   `(ansi-color-names-vector ,(apply 'vector (mapcar 'my-tango-resolve-color [alum-7 red-0 cham-0 butter-1 blue-1 plum-1 blue-0 alum-1])))))

(provide-theme 'my-tango-dark)

;; Local Variables:
;; no-byte-compile: t
;; eval: (font-lock-add-keywords nil (-map (-lambda ((face . color)) (let ((pattern (concat "\\_<" (symbol-name face) "\\_>")) (color-resolved (if (stringp color) color (cdr (assq color (cdar my-tango-colors)))))) (list pattern 0 `(rainbow-colorize-match ,color-resolved)))) (cdar my-tango-colors)))
;; eval: (rainbow-mode 1)
;; eval: (fontify-face-mode 1)
;; End:

;;; my-tango-dark-theme.el ends here
