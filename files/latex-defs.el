;; how to setup zathura http://oku.edu.mie-u.ac.jp/~okumura/texwiki/?AUCTeX

(use-package latex
  :defer t
  :config
  (progn
    (use-package reftex
      :defer t
      :diminish reftex-mode)
    (sp-local-pair 'latex-mode "\\begin" "\\end")
    (sp-local-tag 'latex-mode "\\ba" "\\begin{align*}" "\\end{align*}")

    (use-package preview)
    (use-package font-latex)
    (fset 'tex-font-lock-suscript 'ignore)

    (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
      (sp-local-pair "\\[" nil :post-handlers '(my-latex-math-block-indent)))

    (defun my-latex-math-block-indent (a action c)
      (when (eq action 'insert)
        (newline-and-indent)
        (save-excursion (newline))))

    (defun my-latex-install-package (package)
      "Install a texlive packag."
      (interactive (list (word-at-point)))
      (let ((buf (get-buffer-create "*tlmgr*")))
        (with-current-buffer buf (erase-buffer))
        (display-buffer buf)
        (call-process "tlmgr" nil buf t "install" package)))
    (bind-key "C-c i" 'my-latex-install-package LaTeX-mode-map)

    (defun my-latex-compile ()
      (interactive)
      (save-buffer)
      (TeX-command "LaTeX" 'TeX-master-file nil))
    (bind-key "C-M-x" 'my-latex-compile LaTeX-mode-map)

    (defvar my-latex-wrap-choices '("emph"
                                    "textsc"
                                    "underbrace"))
    (defvar my-latex-wrap-history nil)

    (defun my-latex-wrap (macro-name)
      (interactive (list (ido-completing-read
                          "Macro> "
                          my-latex-wrap-choices
                          nil 'confirm nil my-latex-wrap-history)))
      (when (use-region-p)
        (let ((b (region-beginning))
              (e (region-end)))
          (goto-char e)
          (insert "}")
          (goto-char b)
          (insert "\\" macro-name "{"))))
    (bind-key "C-c w" 'my-latex-wrap LaTeX-mode-map)

    (defun my-latex-beginning-of-defun (&optional arg)
      "Move to the beginning of ARGth environment after the point."
      (interactive "p")
      (when (re-search-forward "^\\\\begin{" nil t (- arg))
        (back-to-indentation)
        t))

    (defun my-latex-end-of-defun ()
      "Move to the end of ARGth environment after the point."
      (interactive)
      (when (re-search-forward "^\\\\end{" nil t arg)
        (forward-line)))

    ;; fix italian quote highlight
    (push '("\"<" "\">") font-latex-quote-list)

    (defun my-latex-remove-command ()
      "Unwrap the expression that point is in or before, also
removing the command name.  By command we understand a symbol
starting with \\ and followed by a block of text enclosed in {}."
      (interactive)
      (let ((ok (sp-get-enclosing-sexp)))
        (cond
         ;; we're inside the { } block
         (ok
          (progn
            (save-excursion
              (goto-char (sp-get ok :beg))
              (zap-to-char -1 ?\\ ))
            (sp-splice-sexp)))
         ;; test if we are in looking at the command fromt he front
         ((looking-at "\\\\")
          (zap-up-to-char 1 ?{)
          (sp-unwrap-sexp))
         ;; otherwise we're inside the command name
         (t
          (zap-to-char -1 ?\\ )
          (zap-up-to-char 1 ?{)
          (sp-unwrap-sexp)))))
    (bind-key "C-c d" 'my-latex-remove-command LaTeX-mode-map)
    (bind-key "M-RET" 'LaTeX-insert-item LaTeX-mode-map)

    (defun my-latex-preview-math ()
      (interactive)
      (let ((b (save-excursion (while (texmathp) (backward-char 1)) (1- (point))))
            (e (save-excursion (while (texmathp) (forward-char 1)) (point))))
        (preview-region b e)))
    (bind-key "C-<m-key>" 'my-latex-preview-math preview-map)

    (defun my-latex-goto-label ()
      (interactive)
      (let ((enc (sp-get-enclosing-sexp))
            label-name)
        (sp-get enc
          (when (save-excursion
                  (goto-char :beg-prf)
                  (looking-at-p (regexp-opt '("\\cref" "\\eqref" "\\autoref" "\\ref"))))
            (setq label-name (buffer-substring-no-properties :beg-in :end-in))))
        (when label-name
          (push-mark)
          (goto-char (point-min))
          (search-forward (concat "\\label{" label-name))
          (search-backward "{")
          (forward-char 1))))
    (bind-key "C-c L" 'my-latex-goto-label LaTeX-mode-map)

    (defun my-latex-mode-init ()
      (setq TeX-auto-save t)
      (setq TeX-parse-self t)
      (TeX-PDF-mode t)
      (setq reftex-plug-into-AUCTeX t)
      (reftex-mode t)
      (TeX-fold-mode t)

      (set (make-local-variable 'beginning-of-defun-function) 'my-latex-beginning-of-defun)
      (set (make-local-variable 'end-of-defun-function) 'my-latex-end-of-defun)

      (LaTeX-add-environments
       '("derivation" LaTeX-env-label))
      (TeX-add-symbols '("emph" 1))

      (setq fill-column 100000)
      (smartparens-mode 1)

      ;; fix the "bug" in SP regexp wrap that treats ' as "word"
      (modify-syntax-entry ?' ".")

      (message "LaTeX mode init complete."))
    ;; ACUTeX replaces latex-mode-hook with LaTeX-mode-hook
    (add-hook 'LaTeX-mode-hook 'my-latex-mode-init)))
