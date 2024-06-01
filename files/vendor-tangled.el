;; -*- lexical-binding: t -*-

(use-package sh-mode
  :defer t
  :init
  (progn
    (define-derived-mode bash-mode sh-mode "Bash mode")

    (defun my-match-variables-in-quotes (limit)
      "Match variables in double-quotes in `sh-mode'."
      (with-syntax-table sh-mode-syntax-table
        (catch 'done
          (while (re-search-forward
                  ;; `rx' is cool, mkay.
                  (rx (or line-start (not (any "\\")))
                      (group "$")
                      (group
                       (or (and "{" (+? nonl) "}")
                           (and (+ (any alnum "_")))
                           (and (any "*" "@" "#" "?" "-" "$" "!" "0" "_")))))
                  limit t)
            (-when-let (string-syntax (nth 3 (syntax-ppss)))
              (when (= string-syntax 34)
                (throw 'done (point))))))))

    (font-lock-add-keywords
     'sh-mode '((my-match-variables-in-quotes
                 (1 'fixed-pitch t)
                 (2 font-lock-variable-name-face t))))
    ))

(use-package shell-mode
  :defer t
  :init
  (progn
    (defun my-shell-guess-major-mode (buffer)
      "Guess major mode for the content of BUFFER."
      (with-current-buffer buffer
        (set-auto-mode)))

    (defadvice shell-command-sentinel (after enable-better-mode activate)
      (when (memq (process-status (ad-get-arg 0)) '(exit signal))
        (my-shell-guess-major-mode (process-buffer (ad-get-arg 0)))))

    (defadvice shell-command (after enable-better-mode activate)
      (unless (eq ad-return-value 'comint-output-filter)
        (-when-let (buffer (get-buffer "*Shell Command Output*"))
          (my-shell-guess-major-mode buffer))))
    
    (defadvice shell-command-on-region (after enable-better-mode activate)
      (unless (eq ad-return-value 'comint-output-filter)
        (-when-let (buffer (get-buffer "*Shell Command Output*"))
          (unless (ad-get-arg 4)
            (my-shell-guess-major-mode buffer)))))

    (defun my-shell-mode-init ()
      (setq tab-width 8))
    (add-hook 'shell-mode-hook 'my-shell-mode-init)))

(use-package term
  :config
  (bind-key "M-o" 'elwm-activate-window term-raw-map)

  (defun my-term-mode-init ()
;;   (compilation-minor-mode 1)
  )

  (add-hook 'term-mode-hook 'my-term-mode-init)

  (defun my-term-exec-hook ()
    (let* ((buff (current-buffer))
           (proc (get-buffer-process buff)))
      (set-process-sentinel
       proc
       (lambda (process event)
         (when (or (string= event "finished\n")
                   (string-prefix-p "exited abnormally" event))
           (kill-buffer buff))))))
  
  (add-hook 'term-exec-hook 'my-term-exec-hook)
  )

(use-package ess
  :config
  (progn
    (defun my-ess-eval-last-sexp ()
      (interactive)
      (-when-let (ok (sp-get-sexp t))
        (sp-get ok
          (ess-eval-region :beg-prf :end nil))))
    (bind-key "C-x C-e" 'my-ess-eval-last-sexp ess-r-mode-map)))

(use-package fontify-face :straight t)

(use-package eshell-bookmark
  :after eshell
  :config
  (add-hook 'eshell-mode-hook #'eshell-bookmark-setup))
