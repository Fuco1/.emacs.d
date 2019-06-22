;;; my-defuns.el --- Personal functions

;;; Commentary:
;;; Code:

(require 'f)
(require 's)
(require 'dash)
(require 'org-table)
(require 'org-element)
(require 'projectile)

(require 'vc-svn)
(require 'dired)
(require 'dired-list)

;;;###autoload
(defun my-lorem ()
  "Insert lorem ipsum text."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Praesent libero orci, auctor sed, faucibus vestibulum, gravida vitae, arcu. Nunc posuere. Suspendisse potenti. Praesent in arcu ac nisl ultricies ultricies. Fusce eros. Sed pulvinar vehicula ante. Maecenas urna dolor, egestas vel, tristique et, porta eu, leo. Curabitur vitae sem eget arcu laoreet vulputate. Cras orci neque, faucibus et, rhoncus ac, venenatis ac, magna. Aenean eu lacus. Aliquam luctus facilisis augue. Nullam fringilla consectetuer sapien. Aenean neque augue, bibendum a, feugiat id, lobortis vel, nunc. Suspendisse in nibh quis erat condimentum pretium. Vestibulum tempor odio et leo. Sed sodales vestibulum justo. Cras convallis pellentesque augue. In eu magna. In pede turpis, feugiat pulvinar, sodales eget, bibendum consectetuer, magna. Pellentesque vitae augue."))

;;;###autoload
(defun shrug ()
  "Insert shrug: ¯\\_(ツ)_/¯"
  (interactive)
  (insert "¯\\_(ツ)_/¯"))

;; some functions to ease the work with mark and mark-ring
;;;###autoload
(defun my-push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

;;;###autoload
(defun my-jump-to-mark ()
  "Jump to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command '(4)))

;;;###autoload
(defun my-exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

;;;###autoload
(defun my-find-url (url)
  "Download URL and insert into current buffer at point."
  (interactive "sULR: ")
  (insert (my-url-retrieve url)))

(defun my-url-retrieve (url &optional no-headers)
  "Retrieve and return as string the content of URL.

If NO-HEADERS is non-nil, remove the HTTP headers first."
  (with-current-buffer (url-retrieve-synchronously url)
    (when no-headers
      (goto-char (point-min))
      (search-forward "\n\n")
      (delete-region (point-min) (point)))
    (prog1 (buffer-string)
      (kill-buffer))))

;;;###autoload
(defun my-unfill-paragraph ()
  "Take a multi-line paragrap and make it into a single line of text.
This is the opposite of fill-paragraph."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;;;###autoload
(defun my-remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;;;###autoload
(defun my-format-synonyms-from-wiki ()
  (interactive)
  (goto-char (point-min))
  (search-forward "Synonyms[edit]")
  (delete-region (line-beginning-position) (line-end-position))
  (delete-char 1)
  (while (< (point) (point-max))
    (sp-down-sexp)
    (let ((number (string-to-number (word-at-point))))
      (sp-up-sexp)
      (save-excursion
        (let ((text (delete-and-extract-region (point) (line-end-position))))
          (goto-line number)
          (goto-char (line-end-position))
          (insert " (syn:" text ")")))
      (delete-region (line-beginning-position) (line-end-position))
      (delete-char 1)))
  (when (looking-at "$")
    (delete-char 1)))

;; should go into s.el
(defun my-next-property-value (start prop val &optional object limit)
  "Find next position where PROP has value VAL."
  (let ((point start) done)
    (or (and (equal (plist-get (text-properties-at point object) prop) val)
             point)
        (progn
          (while (and (not done)
                      (if (or (not object)
                              (bufferp object))
                          (/= point (point-max))
                        (/= point (1- (length object))))
                      (setq point (next-single-property-change point prop object limit)))
            (setq done (equal (plist-get (text-properties-at point object) prop) val)))
          point))))

;; should go into s.el
(defun my-next-property-not-value (start prop val &optional object limit)
  "Find next position where PROP does not have value VAL."
  (let ((point start) done)
    (or (and (not (equal (plist-get (text-properties-at point object) prop) val))
             point)
        (progn
          (while (and (not done)
                      (if (or (not object)
                              (bufferp object))
                          (/= point (point-max))
                        (/= point (1- (length object))))
                      (setq point (next-single-property-change point prop object limit)))
            (setq done (not (equal (plist-get (text-properties-at point object) prop) val))))
          point))))

;; ported from gnus-*... should go into s.el
;; TODO: add more general version where we can pass a predicate
(defun my-remove-text-properties-when
  (property value start end properties &optional object)
  "Like `remove-text-properties', only applied on where PROPERTY is VALUE."
  (let (point)
    (setq start (my-next-property-value start property value object end))
    (while (and start
                (< start end)
                (setq point (my-next-property-not-value start property value object end)))
      (remove-text-properties start point properties object)
      (setq start (my-next-property-value point property value object end)))
    object))

;;;###autoload
(defun my-sprunge (text &optional language)
  "Paste TEXT to sprunge.us.

With non-nil prefix argument, ask for LANGUAGE."
  (interactive (list (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-from-minibuffer "Text: "))
                     (when current-prefix-arg (read-from-minibuffer "Language: " nil
                                                                    nil nil nil "cl"))))
  (let* ((buf (get-buffer-create " *sprunge-result*"))
         (url (with-current-buffer buf
                (cd "/")
                (shell-command (concat "echo "
                                       (shell-quote-argument text)
                                       " | curl -s -F 'sprunge=<-' http://sprunge.us")
                               buf)
                (s-trim (buffer-string)))))
    (kill-buffer buf)
    (kill-new (if language (concat url "?" language) url))))

;;;###autoload
(defun my-update-mpd ()
  "Update the mpd database using the common parent of current dired listing."
  (interactive)
  (start-process "mpc" nil
                 "mpc" "update" (s-chop-prefix (f-canonical (concat dired-list-mpc-music-directory "/"))
                                               (f-common-parent (dired-utils-get-all-files)))))

(defun my-shuffle-things (thing beg end)
  (goto-char beg)
  (let ((cur (cons 0 beg))
        (things nil)
        (vthings))
    (while (< (cdr cur) end)
      (setq cur (bounds-of-thing-at-point thing))
      (push cur things)
      (forward-thing thing 2)
      (forward-thing thing -1))
    (setq things (nreverse things))
    (setq vthings (apply 'vector things))
    (let ((len (length vthings)))
      (message "%s" len)
      (--dotimes len
        (let* ((r (random (- len it)))
               (tmp (elt vthings r)))
          (aset vthings r (elt vthings (- len it 1)))
          (aset vthings (- len it 1) tmp))))
    (let* ((orig (current-buffer))
           (replacement
            (with-temp-buffer
              ;; todo: grab whitespace around it
              (cl-loop for x across vthings do
                       (insert (with-current-buffer orig
                                 (buffer-substring (car x) (cdr x)))))
              (buffer-string))))
      (delete-region beg end)
      (goto-char beg)
      (insert replacement))))

;;;###autoload
(defun my-shuffle-lines (beg end)
  "Shuffle lines in the active region."
  (interactive "r")
  (my-shuffle-things 'line beg end))

;;;###autoload
(defun my-shuffle-words (beg end)
  "Shuffle words in the active region."
  (interactive "r")
  (my-shuffle-things 'word beg end))

;;;###autoload
(defun my-toggle-buffer-input-methods ()
  "Toggle the input methods listed in `my-buffer-input-methods'."
  (interactive)
  (when (bound-and-true-p my-buffer-input-methods)
    (let ((next (car my-buffer-input-methods)))
      (setq my-buffer-input-methods (-rotate 1 my-buffer-input-methods))
      (set-input-method next))))

;;;###autoload
(defun my-switch-buffer-LRU ()
  "Switch to LRU buffer."
  (interactive)
  (let* ((interesting-buffers (cdr (--drop-while (s-starts-with-p " " (buffer-name it)) (buffer-list))))
         (next (--first (not (s-starts-with-p " " (buffer-name it))) interesting-buffers)))
    (switch-to-buffer next)))

;;;###autoload
(defun my-occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (--when-let (thing-at-point 'symbol)
            (regexp-quote it)))
        regexp-history)
  (call-interactively 'occur))

;; TODO: contribute to orgba
;;;###autoload
(defun my-insert-date-iso (date)
  "Insert timestamp."
  (interactive (list (org-read-date)))
  (insert date))

(defun my-fit-window-to-buffer (w)
  "Just like `fit-window-to-buffer' but with max pre-set.

The max height is set to the minimum of `count-screen-lines' and
half the height of parent window."
  (fit-window-to-buffer
   w
   (min
    (1+ (count-screen-lines nil nil nil w))
    (/ (window-height (window-parent w)) 2))))

;;;###autoload
(defun my-visit-init-file ()
  "Visit init file."
  (interactive)
  (find-file user-init-file))

;;;###autoload
(defun my-find-file-in-home ()
  "Find file in the home directory"
  (interactive)
  (let ((default-directory "~"))
    (ido-find-file)))

;;;###autoload
(defun my-md5-file (filename)
  "Open FILENAME, load it into a buffer and generate the md5 of its contents"
  (interactive "f")
  (with-temp-buffer
    (insert-file-contents filename)
    (let ((hash (md5 (current-buffer))))
      (when (called-interactively-p 'interactive)
        (message hash))
      hash)))

;;;###autoload
(defun my-dired-show-duplicates (list-a list-b)
  (interactive (list (dired-get-marked-files)
                     (with-current-buffer (cdr (assoc (dired-dwim-target-directory) dired-buffers))
                       (dired-get-marked-files))))
  (let* ((list-a (--map (cons (my-md5-file it) it) list-a))
         (list-b (--map (cons (my-md5-file it) it) list-b))
         (same-files 0))
    (with-current-buffer (get-buffer-create "*duplicates*")
      (erase-buffer)
      (--each list-a
        (-when-let (name-other (cdr (assoc (car it) list-b)))
          (insert (format "- File =%s= is the same as =%s=\n" (cdr it) name-other))
          (incf same-files)))
      (insert (format "Total: %d\n" same-files))
      (org-mode)
      (pop-to-buffer (current-buffer)))))

(defvar my-rsync-remote nil
  "Remote rsync target for a project.")
(put 'my-rsync-remote 'safe-local-variable 'identity)

(defun my-sync-rsync-local-to-remote-sentinel (process state)
  "Sentinel for rsync."
  (when (equal state "finished\n")
    (with-current-buffer (process-buffer process)
      ;; the variables relative and remote are set buffer-locally in
      ;; `my-sync-rsync-remote'
      (message "Synchronized %s to %s"
               (propertize relative 'face 'font-lock-comment-face)
               (propertize remote 'face 'font-lock-builtin-face)))))

;; TODO: dorobit support na sync zloziek
;;;###autoload
(defun my-sync-rsync-local-to-remote (&optional entire-project)
  "Sync the current file/directory with `my-rsync-remote'.

With prefix argument \\[universal-argument], sync the entire project."
  (interactive "P")
  (when (bound-and-true-p my-rsync-remote)
    (-when-let ((root) (dir-locals-find-file (buffer-file-name)))
      (let ((relative (if entire-project "."
                        (s-chop-prefix root (buffer-file-name))))
            (remote my-rsync-remote))
        (with-current-buffer (get-buffer-create " *rsync-sync*")
          (kill-all-local-variables)
          (cd root)
          (setq-local relative relative)
          (setq-local remote remote)
          (set-process-sentinel
           (start-process "rsync" (current-buffer)
                          "rsync" "-ptdrR" relative remote)
           'my-sync-rsync-local-to-remote-sentinel))))))

;; TODO: remove duplicity with ltr
(defun my-sync-rsync-remote-to-local-sentinel (process state)
  "Sentinel for rsync."
  (when (equal state "finished\n")
    (with-current-buffer (process-buffer process)
      ;; the variables relative and remote are set buffer-locally in
      ;; `my-sync-rsync-remote'
      (message "Synchronized %s to %s"
               (propertize remote 'face 'font-lock-builtin-face)
               (propertize relative 'face 'font-lock-comment-face)))))

;; TODO: dorobit support na sync zloziek
;; TODO: odstranit duplicitu s l2r
;;;###autoload
(defun my-sync-rsync-remote-to-local ()
  "Sync remote version of this file to the local copy.

The remote is determined by `my-rsync-remote'."
  (interactive)
  (when (bound-and-true-p my-rsync-remote)
    (-when-let ((root) (dir-locals-find-file (buffer-file-name)))
      (let ((relative (s-chop-prefix root (buffer-file-name)))
            (remote my-rsync-remote))
        (with-current-buffer (get-buffer-create " *rsync-sync*")
          (kill-all-local-variables)
          (cd root)
          (setq-local relative relative)
          (setq-local remote remote)
          (set-process-sentinel
           (start-process "rsync" (current-buffer)
                          "rsync" "-ptd" (concat remote "/" relative) relative)
           'my-sync-rsync-remote-to-local-sentinel))))))

;;;###autoload
(defun my-run-haddock ()
  "Run haddock on current project."
  (interactive)
  (require 'projectile)
  (projectile-with-default-dir (projectile-project-root)
    (call-process-shell-command "cabal haddock --hyperlink-source &" nil 0)))

;; TODO: contribute to orgba
(defun my-org-with-column (n fun)
  "Go to Nth column of `org-mode' table and call FUN on each row."
  (declare (indent 1))
  (save-excursion
    (let ((i 0))
      (goto-char (org-table-begin))
      (forward-line 2)
      (org-table-goto-column n)
      (while (< (point) (org-table-end))
        (funcall fun i)
        (setq i (1+ i))
        (forward-line 1)
        (org-table-goto-column n))
      (org-table-align))))

;; TODO: contribute to orgba
(defun my-update-column (n data)
  "Update Nth column with DATA.

Ith row is replaced with Ith item of DATA."
  (my-org-with-column n
    (lambda (i)
      (-let* (((_ (&plist :contents-begin cb :contents-end ce)) (org-element-table-cell-parser))
              (value (nth i data)))
        (delete-region cb ce)
        (insert (format "%.3f" value))))))

(defun my-update-instrument ()
  "Recalculate instrument history record."
  (save-excursion
    (let* ((table (cddr (org-babel-read-table)))
           (costs (-map (-lambda ((n v)) (* n v)) (-select-columns '(1 2) table)))
           (total-position (cdr (nreverse (-reduce-from
                                           (-lambda ((acc &as a) it)
                                             (cons (+ a it) acc))
                                           (list 0) (-select-column 1 table)))))
           (total-cost (cdr (nreverse (-reduce-from
                                       (-lambda ((acc &as a) it)
                                         (cons (+ a it) acc))
                                       (list 0) costs))))
           (average-cost (-map (-lambda ((shares . cost)) (/ cost shares))
                               (-zip total-position total-cost)))
           (pe (-map (-lambda ((v e)) (/ v e))
                     (-select-columns '(2 7) table)))
           (yield (-map (-lambda ((v d)) (* 100 (/ d v)))
                        (-select-columns '(2 9) table)))
           (yoc (-map (-lambda ((v . d)) (* 100 (/ d v)))
                      (-zip average-cost (-select-column 9 table))))
           (income (-map (-lambda ((n . d)) (* n d))
                         (-zip total-position (-select-column 9 table)))))
      (my-update-column 4 costs)
      (my-update-column 5 total-position)
      (my-update-column 6 total-cost)
      (my-update-column 7 average-cost)
      (my-update-column 9 pe)
      (my-update-column 11 yield)
      (my-update-column 12 yoc)
      (my-update-column 13 income))))

(defun my-compute-totals ()
  "Calculate the total return and yield on portfolio."
  (save-excursion
    (save-restriction
      (widen)
      (let ((total 0)
            (return 0))
        (org-up-heading-safe)
        (org-narrow-to-subtree)
        (while (and (outline-next-heading)
                    (not (equal "Total" (org-get-heading t t))))
          (when (re-search-forward "^|" nil t)
            (let ((row (-last-item (org-table-to-lisp))))
              (cl-incf total (string-to-number (nth 5 row)))
              (cl-incf return (* (string-to-number (nth 4 row))
                                 (string-to-number (nth 9 row)))))))
        (if (re-search-forward "^|" nil t)
            (delete-region (1- (point)) (org-table-end))
          (end-of-line)
          (newline))
        (insert (format "| Cena | Return | YOC |
|------+-------+-----|
| $%.2f     | $%.2f      | %.2f%%    |"
                        total return (* 100 (/ return total))))
        (org-table-align)))))

(defun my-kaloricke-tabulky-to-cookbook ()
  (interactive)
  (insert "  :PROPERTIES:\n")
  (replace-regexp "Energie" "  :CALORIES:")
  (replace-regexp " kcal" "")
  (replace-regexp "Bílkoviny" "  :PROTEINS:")
  (replace-regexp "Sacharidy" "  :CARBOHYDRATES:")
  (replace-regexp "Tuky" "  :FATS:")
  (replace-regexp "Vláknina" "  :FIBRE:")
  (goto-char (point-min))
  (replace-regexp " g" "")
  (goto-char (point-min))
  (replace-regexp " -" " 0")
  (goto-char (point-max))
  (insert "\n  :AMOUNT:   100g\n")
  (insert "  :END:\n"))

(defun my-reload-init ()
  "Reload init.el."
  (interactive)
  (straight-transaction
    (straight-mark-transaction-as-init)
    (message "Reloading init.el...")
    (load user-init-file nil 'nomessage)
    (message "Reloading init.el... done.")))

;;;###autoload
(defun my-find-dependency ()
  "Open dependency installed with straight.el"
  (interactive)
  (let ((default-directory (straight--repos-dir)))
    (call-interactively 'ido-find-file)))

;;;###autoload
(defun my-sum-rectangle (beg end)
  (interactive "r")
  (message "rectangle sum: %s"
           (-sum (-map 'string-to-number (extract-rectangle beg end)))))

(provide 'my-defuns)
;;; my-defuns.el ends here
