(package-initialize)

(server-start)
(defconst emacs-start-time (current-time))
(defmacro org-babel-header-args-safe-fn (safe-list) t)
(setq eieio-backward-compatibility nil)

;; Emacs gurus don't need no stinking scroll bars & widgets
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1))

(defmacro my-with-elapsed-timer (text &rest body)
  (declare (indent 1))
  (let ((nowvar (make-symbol "now")))
    `(let ((,nowvar (current-time)))
       (message "%s..." ,text)
       (prog1 (progn ,@body)
         (let ((elapsed
                (float-time (time-subtract (current-time) ,nowvar))))
           (message "%s...done (%.3fs)" ,text elapsed))))))

(my-with-elapsed-timer "straight.el"
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (setq straight-host-usernames '((github . "Fuco1")))

  (straight-use-package '(el-patch :fork t))
  (require 'el-patch))

(prog1 "Set up no-littering"
  (straight-use-package 'no-littering)
  (require 'no-littering)
  (no-littering-theme-backups)
  (setq diary-file "/home/matus/org/diary"))

(straight-use-package 'org-contrib)
(straight-use-package 'org)

(straight-use-package 'use-package)
(setq use-package-verbose t)

(defun my-where-am-i ()
  (with-temp-buffer
    (insert-file-contents "~/.whereami")
    (buffer-string)))

(defun my-startup-screen ()
  (with-current-buffer (get-buffer-create "Startup screen")
    (fundamental-mode)
    (erase-buffer)
    (insert
     (format
      "Blessed art thou, who hath come to the One True Editor.

        – Anonymous

Emacs outshines all other editing software in approximately the same
way that the noonday sun does the stars. It is not just bigger and
brighter; it simply makes everything else vanish.

        – Neal Stephenson, “In the Beginning was the Command Line”

Loaded %d packages in %.3fs seconds

There are %d customizable settings available."
      (length package-activated-list)
      emacs-load-time
      (let (re) (mapatoms
                 (lambda (symbol)
                   (when (get symbol 'standard-value)
                     (push symbol re))))
           (length re))))
    (current-buffer)))

;; add repos
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(my-with-elapsed-timer "Initializing packages"
  (straight-use-package 'dash)
  (straight-use-package 'f)
  (straight-use-package 's)
  (straight-use-package 'dash-functional)

  (require 'uniquify)

  ;; add load paths
  (load "~/.emacs.d/site-lisp/my-bootstrap")
  (my-setup-load-path)

  (require 'workman-layout))

;; autoloads
(autoload 'calc-same-interface "calc" nil t)
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(my-with-elapsed-timer "Loading site lisp"
  ;; load site lisp
  (require 'my-site-lisp-autoloads)
  (require 'my-advices)
  (require 'my-macros)
  (load "~/.emacs.d/site-lisp/vendor")

  ;; load keys
  (load "~/.emacs.d/files/keys"))

;; Customize
(setq custom-file "~/.emacs.d/files/emacs-custom.el")
(load custom-file)

;; load config files
(my-with-elapsed-timer "Loading vendor"
  (load "~/.emacs.d/files/vendor"))

(require 'my-redef)
(require 'my-redef-dynamic)

;; load settings
(my-with-elapsed-timer "Loading settings"
  (load "~/.emacs.d/files/global")
  (load "~/.emacs.d/files/mode-line")
  (load "~/.emacs.d/files/tabs")
  (when (eq system-type 'windows-nt)
    (load "~/.emacs.d/files/windows")))

(my-with-elapsed-timer "Loading personal"
  (load "~/.emacs.d/files/personal" :no-error))

;; diminish useless modeline clutter
;; TODO: move to vendor.el
(straight-use-package 'diminish)
(diminish 'visual-line-mode)
(eval-after-load "face-remap" '(diminish 'buffer-face-mode))

;; Reload theme -- hackish
(load "~/.emacs.d/themes/my-tango-dark-theme")

;;; post init.
(when window-system
  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (defconst emacs-load-time elapsed)
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))
(put 'list-timers 'disabled nil)
