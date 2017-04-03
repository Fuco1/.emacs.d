(server-start)
(defconst emacs-start-time (current-time))

;; Emacs gurus don't need no stinking scroll bars & widgets
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(defmacro my-with-elapsed-timer (text &rest body)
  (declare (indent 1))
  (let ((nowvar (make-symbol "now")))
    `(let ((,nowvar (current-time)))
       (message "%s..." ,text)
       (prog1 (progn ,@body)
         (let ((elapsed
                (float-time (time-subtract (current-time) ,nowvar))))
           (message "%s...done (%.3fs)" ,text elapsed))))))

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)
(require 'use-package)
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

Loaded %d packages in %.3fs seconds"
      (length package-activated-list)
      emacs-load-time))
    (current-buffer)))

;; add repos
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(my-with-elapsed-timer "Initializing packages"
  (load "~/.emacs.d/dev/dash.el/dash")
  (load "~/.emacs.d/dev/dash.el/dash-functional")

  (require 'uniquify)
  (require 'f)
  (require 's)

  ;; add load paths
  (add-to-list 'load-path "/home/matus/dev/c++/ledger/lisp")
  (add-to-list 'load-path "/home/matus/.emacs.d/dev/legalese")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/special/")
  (mapc (lambda (dir)
          (add-to-list 'load-path dir)
          (-when-let
              (autoloads (--filter
                          (string-match-p "autoloads\\.el$" it)
                          (f-files dir)))
            (mapc 'load autoloads)))
        (f-directories "~/.emacs.d/vendor"))
  (mapc (apply-partially 'add-to-list 'load-path) (f-directories "~/.emacs.d/projects"))

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
  (require 'my-redef)
  (load "~/.emacs.d/site-lisp/vendor")

  ;; load keys
  (load "~/.emacs.d/files/keys"))

;; load settings
(my-with-elapsed-timer "Loading settings"
  (load "~/.emacs.d/files/global")
  (load "~/.emacs.d/files/mode-line")
  (load "~/.emacs.d/files/tabs")
  (when (eq system-type 'windows-nt)
    (load "~/.emacs.d/files/windows")))

;; load config files
(my-with-elapsed-timer "Loading vendor"
  (load "~/.emacs.d/files/vendor"))

(my-with-elapsed-timer "Loading personal"
  (load "~/.emacs.d/files/personal"))

;; diminish useless modeline clutter
(require 'diminish)
(diminish 'visual-line-mode)
(eval-after-load "face-remap" '(diminish 'buffer-face-mode))

;; Customize
(setq custom-file "~/.emacs.d/files/emacs-custom.el")
(load custom-file)

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
