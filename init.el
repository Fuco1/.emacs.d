(server-start)
(defconst emacs-start-time (current-time))
(defmacro org-babel-header-args-safe-fn (safe-list) t)

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

        – Anonymous

Emacs outshines all other editing software in approximately the same
way that the noonday sun does the stars. It is not just bigger and
brighter; it simply makes everything else vanish.

        – Neal Stephenson, “In the Beginning was the Command Line”

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

;; Customize
(setq custom-file "~/.emacs.d/files/emacs-custom.el")
(load custom-file)

;; load config files
(my-with-elapsed-timer "Loading vendor"
  (load "~/.emacs.d/files/vendor"))

(my-with-elapsed-timer "Loading personal"
  (load "~/.emacs.d/files/personal" :no-error))

;; diminish useless modeline clutter
(require 'diminish)
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
