(server-start)
(defconst emacs-start-time (current-time))

(add-to-list 'load-path "~/.emacs.d/vendor/use-package/")
(require 'use-package)
(setq use-package-verbose t)

;; Emacs gurus don't need no stinking scroll bars & widgets
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

;; add repos
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(use-package-with-elapsed-timer "Initializing packages"
  (package-initialize)
  (load "~/.emacs.d/dev/dash.el/dash")
  (load "~/.emacs.d/dev/dash.el/dash-functional")
  (load "~/.emacs.d/autoinstall")

  (require 'uniquify)
  (require 'f)
  (require 's)

  ;; add load paths
  (add-to-list 'load-path "~/.emacs.d/")
  (mapc (apply-partially 'add-to-list 'load-path) (f-directories "~/.emacs.d/vendor"))
  (mapc (apply-partially 'add-to-list 'load-path) (f-directories "~/.emacs.d/projects")))

;; autoloads
(autoload 'calc-same-interface "calc" nil t)
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

;; TODO: go through this and sort out useless crap, package the useful bits
(use-package-with-elapsed-timer "Loading site lisp"
  ;; load site lisp
  (load "site-lisp/advices")
  (load "site-lisp/defuns-buffer")
  (load "site-lisp/defuns-edit")
  (load "site-lisp/defuns-macros")
  (load "site-lisp/defuns")
  (load "site-lisp/emacs-lisp-mode")
  (load "site-lisp/macros")
  (load "site-lisp/vendor")
  (load "site-lisp/redef")

  ;; load keys
  (load "files/keys"))

;; load settings
(use-package-with-elapsed-timer "Loading settings"
  (load "files/global")
  (load "files/layouts")
  (load "files/mode-line")
  (load "files/tabs")
  (load "files/windows"))

;; load config files
(use-package-with-elapsed-timer "Loading vendor"
  (load "files/vendor"))

(use-package-with-elapsed-timer "Loading personal"
  (load "files/personal"))

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
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))
