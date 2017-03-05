;;; notmuch-unread.el --- Display unread mail count in the mode line
;;; Package-Requires: ((notmuch "0.18"))
;; Version: 20140613.444
;;; X-Original-Version: 0.1

;; Copyright (C) 2014  David Thompson <davet@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This global minor mode alters the mode line to display the number
;; of messages with the "unread" tag in notmuch.

;;; Code:

(defvar notmuch-unread-mode-line-string nil
  "String to display in the mode line.")

(defvar notmuch-unread-update-timer nil
  "Timer for updating the mode line.")

(defcustom notmuch-unread-update-interval 5
  "The number of seconds to wait in between updates."
  :type 'integer
  :group 'notmuch-unread)

(defcustom notmuch-unread-search-term "tag:unread"
  "The search term to pass to notmuch count."
  :type 'string
  :group 'notmuch-unread)

(defun notmuch-unread-count ()
  "Return the number of messages that match
`notmuch-unread-search-term`."
  (string-to-number
   (replace-regexp-in-string
    "\n" ""
    (notmuch-command-to-string "count" notmuch-unread-search-term))))

(defun notmuch-unread-update-handler ()
  "Update the mode line."
  (setq notmuch-unread-mode-line-string
        (format " [✉ %d]" (notmuch-unread-count)))
  (force-mode-line-update))

;;;###autoload
(define-minor-mode notmuch-unread-mode
  "Display unread mail count in the mode line"
  :global t
  :require 'notmuch
  (and notmuch-unread-update-timer
       (cancel-timer notmuch-unread-update-timer))
  (if notmuch-unread-mode
      (progn
       (add-to-list 'global-mode-string
                    'notmuch-unread-mode-line-string t)
       (setq notmuch-unread-update-timer
             (run-at-time nil notmuch-unread-update-interval
                          'notmuch-unread-update-handler)))
    (setq global-mode-string
          (delq 'notmuch-unread-mode-line-string
                global-mode-string))))

(provide 'notmuch-unread)
;;; notmuch-unread.el ends here
