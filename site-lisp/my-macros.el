;;; my-macros.el --- Personal macros

;;; Commentary:
;;; Code:

(defmacro my-with-every-line (&rest forms)
  "Execute FORMS on each line following point to the end of buffer."
  (declare (indent 0))
  `(progn
     (beginning-of-line)
     (while (not (eobp))
       ,@forms
       (forward-line))))

(defmacro my-with-each-line (&rest body)
  "Execute BODY on each line in buffer."
  (declare (indent 0)
           (debug (body)))
  `(save-excursion
     (goto-char (point-min))
     ,@body
     (while (= (forward-line) 0)
       ,@body)))

(defmacro my-fix-reset-after-each (&rest forms)
  (declare (indent 0))
  `(progn
     ,@(apply 'append (mapcar (lambda (form) (list '(goto-char (point-min)) form)) forms))))

(defmacro my-with-temporary-hook (hook fn &rest body)
  "For the duration of BODY add FN to HOOK.

FN can be a lambda or a symbol with a function.

This is especially useful to add closures which are built
on-the-fly to hooks for the duration of the BODY."
  (declare (indent 1))
  (let ((hook-fn (make-symbol "--temp-symbol--")))
    `(let ((,hook-fn (make-symbol "--temp-hook--")))
       (cl-letf (((symbol-function ,hook-fn) ,fn))
         (unwind-protect
             (progn
               (add-hook ,hook ,hook-fn)
               ,@body)
           (remove-hook ,hook ,hook-fn))))))

(provide 'my-macros)
;;; my-macros.el ends here
