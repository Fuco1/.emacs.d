
;;; Definition stored by Calc on Sat Aug 30 21:54:31 2014
(put 'calc-define 'calc-tri '(progn
 (defun calc-tri nil (interactive) (calc-wrapper (calc-enter-result 1 "tri" (cons (quote calcFunc-tri) (calc-top-list-n 1)))))
 (put 'calc-tri 'calc-user-defn 't)
 (defun calcFunc-tri (x) (math-check-const x t) (math-normalize (list
  (quote /) (list (quote *) x (list (quote +) x 1)) 2)))
 (put 'calcFunc-tri 'calc-user-defn '(/ (* (var x var-x) (+ (var x
  var-x) 1)) 2))
 (define-key calc-mode-map "zt" 'calc-tri)
))
