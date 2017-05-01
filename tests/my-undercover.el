(when (require 'undercover nil t)
  (undercover "*.el" "site-lisp/*.el"))

(provide 'my-undercover)
