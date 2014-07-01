(require 'quail)

(quail-define-package
 "english-prog" "English programmer" "EN-P" t
 "English programmer's keyboard."
 nil t nil nil t nil nil nil nil nil t)

(quail-define-rules
 ("!" ?1)
 ("@" ?2)
 ("#" ?3)
 ("$" ?4)
 ("%" ?5)
 ("^" ?6)
 ("&" ?7)
 ("*" ?8)
 ("(" ?9)
 (")" ?0)
 ("1" ?!)
 ("2" ?@)
 ("3" ?#)
 ("4" ?$)
 ("5" ?%)
 ("6" ?^)
 ("7" ?&)
 ("8" ?*)
 ("9" ?\()
 ("0" ?\)))

(quail-define-package
 "latin-macrons" "Latin with macrons" "LA" t
 "Latin with macrons."
 nil t nil nil t nil nil nil nil nil t)

(quail-define-rules
 ("1" ?!)
 ("2" ?ō)
 ("3" ?ū)
 ("4" ?$)
 ("5" ?%)
 ("6" ?^)
 ("7" ?&)
 ("8" ?ā)
 ("9" ?ī)
 ("0" ?ē))
