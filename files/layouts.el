(register-input-method
 "devanagari-translit" "Devanagari"
 'quail-use-package
 "DevLAT" "Devanagari transliteration to latin"
 "~/.emacs.d/files/layouts/devanagari-translit")

(register-input-method
 "english-prog" "English"
 'quail-use-package
 "EN-P" "English programmer's keyboard."
 "~/.emacs.d/files/layouts/english-prog")

(register-input-method
 "latin-macrons" "Latin"
 'quail-use-package
 "LA" "Latin with macrons."
 "~/.emacs.d/files/layouts/latin-macrons")

(eval-after-load "quail/latin-ltx"
  '(quail-define-rules
    ((append . t))
    ("\\Bbb{Q}" ?ℚ)
    ("\\,a" ?ạ) ("\\,b" ?ḅ) ("\\,d" ?ḍ) ("\\,e" ?ẹ) ("\\,h" ?ḥ) ("\\,i" ?ị)
    ("\\,k" ?ḳ) ("\\,l" ?ḷ) ("\\,m" ?ṃ) ("\\,n" ?ṇ) ("\\,o" ?ọ) ("\\,r" ?ṛ)
    ("\\,s" ?ṣ) ("\\,t" ?ṭ) ("\\,u" ?ụ) ("\\,v" ?ṿ) ("\\,w" ?ẉ) ("\\,y" ?ỵ)
    ("\\,z" ?ẓ) ("\\,A" ?Ạ) ("\\,B" ?Ḅ) ("\\,D" ?Ḍ) ("\\,E" ?Ẹ) ("\\,H" ?Ḥ)
    ("\\,I" ?Ị) ("\\,K" ?Ḳ) ("\\,L" ?Ḷ) ("\\,M" ?Ṃ) ("\\,N" ?Ṇ) ("\\,O" ?Ọ)
    ("\\,R" ?Ṛ) ("\\,S" ?Ṣ) ("\\,T" ?Ṭ) ("\\,U" ?Ụ) ("\\,V" ?Ṿ) ("\\,W" ?Ẉ)
    ("\\,Y" ?Ỵ) ("\\,Z" ?Ẓ) (".a" ?ạ) (".b" ?ḅ) (".d" ?ḍ) (".e" ?ẹ)
    (".h" ?ḥ) (".i" ?ị) (".k" ?ḳ) (".l" ?ḷ) (".m" ?ṃ) (".n" ?ṇ)
    (".o" ?ọ) (".r" ?ṛ) (".s" ?ṣ) (".t" ?ṭ) (".u" ?ụ) (".v" ?ṿ)
    (".w" ?ẉ) (".y" ?ỵ) (".z" ?ẓ) (".A" ?Ạ) (".B" ?Ḅ) (".D" ?Ḍ)
    (".E" ?Ẹ) (".H" ?Ḥ) (".I" ?Ị) (".K" ?Ḳ) (".L" ?Ḷ) (".M" ?Ṃ)
    (".N" ?Ṇ) (".O" ?Ọ) (".R" ?Ṛ) (".S" ?Ṣ) (".T" ?Ṭ) (".U" ?Ụ)
    (".V" ?Ṿ) (".W" ?Ẉ) (".Y" ?Ỵ) (".Z" ?Ẓ)
    ;; ("qq" 'my-noninteractive-toggle-input-method)
    ))

(eval-after-load "quail/slovak"
  '(progn
     (quail-select-package "slovak-prog-2")
     (quail-define-rules
      ((append . t))
      ("1" ?ú)
      ("[" ?ď)
      (";" ?ň)
      ("=1" ?1))))

(eval-after-load "quail/indian"
  '(progn
     (quail-select-package "devanagari-aiba")
     (quail-define-rules
      ((append . t))
      ("\\\\" ?√))

     (quail-select-package "devanagari-kyoto-harvard")
     (quail-define-rules
      ((append . t))
      ("\\\\" ?√))))
