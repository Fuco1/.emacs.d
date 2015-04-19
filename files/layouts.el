(quail-define-package
 "english-prog" "English programmer" "EN-P" t
 "English programmer's keyboard."
 nil t nil nil t nil nil nil nil nil t)

(quail-define-rules
 ("!" ?1) ("@" ?2) ("#" ?3) ("$" ?4) ("%" ?5) ("^" ?6) ("&" ?7) ("*" ?8) ("(" ?9) (")" ?0)
 ("1" ?!) ("2" ?@) ("3" ?#) ("4" ?$) ("5" ?%) ("6" ?^) ("7" ?&) ("8" ?*) ("9" ?\() ("0" ?\)))

(quail-define-package
 "latin-macrons" "Latin with macrons" "LA" t
 "Latin with macrons."
 nil t nil nil t nil nil nil nil nil t)

(quail-define-rules
 ("1" ?!) ("2" ?ō) ("3" ?ū) ("4" ?$) ("5" ?%) ("6" ?^) ("7" ?&) ("8" ?ā) ("9" ?ī) ("0" ?ē))

(quail-define-package
 "devanagari-translit" "Input method for inputing devanagari transliteration" "DevLAT" t
 "Devanagari transliteration to latin"
 nil t nil nil t nil nil nil nil nil t)

(quail-define-rules
 ;; macrons
 ("aa" ?ā) ("ee" ?ē)("ii" ?ī) ("oo" ?ō) ("uu" ?ū)
 ;; retroflex
 (".t" ?ṭ) (".d" ?ḍ) (".h" ?ḥ)
 ;; nasals
 ("N" ?ṅ) ("nn" ?ñ) (".n" ?ṇ) (".m" ?ṃ)
 ;; sibilants
 ("S" ?ś) (".s" ?ṣ)
 ;; semivowel vowels
 (".l" ?ḷ) (".r" ?ṛ) (".L" ?ḹ) (".R" ?ṝ)
 ;; special
 ("\\\\" ?√))

(set-input-method "TeX")
(quail-define-rules
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
 )

(set-input-method "slovak-prog-2")
(quail-define-rules
 ((append . t))
 ("1" ?ú)
 ("=1" ?1))

(set-input-method "devanagari-aiba")
(quail-define-rules
 ((append . t))
 ("\\\\" ?√))

(set-input-method "devanagari-kyoto-harvard")
(quail-define-rules
 ((append . t))
 ("\\\\" ?√))

(toggle-input-method)
