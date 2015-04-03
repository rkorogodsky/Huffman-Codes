;;; HERE IS A NON-ABSTRACTED AND UNDOCUMENTED VERSION OF HOW TO MAKE THE INITIAL 
;;; FREQUESCY LIST.  DO NOT USE THIS CODE AS-IS AS IT IS NOT ABSTRACTED.  HOWEVER,
;;; YOU MIGHT FIND THE CODE USEFUL TO LOOK AT.



;;;  GENERATE THE FREQUENCY LIST FROM THE MESSAGE


;; This code is deliberately written without comments and "fairly badly" so as not to make it too clear.

(defconstant message 
  '(
              no yes not yes yes not no no no is is hello yes no no yes not no
              not no hello is goodbye no yes when no yes not no not no
              no yes not is why why why yes is how is yes is why no yes not
              no no is is not no yes no yes hello yes hello yes no no yes
              no hello goodbye no is goodbye no yes when
    ))

(defconstant htreeTest
  '(
     ((((NO) 23) ((YES) 17) ((NOT) 9) ((IS) 10) ((HELLO) 5)))
    ))

(Defun freqlist (mess)
  (fl mess nil))

(defun fl (m pfl)
  (cond ((endp m) pfl)
	(t (fl (rest m)(update (first m) pfl)))))

(defun update (w fl)
  (cond ((endp fl) (list(list(list w) 1)))
	((fequal w (first(first fl))) (cons (incpair(first fl))(rest fl)))
	( t (cons (first fl)(update w (rest fl))))))

(defun fequal (w p) (equal w (first p)))

(defun incpair (p) (list (first p)(1+ (second p))))

(defun htree-weight (htree)
  (cond ((endp htree) 0)
	( t (+  (first (last(first(first(first htree))))) (first(last(last(first(last(rest(first(first htree))))))))

(defun htree-less (htree1 htree2)
  



