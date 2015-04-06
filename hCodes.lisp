;;; Program written by Royce Korogodsky
;;; Huffman Codes
;;; CSC 345

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
     (((NO) 23) ((YES) 17) ((NOT) 9) ((IS) 10) ((HELLO) 5) 0)
    ))

(defun htree-symbols (mess)
  (htree-freq mess nil))

(defun htree-freq (mess pfl)
  (cond ((endp mess) pfl)
	(t (htree-freq (rest mess)(update (first mess) pfl)))))

(defun update (weight freq-list)
  (cond ((endp freq-list) (list(list(list weight) 1)))
	((freq-equal weight (first(first freq-list))) (cons (incpair(first freq-lisy))(rest freq-list)))
	( t (cons (first freq-list)(update weight (rest freq-list))))))

(defun freq-equal (weight p) (equal w (first p)))

(defun incpair (p) (list (first p)(1+ (second p))))

(defun htree-weight (htree)
  (cond ((endp htree) 0)
	( t (weight-inc(first(last(first(first htree))))) htree)))

(defun weight-inc (weight htree)
  (cons(htree(last(weight)))) (htree-weight(rest(first))))
			 

(defun htree-less (htree1 htree2)
  (cond ((null htree1) (not (null htree2)))
	((null htree2) nil)
	(( = (first(last(first(first htree1)))) (first(last(first(first htree2)))) (htree-less (first htree1))))
											      (rest(first htree2)))))))
	(t ( < (first(last(first(first htree1)))) (first(last(first(first htree2))))))))
						   
			    

(defun htree-symbols (htree)
  (first(last(first(first htree))))

(defun root (htree)
  )

(defun htree-sort (htrees)
  (sort (copy-seq htrees) #'htree-less)))

(defun make-huffman-tree (mess)
  (freqlist mess))

(defun htree-merge (htree1) (htree2)
       (assert(not(null branches)))
       (if(null(rest branches))
	  (first branches)
	  (htree-merge (htree-sort
			(cons (htree-merge (first branches)
					   (second branches)
					   (rest (rest branches))))))))

(defun leaf-p (htree)
  )

(defun left-subhtree (htree)
  (first htree)
  )

(defun right-subhtree (htree)
  (last htree)
  )



		
  



