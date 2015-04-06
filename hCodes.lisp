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

(defun htree-symbols (mess)
  (htree-freq mess nil))

(defun htree-freq (mess pfl)
  (cond ((endp mess) pfl)
	(t (htree-freq (rest mess)(update (first mess) pfl)))))

(defun update (weight freq-list)
  (cond ((endp freq-list) (list(list(list weight) 1)))
	((freq-equal weight (first(first freq-list))) (cons (incpair(first freq-list))(rest freq-list)))
	( t (cons (first freq-list)(update weight (rest freq-list))))))

(defun freq-equal (weight p) (equal weight (first p)))

(defun incpair (p) (list (first p)(1+ (second p))))

(defun htree-weight (htree)
  (cond ((endp htree) 0)
	( t (weight-inc(first(last(first(first htree))))) htree)))

(defun weight-inc (weight htree)
  (cons(htree(last(weight)))) (htree-weight(rest(first))))
			 

(defun htree-less (htree1 htree2)
  (cond ((< (first(last htree1)) (first(last htree2))))))


(defun root (htree)
  )

(defun htree-sort (htrees)
  (sort (copy-seq htrees) #'htree-less))

(defun make-huffman-tree (mess)
  (htree-sort(htree-symbols mess)))
  
(defun start-merge (htrees)
  (htree-merge (first htrees) (rest htrees)))

(defun htree-merge (htree1 htree2)
  (cond ((and(eql nil htree1) (eql nil htree2)) 0)
  (t (list(list(list(first(first htree1)) (first(first(first htree2)))) ( + (first(last htree1)) (first(last(first htree2)))))(htree-merge (rest htree1) (rest htree2))))))


(defun leaf-p (htree)
  (cond ((= (length (first htree)) 1))))

(defun left-subhtree (htree)
  (first htree))

(defun right-subhtree (htree)
  (last htree))



		
  



