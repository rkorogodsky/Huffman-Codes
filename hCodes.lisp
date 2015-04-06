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
  "Function Name: HTree-Symbols"
  "Description: Calls frequency function to get a count of each instance"
  "Input Params: String"
  "Output Params: String List"
  (htree-freq mess nil))

(defun htree-freq (mess pfl)
  "Function Name: HTree-Freq"
  "Description: Constructs list using message passed from HTree-Symbols"
  "Input Params: String list(passed from htree-symbols)"
  "Output Params: list list" 
  (cond ((endp mess) pfl)
	(t (htree-freq (rest mess)(update (first mess) pfl)))))

(defun update (weight freq-list)
  "Function Name: Update"
  "Description: Updates frequency list with new values passed from message."
  "Input Params: list list"
  "Output Params: list list"
  (cond ((endp freq-list) (list(list(list weight) 1)))
	((freq-equal weight (first(first freq-list))) (cons (incpair(first freq-list))(rest freq-list)))
	( t (cons (first freq-list)(update weight (rest freq-list))))))

(defun freq-equal (weight p)
  "Function Name: Freq-Equal"
  "Description: Checks if two lists passed from update are equal."
  "Input Params: list list"
  "Output Params: boolean"
  (equal weight (first p)))

(defun incpair (p)
  "Function Name: incpair"
  "Description: Increments list to show how often a string shows up in the message."
  "Input Params: list"
  "Output Params: list"
  (list (first p)(1+ (second p))))

(defun htree-weight (htree)
  "Function Name: htree-weight"
  "Description: Doesn't work currently. Will be able to add values of an htree and update list"
  "Input Params: list"
  "Output Params: list"
  (cond ((endp htree) 0)
	( t (weight-inc(first(last(first(first htree))))) htree)))

(defun weight-inc (weight htree)
  "Function Name: weight-inc"
  "Description: Doesn't work currently. Helper function designed to increment weight of htree."
  "Input Params: list list"
  "Output Params: list list"
  (cons(htree(last(weight)))) (htree-weight(rest(first))))
			 

(defun htree-less (htree1 htree2)
  "Function Name: htree-less"
  "Description: Checks to see which htree is less then the other."
  "Input Params: list list"
  "Output Params: boolean"
  (cond ((< (first(last htree1)) (first(last htree2))))))


(defun root (htree)
  "Function Name: Root"
  "Description: Will be used to output all of the leaves of the htree."
  "Input Params: list"
  "Output Params: list"
  )

(defun htree-sort (htrees)
  "Function Name: htree-sort"
  "Description: Sorts the htrees from smallest to largest"
  "Input params: list"
  "Output Params: list"
  (sort (copy-seq htrees) #'htree-less))

(defun make-huffman-tree (mess)
  "Function Name: make-huffman-tree"
  "Description: Main function to create the huffman tree"
  "Input Params: String"
  "Output Params: list"
  (htree-sort(htree-symbols mess)))
  
(defun start-merge (htrees)
  "Function Name: start-merge"
  "Description: Doesn't work. Helper function designed to start process of merging htrees"
  "Input Params: list"
  "Output Params: list"
  (htree-merge (first htrees) (rest htrees)))

(defun htree-merge (htree1 htree2)
  "Function Name: htree-merge"
  "Description: Doesn't work. Designed to merge huffman trees to create data structure."
  "Input Params: list list"
  "Output Params: list"
  (cond ((NULL htree1) 0) ((NULL htree2) 0)
  (t (list(list(list(first(first htree1)) (first(first(first htree2)))) ( + (first(last htree1)) (first(last(first htree2)))))(htree-merge htree1 (rest htree2))))))


(defun leaf-p (htree)
  "Function Name: leaf-p"
  "Description: Checks if htree is a leaf."
  "Input Params: list"
  "Output Params: boolean"
  (cond ((= (length (first htree)) 1))))

(defun left-subhtree (htree)
  "Function Name: leaf-subhtree"
  "Description: Used to replicate a zero in binary and traverses left in tree."
  "Input Params: list"
  "Output Params: list"
  (first htree))

(defun right-subhtree (htree)
  "Function Name: htree"
  "Description: Used to replicate a one in binary and traverses right in tree."
  "Input Params: list"
  "Output Params: list"
  (last htree))



		
  



