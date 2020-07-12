; Name : Sum Li (UID : 505146702)
; CS 161 Homework 1

; Problem 1 : Boolean function called TREE-CONTAINS

; Overall comment : First, we need to check two base cases. The first base case is when TREE is empty, return NIL. 
; The second base case is when TREE is left with a single number, then we will check the number with N. If the number is same as N, return T.
; If N is the same as the middle number, return T 
; For the recursive case, if N is smaller than the middle number of the tree, search the left side of the tree.
; If N is greater than the middle number of the tree, search the right side of the tree.

; Header comment : 
	; Argument : two arguments, number N and ordered tree TREE
	; Returned Value : if N is in the TREE, then return T
	; 				   else N is not in the TREE, then return NIL 

(defun TREE-CONTAINS (N TREE)
	(cond ((null TREE) NIL)
		  ((numberp TREE) (= N TREE))
		  ((= N (car(cdr TREE))) t)
		  ((< N (car(cdr TREE))) (TREE-CONTAINS N (car TREE)))
		  (t (TREE-CONTAINS N (car (cddr TREE))))
	)
)

; Problem 2 : function called TREE-MIN

; Overall comment : First, we need to check two base cases. 
; If TREE is empty, then we just return NIL
; If TREE only left with a single number, then return the number which is the TREE
; Since it is a sorted tree, the minimum number located at the leftmost side of the TREE
; So, we just recursively search the left half of the TREE

; Header comment : 
	; Argument : one argument, TREE
	; Returned Value : the minimum number appearing in the TREE

(defun TREE-MIN (TREE)
	(cond ((null TREE) NIL)
		  ((numberp TREE) TREE)
		  (t (TREE-MIN (car TREE))) 
	)
)

; Problem 3 : function called TREE-ORDER (pre-ordered)

; Overall comment : First, we need to check two base cases.
; If TREE is empty, then we just return NIL
; If TREE only has one single element, then return a new list with that single element
; For the last case, we just recursively traverse the tree in pre-ordered and append the element to the list

; Header comment :
	; Argument : one argument, TREE
	; Returned Value : an pre-ordered list of the numbers appearing in the ordered TREE

(defun TREE-ORDER (TREE)
	(cond ((null TREE) NIL)
		  ((numberp TREE) (list TREE))
		  (t (append (TREE-ORDER (cadr TREE)) (TREE-ORDER (car TREE)) (TREE-ORDER (third TREE))))
	)
)

; Problem 4 : function called SUB-LIST (Assume first element has position 0)

; Overall comment : First, we need to check two base cases.
; If the list L is empty, then we just return NIL
; If LEN is 0, then we return NIL
; If the start position is 0, then the sublist starts at the inital position, so we connect the head with the
; rest of the list by shortening up the LEN
; If the start position is not zero, we just recursively search the rest of the list and check the element locate at START position
; by decrement the START each recrusive call 

; Header comment :
	; Argument : three arguments, a list L, two non-negative integers START and LEN
	; Returned Value : the sub-list of L start at position START and have length LEN

(defun SUB-LIST (L START LEN)
	(cond ((null L) NIL)
		  ((equal LEN 0) NIL)
		  ((equal START 0) (cons (first L) (SUB-LIST (rest L) START (- LEN 1))))
		  (t (SUB-LIST (rest L) (- START 1) LEN))
	)
)

; Problem 5 : function called SPLIT-LIST

; Overall comment : First, we need to check one base case.
; If the list L is empty, then  we just return NIL. 
; If the length of list L is even, then we can evenly divide the list into two separate equal length list
; If the length of list L is odd, then we divide the into two separte lists such that the first list length
; is one greater than the second list length. 

; Header comment : 
	; Argument : one argument, a list L
	; Returned Value : return a list of two lists L1 and L2 in order

(defun SPLIT-LIST (L)
	(cond ((null L) NIL)
		  ((evenp (length L)) (list (SUB-LIST L 0 (/ (length L) 2)) (SUB-LIST L (/ (length L) 2) (/ (length L) 2))))
		  (t (list (SUB-LIST L 0 (/ (+ (length L) 1) 2)) (SUB-LIST L (/ (+ (length L) 1) 2) (/ (- (length L) 1) 2))))
	)
)

; Problem 6 : function called BTREE-HEIGHT 

; Overall comment : First, we need to check two base cases.
; If the TREE only have a node, return 0 as the height
; If not, recursively search the left subtree and right subtree. 
; Compare the height of the left and right subtree.
; If the height of left subtree greater than the height of right subtree, then increment left-tree one
; If the height of right subtree greater than the height of left subtree, then increment right-tree one

; Header comment : 
	; Argument : one argument, a binary tree TREE
	; Returned Value : return the height of the TREE, the length of the longest path from
	;				   the root node to the farthest leaf node 
	; Side note : left-tree is left subtree, right-tree is right subtree

(defun BTREE-HEIGHT (TREE)
	(cond ((null TREE) 0)
		  ((atom TREE) 0)
		  (t (let ((left-tree (BTREE-HEIGHT (car TREE))) (right-tree (BTREE-HEIGHT (car (cdr TREE)))))
		  		(cond ((> left-tree right-tree) (+ left-tree 1))
		  			  (t (+ right-tree 1))
		  		)
		  	)
		  )
	)
)

; Problem 7 : function called LIST2BTREE

; Overall comment : First, we need to check two base casess.
; If the list only have one node which is a leaf node, we return the atom back
; If the list have two nodes which is a internal node, we just return the LEAVES back
; If not, we use SPLIT-LIST to recursively divide the list into half, then call
; LIST2BTREE to construct the TREE each recursive call

; Header comment : 
	; Argument : one argument, a non-empty list of atoms LEAVES 
	; Returned Value : return a binary tree such that the tree leaves are the elements of LEAVES.
	; 				   For the internal nodes in the tree, the number of leaves in left branch 
	; 				   minus the number of leaves in right branch is 0 or 1

(defun LIST2BTREE (LEAVES)
	(cond ((equal (length LEAVES) 1) (car LEAVES))
	      ((equal (length LEAVES) 2) LEAVES)
	      (t (list (LIST2BTREE (car (SPLIT-LIST LEAVES))) (LIST2BTREE (car (cdr (SPLIT-LIST LEAVES))))))
	)
)

; Problem 8 : function called BTREE2LIST

; Overall comment : First, we need to consider three base cases. 
; If the TREE is empty, we just return NIL
; If the TREE only contains an atom, return a list with that atom
; If the TREE has a list of length one, return the list back
; If not, recursively search through the left and right subtree and 
; append to the list along each recursive call. 

; Header comment : 
	; Argument : one argument, a binary tree TREE 
	; Returned Value : return a list of atom based from TREE

(defun BTREE2LIST (TREE)
	(cond ((null TREE) NIL)
		  ((atom TREE) (list TREE))
		  ((equal (length TREE) 1) TREE)
		  (t (append (BTREE2LIST (car TREE)) (BTREE2LIST (car (cdr TREE)))))
	)
)

; Problem 9 : Boolean LISP function called IS-SAME

; Overall comment : There are five base cases.
; If both expressions are empty, just return T
; If either one of the expression is empty, but not the other expression is not empty, return NIL
; If either one of the expression is atom, but the other expression is a list, return NIL
; If both of them are atom, then proceed to check if they are the same 
; If both of them are list, then proceed to recursively through E1 and E2 by splitting them 
; into head and rest of list 
; All of the rest cases are considered NIL, E1 and E2 are not equal in this case 

; Header comment :
	; Argument : two arguments, two LISP expression E1 and E2, whose atoms are all numbers  
	; Returned Value : return T or NIL to check whether E1 and E2 are the same 

(defun IS-SAME (E1 E2)
	(cond ((and (null E1) (null E2)) T)
		  ((or (and (null E1) (not (null E2)) ) (and (null E2) (not (null E1)))) NIL)
		  ((and (atom E1) (listp E2)) NIL)
		  ((and (atom E2) (listp E1)) NIL)
		  ((and (atom E1) (atom E2)) (= E1 E2))
		  ((and (listp E1) (listp E2)) (and (IS-SAME (first E1) (first E2)) (IS-SAME (rest E1) (rest E2))))
		  (t NIL)
	)
)

