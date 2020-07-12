; Name : Sum Li (UID : 505146702)
; CS 161 Homework 2

; Problem 1 : single pure Lisp function called bfs (come back)

; Overall comment : First, we need to check two base cases. The first base case is when the tree is empty, return NIL.
; The second base case is when the tree has only one atom, return a list with the atom.
; For the recursive case, if the first node of the tree is a list, then reconstruct the list by appending the
; the rest of the list followed with the head of the list before preforming the bfs on the new list.
; If the first node of the tree is not a list, then add the first node to the list and recursively
; call bfs with the rest of the nodes from tree

; Header comment :
	; Argument : one argument, the list representation of the tree 
	; Returned Value : a list of terminal nodes in the order visited by a left to right breadth first search. 

(defun bfs (tree_input)
	(cond 
		  ; if tree is empty, return NIL
		  ((null tree_input) NIL)
		  ; if tree is an atom, return the list with atom
		  ((atom tree_input) (list tree_input))
		  ; if head of tree is a list, reconstruct the list before bfs
		  ((listp (car tree_input)) (bfs (append (cdr tree_input) (car tree_input))))
		  ; if head of tree is not a list, prepend the head and recursively search the rest of the tree 
		  (t (cons (car tree_input) (bfs (cdr tree_input))))
	)
)

; Problem 2 : single pure Lisp function called dfs 

; Overall comment : First, we need to check two base cases. The first base case is when the tree is empty, return NIL.
; The second base case is when the tree has only one atom, return a list with the atom. 
; For the recurisve case, reconstruct the list by appending the dfs of the rest of the tree and the 
; dfs of the head of the tree because the visit order is right to left dfs

; Header comment :
	; Argument : one argument, the list representation of the tree
	; Returned Value : a single top level list of terminal nodes in the order they visit by a 
	; 				   right to left depth first search. 

(defun dfs (tree_input)
	(cond 
		  ; if tree is empty, return NIL
		  ((null tree_input) NIL)
		  ; if tree is an atom, return the list with atom
		  ((atom tree_input) (list tree_input))
		  ; create the list recursively search the rest of tree before head of tree
		  (t (append (dfs (cdr tree_input)) (dfs (car tree_input))))
	)
)

; Problem 3 : two functions that implement depth-first iterative-deepening called dfid_helper and dfid

; Overall comment : First, we need to check four base cases. The first base case is when subtree is empty, just return NIL.
; The second base case is when the given limited depth is less than zero, return NIL.
; The third base case is when the subtree is an atom, return the list with the atom
; The fourth base case is when the limited depth is zero, return NIL
; The recrsive case is create a list by append the head of the subtree list with decrementing depth follow by the rest of subtree list 

; Header comment :
	; Argument : two arguments, the list representation of the tree,
	;            an integer representing the current depth of the tree which is limited 
	; Returned Value : a single top-level list of the terminal node in order that
	;                  visited by a left to right dfs iterative-deepening search at a limited depth 

(defun dfid_helper (tree_input depth)
	(cond 
	      ; if subtree is empty, return NIL
		  ((null tree_input) NIL)
		  ; if limited depth is less than 0, return NIL
		  ((< depth 0) NIL)
		  ; if subtree is atom, return list with atom
		  ((atom tree_input) (list tree_input))
		  ; if limited depth is 0, return NIL
		  ((= depth 0) NIL)
		  ; create the list by recursively append the head of list and rest of list
		  (t (append (dfid_helper (car tree_input) (- depth 1)) (dfid_helper (cdr tree_input) depth)))
	)
)

; Overall comment : First, we need to check three base cases. The first base case is when the tree is empty, return NIL.
; The second base case is when the depth is less than zero, return NIL
; The third base case is when the depth is equal to zero, return NIL
; The recursive case is create a list by append the list with decrementing depth followed by the sublist return from dfid_helper

; Header comment : 
	; Argument : two arguments, the list representation of the tree, 
	;            an integer representing the maximum depth of the tree
	; Returned Value : a single top-level list of the terminal node in the order
	; 				   that visited by a left-to-right depth first iterative-deepening search

(defun dfid (tree_input depth)
	(cond 
		  ; if tree is empty, return NIL
		  ((null tree_input) NIL)
		  ; if depth is less than 0, return NIL
		  ((< depth 0) NIL)
		  ; if dpeth is 0, return NIL
		  ((= depth 0) NIL)
		  ; create the list by recursively append the list with the sublist from the helper function
		  (t (append (dfid tree_input (- depth 1)) (dfid_helper tree_input depth)))
	)
)

; These functions implement a depth-first solver for the missionary-cannibal
; problem. In this problem, three missionaries and three cannibals are trying to
; go from the east side of a river to the west side. They have a single boat
; that can carry two people at a time from one side of the river to the
; other. There must be at least one person in the boat to cross the river. There
; can never be more cannibals on one side of the river than missionaries. If
; there are, the cannibals eat the missionaries.

; In this implementation, a state is represented by a single list
; (missionaries cannibals side). side represents which side the boat is
; currently on, and is T if it is on the east side and NIL if on the west
; side. missionaries and cannibals represent the number of missionaries and
; cannibals on the same side as the boat. Thus, the initial state for this
; problem is (3 3 T) (three missionaries, three cannibals, and the boat are all
; on the east side of the river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function MC-DFS, which is called
; with the initial state to search from and the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, MC-DFS returns NIL.

; To call MC-DFS to solve the original problem, one would call (MC-DFS '(3 3 T)
; NIL) -- however, it would be possible to call MC-DFS with a different initial
; state or with an initial path.

; Examples of calls to some of the helper functions can be found after the code.

; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
	(equal '(3 3 NIL) s)
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
	; Set up variables for expression use for more than once
	(let ((missionaries (first s)) (cannibals (second s)) 
		  (next_missionaries (+ (- 3 (first s)) m)) 
		 )
			(cond 
				  ; attempt to transfer more missionary than given m
				  ((< missionaries m) NIL)
				  ; attempt to transfer more cannibal than given c
				  ((< cannibals c) NIL)
				  ; the boat need to have at least 2 people
				  ((> 1 (+ c m)) NIL)
				  ; at the side we are standing, there are less missionaries than cannibals after the transfer 
				  (( and (< (- missionaries m) (- cannibals c) ) (> (- missionaries m) 0) ) NIL)
				  ; at the opposite side, there are less missionaries than cannibals after the transfer
				  ((and (< next_missionaries (+ (- 3 (second s)) c)) (> next_missionaries 0) ) NIL)
				  ; create the list based on the side you are standing at either original or opposite 
				  (t (list (list (+ (- 3 missionaries) m) (+ (- 3 cannibals ) c) (not (third s)))))
			)
	)
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun succ-fn (s)
	(append (next-state s 0 1)
		    (next-state s 1 0)
		    (next-state s 1 1)
		    (next-state s 0 2)
		    (next-state s 2 0)
	)
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
	(cond 
		  ; if the states are empty, return empty
		  ((null states) NIL)
		  ; if the top of the state is equal to current state, return t
		  ((equal (car states) s) t)
		  ; recursively search the rest of the stack of states 
		  (t (on-path s (cdr states)))
	)
)

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states from the current state (states).
; MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL. 
; Note that the path should be ordered as: (S_n ... S_2 S_1 S_0)

; Substitute with variables to make it clear 
(defun mult-dfs (states path)
	(cond 
		  ; if the states are empty, return NIL
		  ((null states) NIL)
		  ; if inital state match the goal state 
		  ((final-state (car states)) (append (list (car states)) path))
		  ; if inital state is a member of stack of states
		  ((on-path (car states) path) (mult-dfs (cdr states) path))
		  ; if mult-dfs return a empty list, search the rest of stack of states with the path
		  ((null (mult-dfs (succ-fn (car states)) (append (list (car states)) path))) (mult-dfs (cdr states) path))
		  ; if mult-dfs return an non-empty list 
		  (t (mult-dfs (succ-fn (car states)) (append (list (car states)) path)))
	)
)

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
	(cond 
		  ; check if state s is in the path 
		  ((on-path s path) NIL)
		  ; check if state s is the goal state 
		  ((final-state s) path)
		  ; return the ordered path ((3 3 NIL) ... (3 3 T))
		  (t (mult-dfs (succ-fn s) (append path (list s))))
	)
)

; Function execution examples

; Applying this operator would result in an invalid state, with more cannibals
; than missionaries on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL

; Applying this operator would result in one cannibal and zero missionaries on
; the west side of the river, which is a legal operator. (NOTE that next-state
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))

; succ-fn returns all of the legal states that can result from applying
; operators to the current state.
; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))
