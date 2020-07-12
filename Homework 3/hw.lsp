(defun goal-test (s)
    (cond 
    	((null s) t)
    	((> (+ (count 2 (first s)) (count 3 (first s))) 0) nil)
    	(t (goal-test (rest s)))
  	)
  );end defun

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;

; Description: This function returns the value of the square at row r and column c in state s.
; Logic: This function cuts away the first r rows and c cols, and returns the value
; at that square.
(defun get-square (s r c)
	(cond 
		((or (< r 0) (< c 0) (not(nthcdr c (first(nthcdr r s))))) 1)
		(t (first (nthcdr c (first(nthcdr r s)))))
	)
)

; Description: This function sets the value of the square at row r and column c in state s to v.
; Logic: This function appends the first r-1 rows and c-1 cols to the new value v, and appends to that
; the remainder of the rows and columns.
(defun set-square (s r c v)
	(let ((col (first(nthcdr r s))))
	(append (butlast s (- (length s) r)) (list (append (butlast col (- (length col) c)) (list v) (nthcdr (+ c 1) col) nil)) (nthcdr (+ r 1) s)))
)

; Description: This function takes a state s and a direction d (0 is up, 1 is down, 2 is left, 3 is right)
; and returns a new state where the keeper has successfully moved in that direction or nil if he cannot
; Logic: This function checks the 4 squares around the keeper for an empty space or a goal (where it can successfully move), 
; a wall (where it cannot move), or a box. If there is a box, it checks the space after that box in the same direction to 
; determine if the box can move, where if it can (due to a goal or empty space), the function returns that state. Otherwise, 
; it returns nil.
(defun try-move (s d)
	(let* ((pos (getKeeperPosition s 0))
		(y (car pos))
		(x (cadr pos)))
		(let* (
			(currSquare (cond ((= (get-square s x y) 3) 0) (t 4)))
			(new_x (cond ((= d 2) (- x 1)) ((= d 3) (+ x 1)) (t x)))
		 	(new_y (cond ((= d 0) (- y 1)) ((= d 1) (+ y 1)) (t y)))
		 	(far_x (cond ((= d 2) (- x 2)) ((= d 3) (+ x 2)) (t x))) 
		    (far_y (cond ((= d 0) (- y 2)) ((= d 1) (+ y 2)) (t y))))
		 	(let* ((near (get-square s new_x new_y)) (far (get-square s far_x far_y))) 
	 			(cond 
	 				((= near 0) (set-square(set-square s new_x new_y 3) x y currSquare))
	 				((= near 1) nil)
	 				((= near 2) 
	 					(cond
	 						((= far 0) (set-square(set-square(set-square s new_x new_y 3) x y currSquare) far_x far_y 2))
	 						((= far 4) (set-square(set-square(set-square s new_x new_y 3) x y currSquare) far_x far_y 5))
	 						(t nil)			
	 					)
	 				)
	 				((= near 4) (set-square(set-square s new_x new_y 6) x y currSquare))
	 				(t 
	 					(cond
	 						((= far 0) (set-square(set-square(set-square s new_x new_y 6) x y currSquare) far_x far_y 2))
	 						((= far 4) (set-square(set-square(set-square s new_x new_y 6) x y currSquare) far_x far_y 5))
	 						(t nil)			
	 					)
	 				)
	 			)
		 	)
		)
	)
)

; Description: This function returns a list of the valid states that the keeper can move to from state s.
; Logic: This function creates a list by calling try-move on each of the four directions, and removes the nils.
(defun next-states (s)
    (cleanUpList (list (try-move s 2) (try-move s 1) (try-move s 3) (try-move s 0)))
 )

; Description: This function computes the trivial admissible heuristic.
; Logic: Return 0.
(defun h0 (s)
	0
)

; Description: This function computes the number of misplaced boxes in s.
; Logic: This function counts the number of 2s (boxes) in s.
; This heuristic is admissible because no matter what it will take at least 
; as many moves as there are misplaced boxes to move each box to a goal. Thus,
; h1 will always be less than the actual number of moves it would take to complete 
; the game.
(defun h1 (s)
	(cond 
    	((null s) 0)
    	(t (+ (count 2 (first s)) (h1 (rest s))))
  	)
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.

; Description: This function uses the minimum distance it would take for the keeper/boxes to reach a goal state 
; in s as the heuristic.
; Logic: This function obtains a list of the locations of the keeper, boxes, and goals by calling getLocs, and 
; then passes in these locations to sumMin to obtain the minimum possible distance for each keeper/box to reach 
; a goal.
(defun h504742401 (s)
	(let* ((locs (getLocs s 0)))
		(sumMin (first locs) (second locs))
	)
)
 
; Description: This function calculates the (row col) of the keeper and the boxes in s in the first part of 
; the list and the (row col) of the goals in the second part of the list.
; Logic: This function goes through each row in s starting at index row, and calls getLocCols on it. It then 
; appends all these rows together. An example of the format is shown below:
; ( ((r, c), (r, c), (r, c))  ((r, c) (r,c) (r,c)) )
;   |    Player + boxes    |        Goals 		 |
(defun getLocs (s row)
	(cond
		((null s) '(nil nil))
		(t (let* ((col (getLocCols (first s) row 0)) (more (getLocs (rest s) (+ row 1))))
			(list (append (first col) (first more)) (append (second col) (second more)))
			)
		)
	)
)

; Description: This function calculates the col of the keeper and the boxes in r and appends it to the passed-in row 
; in the first part of the list. It then does the same for goals in the second part of the list. 
; Logic: It checks each element for its value and appends it to the proper part of the overall list. It then calls itself 
; on the remainder of the list, eventually proceeding through the entire thing.
(defun getLocCols (r row col)
	(cond ((null r) '(nil nil))
	(t
			(cond
			((or (= (car r) 2) (= (car r) 3)) (let* ((l (getLocCols (cdr r) row (+ col 1))))
				(list (append (first l) (list(list row col))) (second l))))
			((= (car r) 4) (let* ((l (getLocCols (cdr r) row (+ col 1)))) (list (first l) (append (second l) (list(list row col))))))
			(t (getLocCols (cdr r) row (+ col 1)))
			)
	     
	   )
	)
)

; Description: This function takes a list of the keeper and boxes as l1 and a list of the goals as l2, and calculates the 
; minimum sum of the distance required for each keeper and box to reach a goal.
; Logic: This function calls getMin for each element in l1 to calculate its minimum distance to a goal, and then sums up 
; all these minimums.
(defun sumMin (l1 l2)
	(cond
		((null l1) 0)
		(t (+ (sumMin (rest l1) l2) (getMin l2 (first (first l1)) (second(first l1)) -1)))
	)
)

; Description: This function takes a list of the goals as l, the row and column of the box/keeper as r and c respectively,
; and the current minimum (starts at -1 as base case). It uses these parameters to calculate the minimum distance to a goal.
; minimum sum of the distance required for each keeper and box to reach a goal.
; Logic: This function calculates the distance between r and c, and each element in l, updating the minimum as it recurses through
; the list. Upon scanning through the entire list, it returns the minimum.
(defun getMin (l r c minimum)
	(cond 
		((null l) minimum)
		(t (let* ((row (first(first l))) (col (second(first l))))
			(let* ((rowSum (cond ((> (- r row) 0) (- r row)) (t (- row r))))
				(colSum (cond ((> (- c col) 0) (- c col)) (t (- col c)))))
				(cond 
					((or (< (+ rowSum colSum) minimum) (= minimum -1)) (getMin (rest l) r c (+ rowSum colSum)))
					(t (getMin (rest l) r c minimum))
				)
			)
		))
	)
)




