; Name : Sum Li (UID : 505146702)
; CS 161 Homework 3

;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;

; Overall comment :

; Header comment : 
	; Argument : one argument, given state s
	; Return value : return true if state s equal to goal state which is the
	; 				 game terminating condition 

(defun goal-test (s)
	(cond 
		  ; If the given state is null, then return true
		  ((null s) T)
		  ; Check the row to see if there are any boxes still unhandled, then return NIL immediately
		  ; We have not reach goal state yet 
		  ((and (atom s) (isBox s)) NIL)
		  ((and (atom s) (not(isBox s))) T)
		  ; Recursively search the rest of the symbols on the board
		  (t (and (goal-test (first s)) (goal-test (rest s))))
	)
);end defun

;; testcases for goal-test 
(print (goal-test '((1 1 1) (1 5 1) (1 1 1))))

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

; Overall comment :
; Header comment : 
	; Argument : 3 arguments 
	; Return value : 
(defun set-square-help (r c v)
	(cond ((null r) NIL)
		  ((< c 0) r)
		  ; when column equal to 0
		  ; ((equal c 0) (append v (rest r)))
		  ((equal c 0) (cons v (rest r)))
		  ; when column does not equal to 0
		  ; (t (append (first r) (set-square-help (rest r) (- c 1) v)))
		  (t (cons (first r) (set-square-help (rest r) (- c 1) v)))
	)
)


; Overall comment : Function should not modify input state 
; Header comment : 
	; Argument : 4 arguments, state S, row number r, column number c, square content v
	; Return value : return a new state S' by setting sqaure (r, c) to value v

; be careful of S is empty, might need to take it out 

(defun set-square (S r c v)
	(cond ((null S) NIL)
		  ; check if r is valid, if not, return back S
		  ((< r 0) S)
		  ; check if c is valid, if not, return back S
		  ((< c 0) S) 
		  ; if r equal to 0
		  ; ((equal r 0) (append (set-square-help (first S) c v) (rest S)))
		  ((equal r 0) (cons (set-square-help (first S) c v) (rest S)))
		  ; if r does not equal to 0
		  ; ((not(equal r 0)) (append (first S) (set-square (rest s) (- r 1) c v)) )
		  ((not(equal r 0)) (cons (first S) (set-square (rest s) (- r 1) c v)) )
		  ; (t (append (first S) (set-square (rest s) (- r 1) c v)))

	)
)

; Overall comment :
; Header comment : 
	; Argument :
	; Return value : 

(defun get-square-help (r c)
	(cond 
		  ; if the row is empty, nothing to search
		  ; at position column
		  ((null r) 1)
		  ; check for invalid column position
		  ((< c 0) 1)
		  ; base case, if column is 0, just return the element  
		  ((equal c 0) (first r))
		  ; recursively search the rest of the rows
		  (t (get-square-help (rest r) (- c 1)))
	)
)

; Overall comment : 
; Header comment :
	; Argument : three arguments, a state S, a row number r, a column number c
	; Return value : If square is valid, return the integer content of the state S at square (r, c)
	;                If sqaure is out of scope, return value of a wall which is 1

; be aware of the order of the pass in argument 
(defun get-square (S r c)
	(cond ((null S) 1)
		  ; check if r is valid, if not, return 1
	      ((< r 0) 1)
	      ; check if c is valid, if not, return 1
	      ((< c 0) 1)
	      ; if r equals to 0, call helper function to check each element in the row list 
	      ; pass in r, c order 
	      ((equal r 0) (get-square-help (first S) c))
	      ; if r does not equal to 0, recursively check the next row in the states
	      (t (get-square (rest S) (- r 1) c))
	)
)

;; missing comment 
(defun get_new_keeper_x (x d)
	(cond 
		  ; if move up, no change in x
		  ((equal d 0) x)
		  ; if move down, no change in x
		  ((equal d 1) x)
		  ; if move left, shift x to left by 1
		  ((equal d 2) (- x 1)) 
		  ; if move right, shift y to right by 1
		  ((equal d 3) (+ x 1)) 
		  ; else case, just return back x 
		  (t x)
	)
)

;; missing comment 
(defun get_box_x_pos (x d)
	(cond 
		  ; if move up, no change in x
		  ((equal d 0) x)
		  ; if move down, no change in x
		  ((equal d 1) x)
		  ; if move left, shift x of box to left by 2
		  ((equal d 2) (- x 2))
		  ; if move right, shift y of box to right by 2
		  ((equal d 3) (+ x 2))
		  (t x)
	)
)

;; missing comment 
(defun get_new_keeper_y (y d)
	(cond
		; if move up,
		((equal d 0) (- y 1)) 
		; if move down,
		((equal d 1) (+ y 1))
		; if move left, no change in y
		((equal d 2) y)
		; if move right, no change in y
		((equal d 3) y) 
		; else case, just return back y
		(t y)

	)
)

;; missing comment 
(defun get_box_y_pos (y d)
	(cond 
		((equal d 0) (- y 2))
		((equal d 1) (+ y 2))
		((equal d 2) y)
		((equal d 3) y)
		(t y)
	)
)

;; missing comment 
(defun get_keeper_content (S new_keeper_x_pos new_keeper_y_pos)
	; be careful about the extra parentheses !!
	(get-square S new_keeper_x_pos new_keeper_y_pos)
)

;; missing comment 
(defun get_box_content (S box_x_pos box_y_pos)
	(get-square S box_x_pos box_y_pos)
)

;; missing comment 
(defun get_current_content (S keeper_x_pos keeper_y_pos)
	; flip the condition 
	(cond ((not (equal (get-square S keeper_x_pos keeper_y_pos) 3)) 4) 
		  (t 0)
	)
)

;; missing comment 
(defun handle_blank_or_goal (S keeper_x_pos keeper_y_pos new_keeper_x_pos new_keeper_y_pos new_content current_content)
	(set-square(set-square S new_keeper_x_pos new_keeper_y_pos new_content) keeper_x_pos keeper_y_pos current_content)
)

;; missing comment 
(defun handle_content (S keeper_x_pos keeper_y_pos new_keeper_x_pos new_keeper_y_pos box_x_pos box_y_pos new_content current_content box_content)
	(set-square (set-square (set-square S new_keeper_x_pos new_keeper_y_pos new_content) keeper_x_pos keeper_y_pos current_content) box_x_pos box_y_pos box_content)
)

; Overall comment :
; Header comment :
	; Argument : 2 arguments, state S, a move direction D
	; Return value : If the move is valid, return the state that is result of moving the keeper in state S in direction D 
	; 				 Update the content of every square to the right value 
	; 				 If the move is invalid, return NIL 

(defun try-move (S D)
	; check if D is a valid direction, if not return NIL immediately 
	(cond ((or (< D 0) (> D 3)) NIL)
	; D is a valid direction, continue 
		  (t 
		  	; set up some variables to store the possible coordinates on the board
		  	; 1. Keeper position
		  	; 2. x coordinate of Keeper position
		  	; 3. y coordinate of Keeper position
		  	; 4. updated x coordinate of Keeper position 
		  	; 5. updated y coordinate of Keeper position
		  	; 6. check x coordinate of move box position
		  	; 7. check y coordinate of move box position
		  	; 8. integer content at new Keeper position
		  	; 9. integer content at move box position
		  	(let*  
		  		   (
		  		   (keeper_position (getKeeperPosition S 0)) 
		  		   (keeper_x_pos (car(cdr keeper_position))) 
		  		   (keeper_y_pos (first keeper_position)) 

		  		   (new_keeper_x_pos (get_new_keeper_x keeper_x_pos d) ) 
		  		   (new_keeper_y_pos (get_new_keeper_y keeper_y_pos d) ) 

		  		   (box_x_pos (get_box_x_pos keeper_x_pos d)) 
		  		   (box_y_pos (get_box_y_pos keeper_y_pos d)) 

		  		   (keeper_content (get_keeper_content S new_keeper_x_pos new_keeper_y_pos)) 
		  		   (box_content (get_box_content S box_x_pos box_y_pos)) 
		  		   (current_content (get_current_content S keeper_x_pos keeper_y_pos)))

		  		(cond ((equal keeper_content 0) (handle_blank_or_goal S keeper_x_pos keeper_y_pos new_keeper_x_pos new_keeper_y_pos 3 current_content))
		  			  ((equal keeper_content 1) NIL)
		  			  ((and (equal keeper_content 2) (equal box_content 0)) (handle_content S keeper_x_pos keeper_y_pos new_keeper_x_pos new_keeper_y_pos box_x_pos box_y_pos 3 current_content 2))
		  			  ((and (equal keeper_content 2) (equal box_content 4)) (handle_content S keeper_x_pos keeper_y_pos new_keeper_x_pos new_keeper_y_pos box_x_pos box_y_pos 3 current_content 5))
		  			  ((and (equal keeper_content 2) (not(equal box_content 0)) (not(equal box_content 4))) NIL)
		  			  ((and (equal keeper_content 4) (handle_blank_or_goal S keeper_x_pos keeper_y_pos new_keeper_x_pos new_keeper_y_pos 6 current_content)))
		  			  ((equal box_content 0) (handle_content S keeper_x_pos keeper_y_pos new_keeper_x_pos new_keeper_y_pos box_x_pos box_y_pos 6 current_content 2))
		  			  ((equal box_content 4) (handle_content S keeper_x_pos keeper_y_pos new_keeper_x_pos new_keeper_y_pos box_x_pos box_y_pos 6 current_content 5))
		  			  (t NIL)
		  		)
		  	)
		  )
	)
)

; Overall comment : 
; Header comment :
	; Argument : one argument, given state s
	; Return value : return a list of states that can be reached from the given states in one move

; (defun next-states (s)
;   (let* ((pos (getKeeperPosition s 0))
; 	 (x (car pos))
; 	 (y (cadr pos))
; 	 ;x and y are now the coordinate of the keeper in s.
; 	 ; Try to call try-move in each of the four directions : up, down, left, right
; 	 (result (list (try-move s 0) (try-move s 1) (try-move s 2) (try-move s 3)))
; 	 )
;     (cleanUpList result);end
;    );end let
;  );

(defun next-states (s)
	(cleanUpList (list (try-move s 2) (try-move s 1) (try-move s 3) (try-move s 0)) );end
);

;; Testcases for next-states 
;; Example 1
(print "This is output of example 1")
(setq s1 '((1 1 1 1 1)
		   (1 4 0 0 1)
		   (1 0 2 0 1)
	       (1 0 3 0 1)
           (1 0 0 0 1)
           (1 1 1 1 1)
))
(print (next-states s1))

;; Example 2
(print "This is output of example 2")
(setq s2 '((1 1 1 1 1)
			(1 0 0 4 1)
			(1 0 2 3 1)
			(1 0 0 0 1)
			(1 0 0 4 1)
			(1 1 1 1 1)
))
(print (next-states s2))

;; Example 3
(print "This is output of example 3")
(setq s3 '((1 1 1 1 1)
			(1 0 0 6 1)
			(1 0 2 0 1)
			(1 0 0 0 1)
			(1 4 0 4 1)
			(1 1 1 1 1)
))
(print (next-states s3))

;; Example 4
(print "This is output of example 4")
(setq s4 '((1 1 1 1 1)
		(1 0 2 4 1)
		(1 0 0 0 1)
		(1 0 0 0 1)
		(1 0 5 3 1)
		(1 1 1 1 1)
))
(print (next-states s4))

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.

; Overall comment : a heuristic function that return the constant 0

; Header comment : 
;	Argument : given state 
; 	Return Value : return the constant 0

(defun h0 (s)
	0  
)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.

; Overall comment :
; Header comment :
	; Argument : one argument, given state s
	; Return value : the numbers of boxes that are not on goal positions in the given state

; Answer : Yes, the function h1 is admissible because 

(defun h1 (s)
	(cond 
		  ; If the given state is null, then return true
		  ((null s) 0)
		  ; Check the row to see if there are any boxes still unhandled, then return NIL immediately
		  ; We have not reach goal state yet 
		  ((and (atom s) (isBox s)) 1)
		  ((and (atom s) (not(isBox s))) 0)
		  ; Recursively search the rest of the symbols on the board
		  (t (+ (h1 (first s)) (h1 (rest s))))
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


; Overall comment : 
; Header comment : 
	; Argument : one argument, the given state s
	; Return Values : return an integer which is >= 0, the number of solved instances under a time 
	;				  limit and total running time for solving the problem 

; Answers : 

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;

(defun h505146702 (s)
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



; (defun h505146702 (s)
; 	(cond ((null s) 0)
; 		  (t 
; 		  	(let ((goal_location (find_positions s 0 4)) (box_location (find_positions s 0 2)) )

; 		  		;; find the total distance
; 		  		; get the keep distance (1)
; 		  		; get the minimum distance (2)

; 		  		(+ (calc_distance s goal_location box_location 1) (calc_distance s goal_location box_location 2))
; 		  	)
; 		  )
; 	)
; )

; (defun calc_distance (s goal_location box_location flag)
; 	(cond 
; 		  ((null s) NIL)
; 		  ((equal flag 1) 
; 			; find the keeper distance

; 			; get the current keeper location first 
; 			(let ((curr_keeper_pos (getKeeperPosition s 0)))

; 				; I need s, keeper position, box position 
; 				(keeper_distance_helper curr_keeper_pos box_location s)
; 			)

; 		  )
; 		  ((equal flag 2) 
; 		  	; find the minimum distance 

; 		  	(min_distance_helper box_location goal_location)
; 		  )

; 		  ; this is optional, error number = -1
; 		  (t -1)
; 	)
; )

; (defun min_distance_helper (box_location goal_location)
; 	(cond 
; 		((null box_location) NIL)
; 		; if we dont have any more goal_location
; 		((null goal_location) 0)
; 		(t
; 			(let (minimum_distance 
; 					(
; 						; (car box_location), goal_location
; 						; initalize the result first 
; 						(let (result (+ (difference (car (car box_location)) (car (car goal_location))) (difference (second (car box_location)) (second (car goal_location)))))



; 							compare_distance result box_location goal_location
; 						)
; 					)
; 				 )
; 				(if minimum_distance

; 					(+ minimum_distance (min_distance_helper (rest box_location) (rest goal_location)))

; 					; optional, error  
; 					; (NIL)
; 					; (0)
; 				)
; 			)
; 		)
; 	)
; )

; (defun compare_distance (result box_location goal_location)
; 	; use min function to find out the smallest one 
; 	; result should not be empty now 
; 	(cond 
; 		((null goal_location) result)
; 		(t 
; 			; be careful of manhantan distance 
; 			(let (current_distance (+ (difference (first (car box_location)) (first goal_location) ) (difference (second (car box_location)) (second goal_location) )))

; 				(compare_distance (min result current_distance) box_location (rest goal_location))
; 			)
; 		)
; 	)
; )

; (defun keeper_distance_helper (curr_keeper_pos box_location s)
; 	(cond
; 		; check if the keeper position is valid, might need to remove
; 		((null curr_keeper_pos) NIL)
; 		; if we run out of box location at the end 
; 		((null box_location) 0)
; 		(t 
; 			; curr_keeper_pos, (first box_location)
; 			; be careful of the manhantan distance
; 			(let (distance (+ (difference (car curr_keeper_pos) (car (car box_location))) (difference (car (cdr curr_keeper_pos)) (car (cdr (car box_location)))) ) )
; 				(if distance
; 					(+ distance (keeper_distance_helper curr_keeper_pos (rest box_location) s))

; 					; optional error, 0
; 					(NIL)
; 				)
; 			)
; 		)

; 	)
; )

; (defun difference (num1 num2)
; 	(abs (- num1 num2))
; )

; ;; find_positions function
; (defun find_positions (s row type)
; 	(let ((box 2) (goal 4))
; 		(cond ((null s) NIL)
; 			  ; this is for box location
; 			  ((equal type box) 
; 			  	(handle_box s row)		  	
; 			  )
; 			  ; this is for goal location
; 			  ((equal type goal) 
; 			  	(handle_goal s row)
; 			  )
; 			  ; optional 
; 			  (t -1)
; 		)
; 	)
; )

; (defun handle_box (s row)
; 	(cond 
; 		((null s) NIL)
; 		(t 
; 			(let ((location (handle_box_helper (first s) row 0)))

; 				(if location
; 					; the location is legit
; 					(append location (handle_box (rest s) (+ row 1)))

; 					; optional else case
; 				)
; 			)
; 		)
; 	)
; )


; ;; i am here 


; (defun handle_goal (s row)
; 	(cond 
; 		((null s) NIL)
; 		(t 
; 			(let ((location (handle_goal_helper (first s) row 0)))

; 				(if location
; 					(append location (handle_goal (rest s) (+ row 1)))

; 					; optional
; 					; (f)
; 				)
; 			)
; 		)

; 	)
; )

; (defun handle_goal_helper (s row column)
; 	(cond ((null s) NIL)
; 		((not (isStar (car s))) 
; 			(handle_goal_helper (rest s) row (+ column 1))
; 		)
; 		(t 
; 			(append (list (list row column)) (handle_goal_helper (rest s) row (+ column 1)))
; 		)
; 	)
; )

; (defun handle_box_helper (s row column)
; 						;(row r c)
; 	(cond ((null s) NIL)
; 		  ; if it is not a box, anything that is not a box
; 		  ((not (isBox (car s))) 
; 		  	(handle_box_helper (rest s) row (+ column 1))
; 		  )
; 		  ; if it is a box
; 		  (t 
; 		  	; the order of coordinates is in (r, c), not (c, r)
; 		  	(append (list (list row column)) (handle_box_helper (rest s) row (+ column 1)) )
; 		  )
; 	)
; )


;; output_distance function

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

(print "This is the result of p1")
(sokoban p1 #'h505146702)

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
