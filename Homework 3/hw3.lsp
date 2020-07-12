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

; Overall comment : count the number of box in the given state s
; Header argument :
	; Argument : one argument, given state s
	; Return value : return the number of box appear
(defun count-box (s)
	(count 2 (car s))
)

; Overall comment : count the number of keeper in the given state s
; Header argument :
	; Argument : one argument, given state s
	; Return value : return the number of keeper appear 
(defun count-keeper (s)
	(count 3 (car s))
)

; Overall comment : Check if the sum of the number of box and keeper together is greater than 0
; Header argument :
	; Argument : one argument, given state s
	; Return value : true when the total number of box and keeper is greater than 0
	;				 false when the total number of box and keeper is less than 0
(defun goal-test-helper (s)
	(< 0 (+ (count-box s) (count-keeper s)))
)

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.

; Overall comment : The function check if any row of the given state has box or keeper is not 
;					in the goal position, then return NIL to indicate the game is not terminated yet
; Header comment : 
	; Argument : one argument, given state s
	; Return value : return true if state s equal to goal state which is the
	; 				 game terminating condition which is when box and the keeper 
	; 				 is in the goal position 

(defun goal-test (s)
	(cond 
		; base case : when s is empty, return true 
		; game terminating condition when it returns 0
		((null s) T)
		; if the condition is true, return false because not game terminating condition
		; not game terminating condition when it returns positive integer greater than 0
		((goal-test-helper s) NIL)
		; recursively search the next row in the given state
		(t (goal-test (cdr s)))
	)
)

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

; Overall comment : The function construct the next state based on the given state by attempting to 
;					move in the four direction, up, down, left, right 
; Header comment :
;	Argument : one argument, the given state
;	Return value : return a list of states that can be reached from the given states in one move

;; Problem !!!!
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 ; Try to call try-move in each of the four directions : up, down, left, right
	 (result (list (try-move s 2) (try-move s 1) (try-move s 3) (try-move s 0)))
	 )
    (cleanUpList result);end
   );end let
 );


; Overall comment : helper function for set-sqaure to actually modify the content 
;					in the corresponding square 
; Header comment : 
;	Argument : three argument, row r, column c, square content v
; 	Return value : return square with content updated to content v

(defun set-square-help (r c v)
	(cond ; base case : if row is empty, return NIL
		  ((null r) NIL)
		  ; base case : if column is less than 0, return row back
		  ((< c 0) r)
		  ; base case : if column equal to 0, add the new content v to the row list
		  ((equal c 0) (cons v (rest r)))
		  ; recursive case : search the rest of the rows and columns, carry along new content v
		  (t (cons (first r) (set-square-help (rest r) (- c 1) v)))
	)
)

; Overall comment : The function should not modify the input state. It returns the state by 
;					setting the square (r, c) to content v
; Header comment : 
	; Argument : 4 arguments, state S, row number r, column number c, square content v
	; Return value : return a new state S' 

(defun set-square (S r c v)
	(cond 
		  ; base case : if state is empty, return NIL
		  ((null S) NIL)
		  ; base case : if row is less than zero, return original state 
		  ((< r 0) S)
		  ; base case : if column is less than zero, return original state 
		  ((< c 0) S) 
		  ; base case : if row is equal to zero, call helper function to modify the content in the sqaure
		  ((equal r 0) (cons (set-square-help (first S) c v) (rest S)))
		  ; recursive case : if row is not equal to zero, recursively go to the rest of the rows and columns 
		  ((not(equal r 0)) (cons (first S) (set-square (rest s) (- r 1) c v)))
	)
)

; Overall comment : helper function for set-sqaure to actually extract the content at (r, c)
; Header comment :
;	Argument : two arguments, row r, column c
;	Return Value : extract the content at suqare (r, c)

(defun get-square-help (r c)
	(cond 
		  ; base case : if row is empty, return wall 
		  ((null r) 1)
		  ; base case : if column is less than zero, return wall
		  ((< c 0) 1)
		  ; base case, if column is 0, return head of row 
		  ((equal c 0) (first r))
		  ; recursive case : recursively search the rest of row and column
		  (t (get-square-help (rest r) (- c 1)))
	)
)

; Overall comment : The function extract and return the content at (r, c) by calling the 
;					get-sqaure-help function 
; Header comment :
	; Argument : three arguments, a state S, a row number r, a column number c
	; Return value : If square is valid, return the integer content of the state S at square (r, c)
	;                If sqaure is out of scope, return value of a wall which is 1

(defun get-square (S r c)
	(cond 
		  ; base case : if state is empty, return wall
		  ((null S) 1)
		  ; base case : if row is less than zero, return wall
	      ((< r 0) 1)
	      ; base case : if column is less than zero, return wall 
	      ((< c 0) 1)
	      ; base case : if row is 0, call helper function to extract the content at (r, c)
	      ((equal r 0) (get-square-help (first S) c))
	      ; recursive case : recursively search the rest of state with corresponding row and column
	      (t (get-square (rest S) (- r 1) c))
	)
)

; Overall comment : The function return the updated x coordinate of the keeper position 
; Header comment :
;	Argument : two arguments, the current x position, the given move direction d
;	Return value : return the updated x position of the keeper 

(defun get_new_keeper_x (x d)
	(cond 
		  ; base case : if direction is down, x does not change 
		  ((equal d 0) x)
		  ; base case : if direction is up, x does not change
		  ((equal d 1) x)
		  ; base case : if direction is left, shift x to left by 1
		  ((equal d 2) (- x 1)) 
		  ; base case : if direction is right, shift x to right by 1
		  ((equal d 3) (+ x 1)) 
		  ; else case : undefined direction, return x back 
		  (t x)
	)
)

; Overall comment : The function return the updated x coordinate of the box position,
;					behind or in front of the keeper 
; Header comment : 
;	Argument : two arguments, the current x position, the given move direction d 
;	Return value : return the updated x position of the box 

(defun get_box_x_pos (x d)
	(cond 
		  ; base case : if direction is down, x does not change
		  ((equal d 0) x)
		  ; base case : if direction is up, x does not change 
		  ((equal d 1) x)
		  ; base case : if direction is left, shift x to left by 2
		  ((equal d 2) (- x 2))
		  ; base case : if direction is right, shift x to right by 2
		  ((equal d 3) (+ x 2))
		  ; else case : undefined direction, return x back 
		  (t x)
	)
)

; Overall comment : The function return the updated y coordinate of the keeper position
; Header comment : 
;	Argument : two arguments, the current y position, the given move direction d 
;	Return value : return the updated y position of the keeper 

(defun get_new_keeper_y (y d)
	(cond
		; base case : if direction is down, shift y down by 1
		((equal d 0) (- y 1)) 
		; base case : if direction is up, shift y up by 1
		((equal d 1) (+ y 1))
		; base case : if direction is left, y does not change 
		((equal d 2) y)
		; base case : if direction is right, y does not change 
		((equal d 3) y) 
		; else case : undefined direction, return y back
		(t y)

	)
)

; Overall comment : The function return the updated y coordinate of the box position,
;					behind or in front of the keeper 
; Header comment : 
;	Argument : two arguments, the current y position, the given move direction d 
;	Return value : return the updated y position of the box 

(defun get_box_y_pos (y d)
	(cond 
		; base case : if direction is down, shift y down by 2
		((equal d 0) (- y 2))
		; base case : if direction is up, shift y up by 2
		((equal d 1) (+ y 2))
		; base case : if direction is left, y does not change 
		((equal d 2) y)
		; base case : if direction is right, y does not change
		((equal d 3) y)
		; else case : undefined direction, return y back 
		(t y)
	)
)

; Header comment : 
;	Argument : 3 arguments, the given state S, updated x and y coordinate of the keeper position
;	Return value : return the content located at the given keeper position
(defun get_keeper_content (S new_keeper_x_pos new_keeper_y_pos)
	; call the get-sqaure function to extract the content 
	(get-square S new_keeper_x_pos new_keeper_y_pos)
)

; Header comment : 
;	Argument : 3 arguments, the given state S, updated x and y coordinate of the box position
;	Return value : return the content located at the given box position
(defun get_box_content (S box_x_pos box_y_pos)
	; call the get-sqaure function to extract the content 
	(get-square S box_x_pos box_y_pos)
)

; Overall comment : The function return either goal or blank depend on content at the keeper position 
; Header comment : 
;	Argument : 3 arguments, the given state S, current x and y coordinate of the keeper position
;	Return value : return the content located at the given keeper position
(defun get_current_content (S keeper_x_pos keeper_y_pos)
		; if the content does not equal to keeper, return goal
	(cond ((not (equal (get-square S keeper_x_pos keeper_y_pos) 3)) 4) 
		; if the content equals to the keeper, return blank
		  (t 0)
	)
)

; Overall comment : The function specifically handle the case when the keeper content is blank or goal
; Header comment : 
;	Argument : 7 arguments, the given state S, the x and y coordinate of the original keeper position, the x and y coordinate of the new keeper position
;			   the new content for the new position, the current content for the original position 
;	Return value : return the state with the modified content

(defun handle_blank_or_goal (S keeper_x_pos keeper_y_pos new_keeper_x_pos new_keeper_y_pos new_content current_content)
	; might switch it back !!!
	; (set-square (set-square S new_keeper_x_pos new_keeper_y_pos new_content) keeper_x_pos keeper_y_pos current_content)

	; update the keeper content to current content and the new keeper content to new content
	(set-square (set-square S keeper_x_pos keeper_y_pos current_content) new_keeper_x_pos new_keeper_y_pos new_content)
)

; Overall comment : The function specifically handle the case when the keeper content is box or the box content is either blank or goal
; Header comment : 
;	Argument : 10 arguments, the given state S, the x and y coordinate of the original keeper position, the x and y coordinate of the new keeper position
;			   the x and y coordinate of the box, the new content for the new position, the current content for the original position,
;				the new box content 
;	Return value : return state with the modified content 

(defun handle_content (S keeper_x_pos keeper_y_pos new_keeper_x_pos new_keeper_y_pos box_x_pos box_y_pos new_content current_content box_content)
	; try flip the set-square order 
	; might switch it back 
	; (set-square (set-square (set-square S new_keeper_x_pos new_keeper_y_pos new_content) keeper_x_pos keeper_y_pos current_content) box_x_pos box_y_pos box_content)

	; update the keeper content to current content, the new keeper content to new content, and the box content to new box content 
	(set-square (set-square (set-square S keeper_x_pos keeper_y_pos current_content) new_keeper_x_pos new_keeper_y_pos new_content) box_x_pos box_y_pos box_content)
)

; Header comment :
	; Argument : 2 arguments, state S, a move direction D
	; Return value : If the move is valid, return the state that is result of moving the keeper in state S in direction D 
	; 				 Update the content of every square to the right value 
	; 				 If the move is invalid, return NIL 

(defun try-move (S D)
	; if D is not a valid direction, return NIL
	(cond ((or (< D 0) (> D 3)) NIL)
	; if D is a valid direction, continue to check
		  (t 
		  	; Set up some variables to store all the needed coordinates on the board
		  	(let*  
		  		   ; 1. the keeper position
		  		   ((keeper_position (getKeeperPosition S 0)) 
		  		   ; 2. x coordinate of Keeper position
		  		   (keeper_x_pos (car (cdr keeper_position))) 
		  		   ; 3. y coordinate of Keeper position
		  		   (keeper_y_pos (car keeper_position)) 

		  		   ; 4. new x coordinate of the keeper position 
		  		   (new_keeper_x_pos (get_new_keeper_x keeper_x_pos d)) 
		  		   ; 5. new y coordinate of the keeper position
		  		   (new_keeper_y_pos (get_new_keeper_y keeper_y_pos d)) 

		  		   ; 6. the x coordinate of move box position
		  		   (box_x_pos (get_box_x_pos keeper_x_pos d)) 
		  		   ; 7. the y coordinate of move box position
		  		   (box_y_pos (get_box_y_pos keeper_y_pos d)) 

		  		   ; 8. the content at the new keeper position
		  		   (keeper_content (get_keeper_content S new_keeper_x_pos new_keeper_y_pos)) 
		  		   ; 9. the content at the move box position
		  		   (box_content (get_box_content S box_x_pos box_y_pos)) 
		  		   ; 10. the current content at the keeper position
		  		   (current_content (get_current_content S keeper_x_pos keeper_y_pos)))

		  		(cond 
		  			  ; if the keeper content is blank, call the helper function to update the content  
		  			  ((equal keeper_content 0) (handle_blank_or_goal S keeper_x_pos keeper_y_pos new_keeper_x_pos new_keeper_y_pos 3 current_content))
		  			  ; if the keeper content is wall, return NIL
		  			  ((equal keeper_content 1) NIL)
		  			  ; After the move, if keeper content is box and the box content is blank
		  			  ; There is a box next to keeper and the keeper is able to move the box to blank as well
		  			  ((and (equal keeper_content 2) (equal box_content 0)) (handle_content S keeper_x_pos keeper_y_pos new_keeper_x_pos new_keeper_y_pos box_x_pos box_y_pos 3 current_content 2))
		  			  ; After the move, if keeper content is box and the box content is goal 
		  			  ; There is a box next to keeper and the keeper is able to move the box to goal as well 
		  			  ((and (equal keeper_content 2) (equal box_content 4)) (handle_content S keeper_x_pos keeper_y_pos new_keeper_x_pos new_keeper_y_pos box_x_pos box_y_pos 3 current_content 5))
		  			  ; After the move, if keeper content is box and the box content is not blank, return NIL
		  			  ; There is a box next to keeper and the keeper is not able to move the box because something is blocking it 
		  			  ((and (equal keeper_content 2) (not(equal box_content 0)) (not(equal box_content 4))) NIL)
		  			  ; ((and (equal keeper_content 4) (handle_blank_or_goal S keeper_x_pos keeper_y_pos new_keeper_x_pos new_keeper_y_pos 6 current_content)))
		  			  ; After the move, if the keeper content is a goal and the keeper is in the goal position
		  			  ((equal keeper_content 4) (handle_blank_or_goal S keeper_x_pos keeper_y_pos new_keeper_x_pos new_keeper_y_pos 6 current_content))
		  			  ; After the move, the box content is a blank and it is move successfully 
		  			  ((equal box_content 0) (handle_content S keeper_x_pos keeper_y_pos new_keeper_x_pos new_keeper_y_pos box_x_pos box_y_pos 6 current_content 2))
		  			  ; After the move, the box content is a goal and it is move successfully 
		  			  ((equal box_content 4) (handle_content S keeper_x_pos keeper_y_pos new_keeper_x_pos new_keeper_y_pos box_x_pos box_y_pos 6 current_content 5))
		  			  ; anything other than, return NIL
		  			  (t NIL)
		  		)
		  	)
		  )
	)
)

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.

; Overall comment : a heuristic function that return the constant 0
; Header comment : 
;	Argument : one argument, given state s
; 	Return Value : return the constant 0

(defun h0 (s)
	0
)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.

; Overall comment : a heuristic function that return the number of misplaced boxes 
; Header comment :
	; Argument : one argument, given state s
	; Return value : the numbers of boxes that are not on goal positions in the given state

; Answer : Yes, the function h1 is admissible because h1 seek to find the number of misplaced boxes
;		   which is less than the total number of travels need to move all the boxes to the goal position
; 		   and keeper to the corresponding position. 

(defun h1 (s)
	; if the state is null, return 0 
	(cond ((null s) 0)
		(t 
			; add up all the found boxes, repeat for the rest of the rows 
			(+ (count-box s) (h1 (cdr s)))
		)
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


; Overall comment : a heuristic function is used to speed up the A* search engine 
; Header comment :
	; Argument : one argument, given state s
	; Return value : return an integer >= 0, the shortest distance

(defun h505146702 (s)
	; find the keeper + goal position
	; find the goal position 
	(let* ( (keeper_goal_location (find-k-goal-pos s 0)) (goal_location (find-goal-pos s 0)) )

		; (sumMin (keeper_goal_location) (goal_location) )
		; (sumMin (first keeper_goal_location) (first goal_location) )

		; find the distance in order for the keeper to reach the goal  
		(find-distance (first keeper_goal_location) (first goal_location))
	)
)

; Overall comment : a function finds a list of coordinates of keeper goal 
; Header comment :
	; Argument : two argument, given state s, row r
	; Return value : return keeper + goal position 
(defun find-k-goal-pos (s r)
	; if state is empty, return NIL
	(cond ((null s) NIL)
		(t 
			; find the column number of the corrpesonding element 
			(let* ((c (find-column (car s) r 0)))


				; form the list 
				(cons (first c) (first (find-k-goal-pos (rest s) (+ r 1))))
			)
		)

	)
)

; Overall comment : a function finds a list of coordinates of goal 
; Header comment :
	; Argument : one argument, given state s, row r
	; Return value : return goal position 
(defun find-goal-pos (s r)
	; if state is empty, return NIL
	(cond ((null s) NIL)
		(t 
			; find the column number of the corresponding element 
			(let* ((c (find-column (car s) r 0)))

				; form the list 
				(cons (second c) (second (find-goal-pos (rest s) (+ r 1))))
			)
		)

	)
)

; Header comment :
	; Argument : three arguments, row r, current row r_num, column c 
	; Return value : return the column of the corresponding element  
(defun find-column (r r_num c)
	(cond 
		  ; if row is empty, return NIL
		  ((null r) NIL)
		  ; if the content is box
		  ((equal 2 (car r)) 
		  	; add to the list 
		  	(cons (first (find-column (cdr r) r_num (+ c 1))) (list (list r_num c)))
		  )
		  ; if the content is keeper
		  ((equal 3 (car r)) 
		  	; add to the list 
		  	(cons  (first (find-column (cdr r) r_num (+ c 1))) (list (list r_num c)))
		  )
		  ; if the content is goal 
		  ((equal 4 (car r)) 
		  	(first (find-column (cdr r) r_num (+ c 1)))
		  )
		  ; else case
		  (t 
		  	; recursively search 
		  	(find-column (rest r) r_num (+ c 1))
		  )

	)
)

; Header comment : the function find the shortest distance between each node and the goal position 
	; Argument : 2 arguments, the keeper location, the goal location
	; Return value : return the sum of distance, for each node to the goal position 
(defun find-distance (keeper_loc goal_loc)
	(cond 
		; if the keeper location is empty, return 0
		((null keeper_loc) 0)
		; if the goal location is empty, return NIL
		((null goal_loc) NIL)
		(t 
			; get the row and column
			(let* ((r (first (first keeper_loc))) (c (second (car keeper_loc))))
				; find the distance and add it with the recursive part  
				(+ (find-distance-helper 0 r c goal_loc) (find-distance (cdr keeper_loc) goal_loc))
			)
		)

	)
)

; Header comment :
	; Argument : 4 arguments, the minimum distance result, row r, column c, goal location
	; Return value : return the minimum distance
(defun find-distance-helper (result r c goal_loc)
	; if goal location is not null, find the minimum distance
	(if (not (null goal_loc))
		; find the minimum distance by comparing between the current distance and the new calculated distance 
		(let* ( (r_sum (get-sum-row r goal_loc)) (c_sum (get-sum-column c goal_loc)) )
			(find-distance-helper (min result (+ r_sum c_sum)) r c (cdr goal_loc))
		)

		;if the goal location is null, return the original result immediately 
		(result)
	)
)

; Header comment :
	; Argument : 2 arguments, row r, goal location
	; Return value : the maximum between row and goal location
(defun get-sum-row (r goal_loc)
	(max (- r (car(car goal_loc))) (- (car(car goal_loc)) r))
)


; Header comment :
	; Argument : 2 arguments, column c, goal location
	; Return value : the maximum between column and goal location 
(defun get-sum-column (c goal_loc)
	(max (- c (second (car goal_loc))) (- (second (car goal_loc)) c))
)

;; Testcases for next-states 
;; Example 1
; (print "This is output of example 1")
; (setq s1 '((1 1 1 1 1)
; 		   (1 4 0 0 1)
; 		   (1 0 2 0 1)
; 	       (1 0 3 0 1)
;            (1 0 0 0 1)
;            (1 1 1 1 1)
; ))
; (print (next-states s1))

; ;; Example 2
; (print "This is output of example 2")
; (setq s2 '((1 1 1 1 1)
; 			(1 0 0 4 1)
; 			(1 0 2 3 1)
; 			(1 0 0 0 1)
; 			(1 0 0 4 1)
; 			(1 1 1 1 1)
; ))
; (print (next-states s2))

; ;; Example 3
; (print "This is output of example 3")
; (setq s3 '((1 1 1 1 1)
; 			(1 0 0 6 1)
; 			(1 0 2 0 1)
; 			(1 0 0 0 1)
; 			(1 4 0 4 1)
; 			(1 1 1 1 1)
; ))
; (print (next-states s3))

; ;; Example 4
; (print "This is output of example 4")
; (setq s4 '((1 1 1 1 1)
; 		(1 0 2 4 1)
; 		(1 0 0 0 1)
; 		(1 0 0 0 1)
; 		(1 0 5 3 1)
; 		(1 1 1 1 1)
; ))
; (print (next-states s4))


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

; (print "This is the result of p1")
; (sokoban p1 #'h505146702)
; (sokoban p1 #'h1)

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

; (print "This is the result of p2")
; (sokoban p2 #'h505146702)
; (sokoban p2 #'h1)

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

; (print "This is the result of p3")
; (sokoban p3 #'h505146702)
; (sokoban p3 #'h1)

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

; (print "This is the result of p4")
; (sokoban p4 #'h505146702)

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))
; (print "This is the result of p5")
; (sokoban p5 #'h505146702)

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))
; (print "This is the result of p6")
; (sokoban p6 #'h505146702)

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))
; (print "This is the result of p7")
; (sokoban p7 #'h505146702)

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))
; (print "This is the result of p8")
; (sokoban p8 #'h505146702)

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))
; (print "This is the result of p9")
; (sokoban p9 #'h505146702)

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

;; come back and here and wait at least 30 minutes to see 
; (print "This is the result of p10")
; (sokoban p10 #'h505146702)

;(?)
;(51)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))
; (print "This is the result of p11")
; (sokoban p11 #'h505146702)

;(?)
; stop at here 
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))
; (print "This is the result of p12")
; (sokoban p12 #'h505146702)

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))
; (print "This is the result of p13")
; (sokoban p13 #'h505146702)

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
; (print "This is the result of p14")
; (sokoban p14 #'h505146702)

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

; (print "This is the result of p15")
; (sokoban p15 #'h505146702)

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
