; (defun try-move (S D)
; 	(cond ((or (< D 0) (> D 3)) NIL)
; 		(t

; 		(let* 
; 			(

; 			(keeper_position (getKeeperPosition s 0))
; 			(keeper_x_pos (car(cdr keeper_position)))
; 			(keeper_y_pos (first keeper_position))

; 			(new_keeper_x_pos (get_new_keeper_x keeper_x_pos D ) )
; 			(new_keeper_y_pos (get_new_keeper_y keeper_y_pos D ) )

; 			(box_x_pos (get_box_x_pos keeper_x_pos d) )
; 			(box_y_pos (get_box_y_pos keeper_y_pos d) )

; 			(keeper_content (get_keeper_content S new_keeper_x_pos new_keeper_y_pos) )
; 			(box_content (get_box_content S box_x_pos box_y_pos) )

; 			(current_content (cond ((= (get-square s keeper_x_pos keeper_y_pos) 3) 0) (t 4)) )

; 			)

; 		 			(cond 
; 		 				((= keeper_content 0) (set-square(set-square s new_keeper_x_pos new_keeper_y_pos 3) keeper_x_pos keeper_y_pos current_content))
; 		 				((= keeper_content 1) NIL )
; 		 				((= keeper_content 2) 
; 		 					(cond
; 		 						((= box_content 0) (set-square(set-square(set-square s new_keeper_x_pos new_keeper_y_pos 3) keeper_x_pos keeper_y_pos current_content) box_x_pos box_y_pos 2))
; 		 						((= box_content 4) (set-square(set-square(set-square s new_keeper_x_pos new_keeper_y_pos 3) keeper_x_pos keeper_y_pos current_content) box_x_pos box_y_pos 5))
; 		 						(t nil)			
; 		 					)
; 		 				)
; 		 				((= keeper_content 4) (set-square(set-square s new_keeper_x_pos new_keeper_y_pos 6) keeper_x_pos keeper_y_pos current_content))
; 		 				(t 
; 		 					(cond
; 		 						((= box_content 0) (set-square(set-square(set-square s new_keeper_x_pos new_keeper_y_pos 6) keeper_x_pos keeper_y_pos current_content) box_x_pos box_y_pos 2))
; 		 						((= box_content 4) (set-square(set-square(set-square s new_keeper_x_pos new_keeper_y_pos 6) keeper_x_pos keeper_y_pos current_content) box_x_pos box_y_pos 5))
; 		 						(t nil)			
; 		 					)
; 		 				)
; 		 			)
; 		)
; 		)
; 	)
; )


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
		  		   (box_content (get_box_content S box_x_pos box_y_pos) ) 
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


(defun h505146702 (s)
	(cond ((null s) 0)
		  (t 
		  	(let ((goal_location (find_positions s 0 4)) (box_location (find_positions s 0 2)) )

		  		;; find the total distance
		  		; get the keep distance (1)
		  		; get the minimum distance (2)

		  		(+ (calc_distance s goal_location box_location 1) (calc_distance s goal_location box_location 2))
		  	)
		  )
	)
)

(defun calc_distance (s goal_location box_location flag)
	(cond 
		  ((null s) NIL)
		  ((equal flag 1) 
			; find the keeper distance

			; get the current keeper location first 
			(let ((curr_keeper_pos (getKeeperPosition s 0)))

				; I need s, keeper position, box position 
				(keeper_distance_helper curr_keeper_pos box_location s)
			)

		  )
		  ((equal flag 2) 
		  	; find the minimum distance 

		  	(min_distance_helper box_location goal_location)
		  )

		  ; this is optional, error number = -1
		  (t -1)
	)
)

(defun min_distance_helper (box_location goal_location)
	(cond 
		((null box_location) NIL)
		; if we dont have any more goal_location
		((null goal_location) 0)
		(t
			(let (minimum_distance 
					(
						; (car box_location), goal_location
						; initalize the result first 
						(let (result (+ (difference (car (car box_location)) (car (car goal_location))) (difference (second (car box_location)) (second (car goal_location)))))



							compare_distance result box_location goal_location
						)
					)
				 )
				(if minimum_distance

					(+ minimum_distance (min_distance_helper (rest box_location) (rest goal_location)))

					; optional, error  
					; (NIL)
					; (0)
				)
			)
		)
	)
)

(defun compare_distance (result box_location goal_location)
	; use min function to find out the smallest one 
	; result should not be empty now 
	(cond 
		((null goal_location) result)
		(t 
			; be careful of manhantan distance 
			(let (current_distance (+ (difference (first (car box_location)) (first goal_location) ) (difference (second (car box_location)) (second goal_location) )))

				(compare_distance (min result current_distance) box_location (rest goal_location))
			)
		)
	)
)

(defun keeper_distance_helper (curr_keeper_pos box_location s)
	(cond
		; check if the keeper position is valid, might need to remove
		((null curr_keeper_pos) NIL)
		; if we run out of box location at the end 
		((null box_location) 0)
		(t 
			; curr_keeper_pos, (first box_location)
			; be careful of the manhantan distance
			(let (distance (+ (difference (car curr_keeper_pos) (car (car box_location))) (difference (car (cdr curr_keeper_pos)) (car (cdr (car box_location)))) ) )
				(if distance
					(+ distance (keeper_distance_helper curr_keeper_pos (rest box_location) s))

					; optional error, 0
					(NIL)
				)
			)
		)

	)
)

(defun difference (num1 num2)
	(abs (- num1 num2))
)

;; find_positions function
(defun find_positions (s row type)
	(let ((box 2) (goal 4))
		(cond ((null s) NIL)
			  ; this is for box location
			  ((equal type box) 
			  	(handle_box s row)		  	
			  )
			  ; this is for goal location
			  ((equal type goal) 
			  	(handle_goal s row)
			  )
			  ; optional 
			  (t -1)
		)
	)
)

(defun handle_box (s row)
	(cond 
		((null s) NIL)
		(t 
			(let ((location (handle_box_helper (first s) row 0)))

				(if location
					; the location is legit
					(append location (handle_box (rest s) (+ row 1)))

					; optional else case
				)
			)
		)
	)
)


;; i am here 


(defun handle_goal (s row)
	(cond 
		((null s) NIL)
		(t 
			(let ((location (handle_goal_helper (first s) row 0)))

				(if location
					(append location (handle_goal (rest s) (+ row 1)))

					; optional
					; (f)
				)
			)
		)

	)
)

(defun handle_goal_helper (s row column)
	(cond ((null s) NIL)
		((not (isStar (car s))) 
			(handle_goal_helper (rest s) row (+ column 1))
		)
		(t 
			(append (list (list row column)) (handle_goal_helper (rest s) row (+ column 1)))
		)
	)
)

(defun handle_box_helper (s row column)
						;(row r c)
	(cond ((null s) NIL)
		  ; if it is not a box, anything that is not a box
		  ((not (isBox (car s))) 
		  	(handle_box_helper (rest s) row (+ column 1))
		  )
		  ; if it is a box
		  (t 
		  	; the order of coordinates is in (r, c), not (c, r)
		  	(append (list (list row column)) (handle_box_helper (rest s) row (+ column 1)) )
		  )
	)
)

















)