; Name : Sum Li (UID : 505146702)
; CS 161 Homework 4

;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;


; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists

; Header comment :
	; Argument : 1 argument, the result list that store the updated assignment of the variables 
	; Return value : return the result list 
(defun handle_positive_assignment (result)
	; According to given examples, the values increase one by one
	; The assigned value of the variables is just one bigger than the length of the current result list
	(let* ((result_length (length result)) (assigned_value (+ 1 result_length)))
		; try to append the assigned value to update the assignment list 
		(append result (cons assigned_value NIL))
	)
)

; Header comment :
	; Argument : 1 argument, the result list that store the updates assignment of the variables
	; Return value : return the result list 
(defun handle_negative_assignment (result)
	; According to given examples, the values increase one by one
	; The assigned value of the variables is just one bigger than the length of the current result list
	(let* ((result_length (length result)) (assigned_value (+ 1 result_length)))
		; try to append the assigned value to updated the assignment list 
		(append result (cons (* -1 assigned_value) NIL))
	)
)

; Header comment :
	; Argument : 2 arguments, flag to indicate whether it is positive or negative assignment of values
	;			 result to store the assignment 
	; Return value : return the correct assignment of variables to satisfy the sat problem 
(defun fill_assignment (flag result)
	(cond 
		; if the flag is 1, we wil append a positive value as an assignment to the variable
		((equal flag 1) (handle_positive_assignment result))
		; if the flag is 2, we will append a negative value as an assignment to the variable 
		((equal flag 2) (handle_negative_assignment result))
	)
)

; Header comment :
	; Argument : 2 arguments, the result list, delta which is one specific element in one clause
	; Return value : Return true if the assignment of the variable equals to element
	;				 Return true if there is some assignment of the variable is able to satisfy sat
	;				 Return NIL if the assigment of the variable has the opposite sign of element or
	;				 element has the opposite sign of variable 
(defun verify_elem (delta result) 
	(let* ((element (first result)) (remain_elem (rest result)))
		; return true when result is empty
		(cond ((null result) T)
			  ; return NIL if element equal to the opposite sign of variable 
			  ((equal element (* -1 delta)) NIL)
			  ; return NIL if variable equal to the opposite sign of element
			  ((equal delta (* -1 element)) NIL)
			  ; if the variable does not equal to the element, then 
			  ; immediately continue to recursively search the next assignment 
			  ((not (equal element delta)) (verify_elem delta remain_elem))
			  ; return true when variable equal to element 
			  (t T)

		)
	)
)

; Overall comment : The function handle each individual clause by checking whether the current assignment of each of
;					the variables in the clause is able to satisfy the sat problem. The second checking is try to 
;					assign new values to the variables to fulfill the sat problem. The function is similar to 
;					verify_clause, but divide into smaller clauses 
; Header comment :
	; Argument : 2 arguments, the result list, delta which is one specific individual clause 
	; Return value : return true and the correctly assigned part of result list (), sublist 
(defun verify_clause_helper (delta result) 
	; Separate the first element and the rest of the elements in a clause 
	(let* ((head_elem (car delta)) (tail_clause (cdr delta)))
		; if the clause is empty, return NIL immediately 
		(cond  ((null delta) NIL)
			   ; call the verify each element helper function to check each element in an individual clause 
			   ; if it is not valid, immediately continue to search the rest of the elements in a clause 
			   ((not (verify_elem head_elem result)) (verify_clause_helper tail_clause result))
			   ; The rest of possibilities are consider valid assignment, return true
			   (t T)
		)
	)
)

; Overall comment : This function handle the clause list by checking whether the existing assignment of each of the 
;					variables in the list is able to satisfy the sat problem. The second checking is try to assign
;					new values to the variables to fulfill the sat problem.
; Header comment :
	; Argument : 2 argments, the result list, delta which is a list of clauses defined in CNF form
	; Return value : return true and the correctly assigned result list 
(defun verify_clause (delta result)
	; Separate the first clause and the rest of the clause list
	(let* ((head_clause (first delta)) (remain_clause (rest delta)))
	; if the clause list is not empty
	(cond ((not (null delta)) 
			; call the verify individual clause helper function
			(if (verify_clause_helper head_clause result)
				; continue to recursively search the rest of the clause list
				(verify_clause remain_clause result)
			)
		)
	; if the clause list is empty, finish checking, return true 
		(t T)
	)
	)
)

; Overall comment : This function first check the clauses to see if there are valid assignment and return it 
;					immediately. If the assignment is not valid, return NIL. However, if the list is not 
; 					completely assigned properly, we need to perform depth first serach algorithm to complete
;					the assignment. 
; Header comment :
	; Argument : 3 arguments, n is the number of variables, delta is CNf defined over n variables
	; Return value : return a list of n integers, or NIL
(defun backtrack_helper (n delta result)
	; call the verify_clause function to check the individual clause whether it satisfy the sat or not
	(let* ((clause_result (verify_clause delta result)) (result_length (length result)))
		; it does not satisfy, return NIL
		(cond ((not clause_result) NIL)
			(t  
				; the number of variables from the result does not equal to number of given variables
				(cond ((not (equal n result_length)) 
						; attempt to put assignment to rest of the slots in order to match up with n variables 
						(let* ((pos_assignment (fill_assignment 1 result)) (neg_assignment (fill_assignment 2 result)))
						  	; check whether positive or negative assignment fulfill the requirement
						  	; either postive assignment or negative assignment depend on the sign 
						  	(if (not (backtrack_helper n delta pos_assignment))
						  		; attempt negative assignment
						  		(backtrack_helper n delta neg_assignment)
						  		; attempt positive assignment 
						  		(backtrack_helper n delta pos_assignment)
						  	)
						)
					  )
					  ; if the number of variables from the result has the same number of given variables 
					  ; this mean the result list contains correct number and assignment to satisfy the sat 
					  (t result)
				)
			)
		)
	)
)

; Header comment : the function create an empty list to hold the return result 
	; Argument : 2 arguments, n is number of variables, delta is CNF defined over n variables
	; Return value : return a list of n integers, or NIL
(defun pre_backtrack (n delta)
	; create an empty list for storing the result
	(backtrack_helper n delta '())
)

; Header comment :
	; Argument : 2 arguments, n is an integer indicate for number of variables, 
	;            delta is a CNF defined over n variables
	; Return value : if model is satisfiable, return a list of n integers, represent a model of delta
	;                if model is not satisfiable, return NIL
(defun sat? (n delta)
	(cond 
		  ; if the list of CNF is empty, model is not satisfiable, return NIL
		  ((null delta) NIL)
		  ; if n shows up as 0 variables, model is not satisfiable, return NIL
		  ((equal n 0) NIL)
		  ; call the pre_backtrack functioin to prepare for the search
		  (t (pre_backtrack n delta))

	)
)

; Basic two testcases for sat function
; (print "This is result of first basic testcases")
; (print (sat? 3 '((1 -2 3) (-1) (-2 -3))))

; (print "This is result of second basic testcases")
; (print (sat? 1 '((1) (-1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (solve-cnf "./cnfs/f1/sat_f1.cnf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))



; (print "This is result of cnf1 ")
; (print (solve-cnf "./cnfs/f1/sat_f1.cnf"))

; (print "This is the result of cnf2")
; (print (solve-cnf "./cnfs/f2/sat_f2.cnf"))

; (print "This is the result of cnf3")
; (print (solve-cnf "./cnfs/f3/sat_f3.cnf"))

; ; ; ;; come back to check this 
; (print "This is the result of cnf4")
; (print (solve-cnf "./cnfs/f4/sat_f4.cnf"))

; (print "This is the result of cnf5")
; (print (solve-cnf "./cnfs/f5/sat_f5.cnf"))




