(deftemplate qualification
	(multislot student-name)
	(multislot trait))

(deftemplate career-requirements
	(multislot trait)
	(multislot career-name)
	(slot weight(default 0)))

(deftemplate fitting-careers
	(multislot career-name)
	(multislot student-name)
	(slot weight (default 0))
)

(deffunction create-facts(?trait ?related-careers)
	(bind ?len (length$ ?related-careers))
	(bind ?i 0)
	(progn$ (?field (create$ ?related-careers))
		(if (eq (mod ?field-index 2) 0)
			then
			(assert 
				(career-requirements
					(trait ?trait)
					(career-name (subseq$ ?related-careers (- ?field-index 1) (- ?field-index 1)))
					(weight ?field)
				)
			)
		)
	)
)

(deffunction ask-question (?question $?allowed-values)    ;this function poses a question to the student and then compares their answer against expected values
   (printout t ?question)
   (bind ?answer (read))    ; receive the answer from the student and store it in the 'answer' variable
   (if (lexemep ?answer)    ; checks if the variable 'answer' is a symbol or a string
       then (bind ?answer (lowcase ?answer))) 
   (while (not (member ?answer ?allowed-values)) do    ; the question is repeated to the student until they input a valid answer
   	  (printout t "Invalid answer. Allowed : " ?allowed-values crlf)    ;print out to the student the allowed values
      (printout t ?question)   
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)  ;this function returns the student's answer

(deffunction yes-or-no-p (?question)  ;this function provides a standard format for the answer received from the student by representing a yes/y answer as a basic yes and a no/n answer as a basic no. 
   (bind ?response (ask-question ?question yes no y n))  ;the student's answer received from the function 'ask_question' is assigned to variable response 
   (if (or (eq ?response yes) (eq ?response y))
        then yes 
    else no))


(deffunction interrogate-student(?student-name ?question ?trait)   ;this function checks if the student's response from function 'yes-or-no-p' is a yes, in which case the deftemplate 'qualification' is asserted
	(if (eq (yes-or-no-p ?question) yes)
		then
		(assert
			(qualification
				(student-name ?student-name)
				(trait ?trait)
			)
		 )
	)
)


(deffunction read-careers(?file)  ;this funtion welcomes the student to the advisory system and then opens and reads the careers file which contains questions posed to the student that will allow for the system to make an informed career suggestion
	(printout t " ------------------------------------------------------------------------------ " crlf " - " crlf)
	(printout t " -                          STUDENT CAREER ADVISOR                            - " crlf " - " crlf)
	(printout t " ------------------------------------------------------------------------------ " crlf crlf)
	(printout t " Please enter your first name to continue: ")
	(bind ?student-name (read))
	(printout t crlf " Hello " ?student-name ". Welcome! My name is Adama. I am a career advisor." crlf)
	(open ?file file-data)  ;the file whose name is passed to this function's 'file' argument is opened here
	(bind ?stop FALSE)  ;  initial value of variable 'stop' is set to FALSE 
	(while(not ?stop)   ;  reading of the file will continue until the value of the variable stop is no longer FALSE ie when it is  TRUE
		(bind ?temp-line (readline file-data))  ;each line read by function readline from file-data is assigned to the variable temp-line
		(if (eq ?temp-line EOF)    ; check for End Of File
			then (bind ?stop TRUE)
			else(
				if (eq ?temp-line "")  ; check for strings in the file and print them out just as they are
					then (printout t "")
			else   ; if the contents of temp-line are neither EOF or a string then the following is done:  
				(bind ?exploded (explode$ ?temp-line))  ; each element contained in temp-line is returned as a part of a multifield (exploded) value using the function explode$
				(bind ?trait (first$ ?exploded)) ; the first value of the multifield 'exploded' is stored in variable 'trait' (contains the list of possible student traits)
				(bind ?temp (rest$ ?exploded))  ; the rest of the values of the multifield 'exploded' are stored in variable 'temp'
				(bind ?question (implode$ (subseq$ ?exploded 2 2)))
				(bind ?related-careers (rest$ ?temp))
				(create-facts ?trait ?related-careers)
				(interrogate-student ?student-name ?question ?trait)
		   )
		)
	)
	(close)
)

(deffunction give-advise(?career-name ?student-name ?weight)  ;this function prints out to the student the suggested career for them and the requirements for that career
	(printout t crlf crlf "Well well well, " (upcase ?student-name) " :-D" crlf)
	(printout t "I think that the career path that fits you most is ... " crlf crlf "Drumrolls..." crlf crlf (upcase ?career-name) "!!!!!" crlf)
	(printout t "These are the necessary requirements for the career" crlf crlf)
	(open advice.txt file-data)
	(bind ?stop FALSE)
	(bind ?print FALSE)
	(while(not ?stop)
		(bind ?temp-line (readline file-data))
		(if (eq ?temp-line EOF)
			then (bind ?stop TRUE)
			else(
				if (eq ?temp-line "")
					then (printout t "")
			else(
				if(eq (str-compare ?temp-line ?career-name) 0)
					then (bind ?print TRUE)
			else(
				if (eq ?temp-line "NODE")
					then (bind ?print FALSE)
			else
				(if ?print
					then (printout t ?temp-line crlf)
			))))))
	(close)
) 


(defrule start-program  ; this is the first rule that the advisory system will run as it has been assigned first priority (salience of 1)
   (declare (salience 1))
   =>
   (read-careers careers.txt) ; passes the file name careers.txt to the read-careers function argument
)

(defrule coalesce-similar-weights  ;
	(declare (salience -1))
	?fitting-career <- (fitting-careers (career-name ?career-name)
		(student-name ?student-name)
		(weight ?weight))
	?q1 <- (qualification (student-name ?student-name) (trait ?trait1))
	?q2 <- (qualification (student-name ?student-name) (trait ?trait2))
	?cr1 <- (career-requirements (trait ?trait1) (career-name ?career-name) (weight ?weight1))
	?cr2 <- (career-requirements (trait ?trait2) (career-name ?career-name) (weight ?weight2))
	(test (eq ?weight1 ?weight2))
	(test (neq ?q1 ?q2))
	(test (neq ?cr1 ?cr2))
	=>
	(modify ?fitting-career (weight (+ ?weight ?weight1)))
	(retract ?q1)
)

(defrule evaluate-fitting-careers
	(career-requirements 
	(trait ?trait) (career-name ?career-name) (weight ?weight))
	(qualification (student-name ?student-name) (trait ?trait))
	=>
	(assert
		(fitting-careers
			(career-name ?career-name)
			(student-name ?student-name)
			(weight ?weight)
		)
	)
)

(defrule get-best-career
	(declare (salience -11))
	?best-fit <- (fitting-careers (career-name ?career-name) (student-name ?student-name)(weight ?weight))
	(forall (fitting-careers (career-name ?c-name) (student-name ?student-name)(weight ?w))
	          (test (<= ?w ?weight)))
	=>
	(give-advise ?career-name ?student-name ?weight)
)

(defrule aggregate-weights
	?first <-(fitting-careers
		(career-name ?career-name)
		(student-name ?student-name)
		(weight ?weight1)
	)
	?second <- (fitting-careers
		(career-name ?career-name)
		(student-name ?student-name)
		(weight ?weight2)
	)
	(test (neq ?first ?second))
	=>
	(modify ?first (weight (+ ?weight1 ?weight2)))
	(retract ?second)
)
