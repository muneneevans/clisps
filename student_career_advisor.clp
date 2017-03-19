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

(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
   	  (printout t "Invalid answer. Allowed : " ?allowed-values crlf)
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
        then yes 
    else no))


(deffunction interrogate-student(?student-name ?question ?trait)
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


(deffunction read-careers(?file)
	(printout t " ------------------------------------------------------------------------------ " crlf " - " crlf)
	(printout t " -                          STUDENT CAREER ADVISOR                            - " crlf " - " crlf)
	(printout t " ------------------------------------------------------------------------------ " crlf crlf)
	(printout t " Please enter your first name to continue: ")
	(bind ?student-name (read))
	(printout t crlf " Hello " ?student-name ". Welcome! My name is Adama. I am a career advisor." crlf)
	(open ?file file-data)
	(bind ?stop FALSE)
	(while(not ?stop)
		(bind ?temp-line (readline file-data))
		(if (eq ?temp-line EOF)
			then (bind ?stop TRUE)
			else(
				if (eq ?temp-line "")
					then (printout t "")
			else
				(bind ?exploded (explode$ ?temp-line))
				(bind ?trait (first$ ?exploded))
				(bind ?temp (rest$ ?exploded))
				(bind ?question (implode$ (subseq$ ?exploded 2 2)))
				(bind ?related-careers (rest$ ?temp))
				(create-facts ?trait ?related-careers)
				(interrogate-student ?student-name ?question ?trait)
		   )
		)
	)
	(close)
)

(deffunction give-advise(?career-name ?student-name ?weight)
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


(defrule start-program
   (declare (salience 1))
   =>
   (read-careers careers.txt)
)

(defrule coalesce-similar-weights
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
