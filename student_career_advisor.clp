(defglobal ?*student_weight* = NIL) 

(deftemplate qualification
	(multislot student-name)
	(multislot trait))

(deftemplate career-requirements
	(multislot trait)
	(multislot career-name)
	(slot weight(default 0)))

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
	(facts)
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


(defrule start-program
   (declare (salience 1))
   =>
   (read-careers careers.txt)
)

(defrule evaluate-best-career
	(declare (salience 2))
	(career-requirements (trait ?trait) (career-name ?career-name) (weight ?weight))
	(qualification (student-name ?student-name) (trait ?trait))
	=>
	(printout t ?career-name " - " ?weight crlf)
)