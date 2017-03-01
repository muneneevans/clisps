(defglobal ?*student_weight* = NIL) 

(deftemplate qualification
	(slot student-name)
	(slot has-done))

(deftemplate career-requirements
	(multislot name)
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
					(name ?trait)
					(career-name (subseq$ ?related-careers (- ?field-index 1) (- ?field-index 1)))
					(weight ?field)
				)
			)
		)
	)
)

(deffunction read-careers(?file)
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
				(facts)
		   )
		)
	)
	(close)
)

(defrule start-program
   (declare (salience 1))
   =>
   (read-careers careers.txt))