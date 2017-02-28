(deffunction read-careers(?file)
	(open ?file file-data)
	(bind ?stop FALSE)
	(bind ?career-name(read file-data))
	(while(not ?stop)
		(bind ?temp-line (readline file-data))
		(if (eq ?temp-line EOF)
			then (bind ?stop TRUE)
			else(
				if (eq ?temp-line "NODE")
					then (bind ?career-name (read file-data))
			else(
				if (eq ?temp-line "")
					then (printout t "")
			else
				(bind ?exploded (explode$ ?temp-line))
				(printout t ?career-name crlf)
				(printout t (implode$ (subseq$ ?exploded 1 1)) crlf)
		   )
		  )
		)
	)
	(close)
)

(defrule start-program
   (declare (salience 1))
   =>
   (read-careers careers.txt))