(deffacts MAIN::duck
   (bachelor Dicky)
   (bachelor Dopey)
   (bachelor Dopey Mallard)
   (bachelor Dicky Dopey)
   (bachelor Dopey Dinky Mallard))

(defrule MAIN::dating-ducks
   (bachelor Dopey $?name)
   =>
   (printout t "Date Dopey " $?name crlf))

(deffunction read-careers(?file)
	(open ?file file-data)
	(bind ?stop FALSE)
	(bind ?career_name (read file-data))
	(while(not ?stop)
		(bind ?temp-line (readline file-data))
		(if (eq ?temp-line EOF)
			then(bind ?stop TRUE)
		else (if (eq ?temp-line "NODE")
			then
			(bind ?careername (read file-data))
			(printout t ?careername crlf)
			)))
	close())