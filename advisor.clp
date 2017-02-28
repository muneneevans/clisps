(deftemplate MAIN::career-choice
   (slot name (type SYMBOL) (default ?DERIVE))
   (slot first-salary (type NUMBER) (default 30000)))

(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
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


(defrule determine-student "Rules for when no plant name or diagnosis is available"
    (analysis)
    (not (diagnosis ?))
    (not (plant-name ?))
    =>
    (assert (plant-name (which-plant "Which type of plant has a problem?  (1.cabbage 2.banana 3.maize 4.rose)? "))))