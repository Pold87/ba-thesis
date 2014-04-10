(define (domain garys-hacker-world)

	(:requirements	 
  	:typing
    :negative-preconditions)
	
	(:types
		hacker non-hacker - person
		white-hat gray-hat black-hat - hacker
		application system tool - software
		driver os - system
		pizza person software - object)

	(:predicates
		(has ?s - software ?p - person)
		(hungry ?p - person)
		(vulnerable ?s - software)
		(exploited ?s - software))

  ;; Exploit vulnerable software of a victim
	(:action exploit	
		:parameters (?h - hackers ?s - software ?p - person)
		:precondition (and (has ?s ?p)
						    			 (vulnerable ?s)
						   				 (not (hungry ?h)))
		:effect (exploited ?s))

	;; Eat a delicious pizza
	(:action eat-pizza
		:parameters (?pi - pizza ?p - person)
		:precondition (hungry ?p)
		:effect (not (hungry ?p))))