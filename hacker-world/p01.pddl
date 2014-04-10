(define (problem garys-huge-problem)
	
	(:domain garys-hacker-world)

	(:objects	big-pepperoni-pizza - pizza
						gary - white-hat
						gisela - non-hacker
						mysterious-tex-mex-mix - application)

  (:init
  	(hungry gary)
  	(vulnerable mysterious-tex-mex-mix)
  	(has mysterious-tex-mex-mix gisela)) 

 	(:goal
 		(exploited mysterious-tex-mex-mix)))