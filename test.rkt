#lang racket
(require "./tral.rkt")
(define inventory '())
(define actions (add-inventory-actions (list
					(list '("inventory") (lambda (v)
								   (lambda (state)
								     (displayln inventory)
								     state)))
					(list '("get" "lamp") (lambda (v n)
								    (lambda (state)
								      (displayln (cond
										  [(equal? state "Entrance To Cave")
										   (set! inventory (cons n inventory))
										   "Taken."]
										  [else "You can't see any such thing."]))
								      state))))))
(define room-entrance (room (title "Entrance To Cave")
			    (desc "You are in a fairly open field. To your north, the ground opens into a cave.")
			    '("lamp")))
(define room-cave (room (title
			  (if (member "lamp" inventory)
			      "Cave"
			      "Darkness"))
			(desc
			  (if (member "lamp" inventory)
			      "A large, typical cave."
			      "It's to dark to see in here. You are likely to be eaten by a grue."))
			'()))

(define main-fsm (make-fsm
		  (list
		   (list "Cave" "south" "Entrance To Cave" room-entrance)
		   (list "Entrance To Cave" "north" "Cave" room-cave)
		   (list "start" "begin" "Entrance To Cave" room-entrance))))

(repl main-fsm #:actions actions)

