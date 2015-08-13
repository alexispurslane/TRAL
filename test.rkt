#lang racket
(require "./tral.rkt")
(define inventory '())
(define actions (add-inventory-actions (list
					(list (list "inventory") (lambda (v)
								   (lambda (state)
								     (displayln inventory)
								     state)))
					(list (list "get" "lamp") (lambda (v n)
								    (lambda (state)
								      (displayln (cond
										  [(equal? state "Entrance To Cave")
										   (set! inventory (cons n inventory))
										   "Taken."]
										  [else "You can't see any such thing."]))
								      state))))))
(define room-entrance (room "Entrance To Cave" "You are in a fairly open field. To your north, the ground opens into a cave." '("lamp")))
(define room-cave (room "Darkness" "It's to dark to see a thing." '()))

(define main-fsm (make-fsm
		  (list
		   (list "Cave" "south" "Entrance To Cave" room-entrance)
		   (list "Entrance To Cave" "north" "Cave" room-cave)
		   (list "start" "begin" "Entrance To Cave" room-entrance))))

(repl main-fsm #:actions actions)

