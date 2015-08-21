#lang racket
(require "./tral.rkt")
(define inventory '())
(define-values (thing-places
		object-list) (place-things
			      (things '("lamp") "Entrance To Cave")
			      (things '("sword") "Cave")))

(define actions (add-inventory-actions (hash)
				       (action '("inventory") (lambda (v)
								(lambda (state)
								  (say inventory)
								  state)))
				       (action '("drop" _) (lambda (v n)
							     (lambda (state)
							       (set! inventory
								     (drop-item n state thing-places inventory)))))
				       (action '("get" _) (lambda (v n)
							    (lambda (state)
							      (set! inventory
								    (pickup-item n state thing-places inventory)))))))

(define room-entrance (room (title "Entrance To Cave")
			    (desc "You are in a fairly open field. To your north, the ground opens into a cave. You can see a small brass lamp here.")))

(define room-cave (room (title
			 (if (member "lamp" inventory)
			     "Cave"
			     "Darkness"))
			(desc
			 (if (member "lamp" inventory)
			     "A large, typical cave. You notice a plastic sword here, lying proped up on the wall."
			     "It's to dark to see in here. You are likely to be eaten by a grue."))))

(define main-fsm (make-fsm
		  (movement "Cave" "south" "Entrance To Cave" room-entrance)
		  (movement "Entrance To Cave" "north" "Cave" room-cave)
		  (movement "start" "begin" "Entrance To Cave" room-entrance)))

(repl main-fsm #:actions actions
      #:object-list object-list)

