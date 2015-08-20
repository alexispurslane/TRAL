#lang racket
(require (for-syntax racket/syntax))
(provide make-fsm
	 run
	 drop-item
	 say
	 movement
	 action
	 commands
	 same-as
	 isame-as
	 icommands
	 run-inventory-action
	 parse
	 thing
	 things
	 pickup-item
	 place-things
	 repl
	 title
	 desc
	 name
	 room
	 add-inventory-action
	 add-inventory-actions)

(define actions (hash))

(define (hash-set-in ht ks v [default-value (hash)])
  (cond [(not (list? ks)) (error "ks not a list")]
	[(empty? (cdr ks)) (hash-set ht (car ks) v)]
	[else
	 (hash-set ht
		   (car ks)
		   (hash-set-in (hash-ref ht (car ks) default-value)
				(cdr ks)
				v))]))

(define (hash-ref-in ht ks)
  (define res (foldl (lambda (k ht)
		       (if ht
			   (hash-ref ht k #f)
			   #f)) ht ks))
  (if (not res)
      (lambda ([v ""] [n ""])
	(displayln "You can't do that."))
      res))

(define (make-fsm . lst)
  (foldl (lambda (x fsm)
	   (define-values (old event state action) (values (first x)
							   (second x)
							   (third x)
							   (fourth x)))
	   (define fsm-rev (if (hash-ref fsm old #f)
			       fsm
			       (hash-set fsm old (hash))))
	   (hash-set-in fsm-rev `(,old ,event)
			(hash "state" state
			      "action" (action event state))))
	 (hash) lst))
(define (run fsm command state)
  (define s (hash-ref (hash-ref fsm state (hash)) command (hash "state" state
								"action" (lambda ()
									     (displayln "You can't go that way!")))))
  ((hash-ref s "action" (lambda ()
			  (lambda ()
			    (displayln "What's that?")))))
  (hash-ref s "state"))

(define commands '("north"
		   "south"
		   "east"
		   "west"
		   "northeast"
		   "southeast"
		   "northwest"
		   "southwest"
		   "up"
		   "down"
		   "look"
		   "examine"
		   "wait"
		   "again"
		   "quit"))
(define same-as (hash
		 "z" "wait"
		 "n" "north"
		 "s" "south"
		 "e" "east"
		 "w" "west"
		 "ne" "northeast"
		 "se" "southeast"
		 "nw" "northwest"
		 "sw" "southwest"
		 "u" "up"
		 "d" "down"
		 "l" "look"
		 "g" "again"))

(define icommands '("drop"
		    "get"
		    "inventory"
		    "examine"))

(define isame-as (hash "take" "get"
		       "grab" "get"
		       "x" "examine"
		       "throw" "drop"
		       "i" "inventory"))

(define (run-inventory-action c state [h (hash)])
  ((hash-ref-in h c) state)
  state)

(define (add-inventory-action action f [h (hash)])
  (hash-set-in h action (apply f action)))

(define (add-inventory-actions [h (hash)] . action-list)
  (foldl (lambda (a ht)
	   (add-inventory-action (first a) (second a) ht)) h action-list))

(define (parse fsm current-state str [h (hash)])
  (define wic #f)
  (define err #f)
  (define input (string-split str))
  (if (not (null? input))
      (let ()
	(define command (if (and (not (member (first input) commands))
				 (not (equal? (first input) "begin")))
			    (hash-ref same-as (first input)
				      (lambda ()
					(set! wic #t)
					(if (not (member (first input) icommands))
					    (hash-ref isame-as (first input) (lambda ()
									       (displayln "What's that?")
									       (set! err #t)))
					    (first input))))
			    (first input)))
	(cond
	 [err current-state]
	 [(and (not (equal? command "quit")) (not wic))
	  (run fsm command current-state)]
	 [(and (not (equal? command "quit")) wic)
	  (run-inventory-action (cons command (rest input)) current-state h)]
	 [(equal? command "quit") command]))
      current-state))

(define (repl fsm #:state [state "start"] #:command [icommand "begin"] #:actions [object-hash (hash)])
  (define res (parse fsm state (or icommand ((lambda ()
					       (display "> ")
					       (read-line)))) object-hash))
  (if (not (equal? res "quit"))
      (repl fsm #:state res #:command #f #:actions object-hash)
      (displayln "Bye!")))


(define-syntax wrap
  (syntax-rules ()
    [(_ body ...) (lambda () body ...)]))
(define-syntax name
  (syntax-rules ()
    [(_ body ...) (lambda () body ...)]))
(define-syntax title
  (syntax-rules ()
    [(_ body ...) (lambda () body ...)]))
(define-syntax desc
  (syntax-rules ()
    [(_ body ...) (lambda () body ...)]))

(define (action . lst)
  (if (not (eq? (length lst) 2))
      (error "Expected two arguments: a list (the command), and a procedure.")
      lst))
(define (movement . lst)
  (if (not (eq? (length lst) 4))
      (error "Expected four arguments: a string, a string, another string, and a procedure.")
      lst))
(define say displayln)

(define (room name desc)
  (lambda (event state)
    (lambda ()
      (displayln (name))
      (displayln (desc)))))

(define (thing name state [places (hash)])
  (define old (hash-ref places state '()))
  (hash-set places state (cons name old)))

(define (things objs state [places (hash)])
  (foldl (lambda (obj places)
	   (thing obj state places)) places objs))

(define (hash-extend src dst)
  (foldl (lambda (k h)
	   (hash-set h k (hash-ref src k))) dst (hash-keys src)))

(define (place-things . obj-objs)
  (foldl (lambda (h hs)
	   (hash-extend h hs)) (hash) obj-objs))

(define (pickup-item obj state [places (hash)] [inventory '()])
  (if (member obj (hash-ref places state '()))
      (begin (displayln "Taken.")
	     (cons obj inventory))
      (displayln "You can't see any such thing.")))

(define (drop-item obj state [places (hash)] [inventory '()])
  (if (member obj inventory)
      (begin
	(displayln "Dropped.")
	(remove obj inventory))
      (displayln "You don't have that!")))
