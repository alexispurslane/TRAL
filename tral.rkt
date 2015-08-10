#lang racket
(require (for-syntax racket/syntax))
(provide make-fsm
         run
         commands
         same-as
         isame-as
         icommands
         run-inventory-action
         parse
         repl
         room
	 add-inventory-action)

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

(define (make-fsm lst) 
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
  (define s (hash-ref (hash-ref fsm state) command))
  ((hash-ref s "action"))
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

(define (run-inventory-action c state object-hash)
  (cond
   [(= (length c) 2) ; VERB NOUN, i.e. GET LAMP
    (define verb (first c))
    (define noun (second c))
    verb]
   [(= (length c) 3) ; VERB NOUN NOUN, i.e. SWITCH LAMP ON
    (define verb (first c))
    (define noun (second c))
    (define noun2 (third c))
    verb]
   [(= (length c) 4) ; VERB NOUN PREPOSITION NOUN, i.e. PUT LAMP ON STOOL
    (define verb (first c))
    (define noun (second c))
    (define prep (third c))
    (define noun2 (fourth c))
    verb]))

(define (add-inventory-action action f [h (hash)])
  (hash-set-in h action (apply f action)))

(define (add-inventory-actions action-list [h (hash)])
  (foldl (lambda (a ht)
	   (add-inventory-action (first a) (second a) ht)) h action-list))

(define (parse fsm current-state str object-hash)
  (define wic #f)
  (define input (string-split str))
  (define command (if (and (not (member (first input) commands))
                           (not (equal? (first input) "begin")))
                      (hash-ref same-as (first input)
                                (lambda ()
                                  (set! wic #t)
                                  (if (not (member (first input) icommands))
                                      (hash-ref isame-as (first input))
                                      (first input))))
                      (first input)))
  (cond
   [(and (not (equal? command "quit")) (not wic))
    (run fsm command current-state)]
   [(and (not (equal? command "quit")) wic)
    (run-inventory-action input current-state object-hash)]
   [(equal? command "quit") command]))

(define (repl fsm [state "start"] [icommand "begin"] [object-hash (hash)])
  (define res (parse fsm state (or icommand (read-line)) object-hash))
  (if (not (equal? res "quit"))
      (repl fsm res #f)
      (displayln "Bye!")))

(define (room name desc objs)
  (lambda (event state)
    (lambda ()
      (displayln name)
      (displayln desc)
      (for-each (lambda (obj)
                  (displayln (string-append "You see a " obj " here.")))
                objs))))
