#lang racket

(define (make-finite-state-machine lst)
  (define (hash-set-in ht ks v)
    (cond [(not (list? ks)) (error "ks not a list")]
          [(empty? (cdr ks)) (hash-set ht (car ks) v)]
          [else
           (hash-set ht
                     (car ks)
                     (hash-set-in (hash-ref ht (car ks))
                                  (cdr ks)
                                  v))]))
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
                   "up"
                   "down"
                   "look"
                   "examine"
                   "wait"
                   "again"
                   "quit"))

(define icommands '("drop"
                    "get"
                    "inventory"))
(define isame-as (hash "take" "get"
                       "grab" "get"
                       "throw" "drop"
                       "i" "inventory"))
(define same-as (hash
                 "z" "wait"
                 "x" "examine"
                 "n" "north"
                 "s" "south"
                 "e" "east"
                 "w" "west"
                 "u" "up"
                 "d" "down"
                 "l" "look"
                 "g" "again"))

(define (run-inventory-action c)
  (define-values (verb noun prep noun2) (values
                                         (first c)
                                         (second c)
                                         (third c)
                                         (fourth c)))
  verb)

(define (parse fsm current-state str)
  (define wic #f)
  (define input (string-split str))
  (define command (if (and (not (member (first input) commands)) (not (equal? (first input) "begin")))
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
     (run-inventory-action input)]
    [(equal? command "quit") command]))

(define (repl fsm [state "start"] [icommand "begin"])
  (define res (parse fsm state (or icommand (read-line))))
  (if (not (equal? res "quit"))
      (repl fsm res #f)
      (displayln "Bye!")))