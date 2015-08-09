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
           (displayln fsm-rev)
           (hash-set-in fsm-rev `(,old ,event)
                        (hash "state" state
                              "action" (action event state))))
         (hash) lst))