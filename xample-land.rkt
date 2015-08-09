#lang racket
(require "./tral.rkt")

(define the-bar-room
  (room "The Bar Room"
        "Another featureless room."
        '("square peg")))
(define the-foo-room
  (room "The Foo Room"
        "A featureless room."
        '("round hole")))

(define main-fsm (make-fsm (list (list "foo" "north" "bar" the-bar-room)
                                 (list "start" "begin" "foo" the-foo-room))))
(repl main-fsm)