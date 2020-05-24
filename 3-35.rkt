#lang sicp

(#%require racket/include)

(include "constraint-propagation.rkt")

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0: SQUARER"
                   (get-value b))
            (set-value! a
                        (sqrt (get-value b))
                        me))
        (if (has-value? a)
            (set-value! b
                        (* (get-value a) (get-value a))
                        me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me))
  (define (me request)
    (cond [(eq? request 'I-have-a-value) (process-new-value)]
          [(eq? request 'I-lost-my-value) (process-forget-value)]
          [else (error "Unknown request SQUARER" request)]))
  (connect a me)
  (connect b me)
  me)

(define a (make-connector))
(define b (make-connector))

(squarer a b)

(probe "A" a)
(probe "B" b)

(set-value! b 25 'repl)

(forget-value! a 'repl)

(probe "A" a)
(probe "B" b)

(set-value! a 5 'repl)
