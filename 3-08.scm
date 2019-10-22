(define state 1)

(define (f x)
  (set! state (* state x))
  state)
