#lang racket

(define (make-accumulator state)
  (lambda (increment-by)
    (begin
      (set! state (+ state increment-by))
      state)))

(define A (make-accumulator 5))

(A 10)

(A 10)
