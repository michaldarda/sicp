#lang racket

(define (f)
  (let ([state 1])
    (lambda (x)
      (set! state (* state x))
      state)))

(define f1 (f))
(define f2 (f))

(+ (f1 0) (f1 1))
(+ (f2 1) (f2 0))
