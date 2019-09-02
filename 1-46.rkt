#lang racket

(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))

(define (iterative-improve good-enough? improve)
  (define (iterative-improver guess)
    (if (good-enough? guess)
        guess
        (iterative-improver (improve guess))))
  iterative-improver)


(define (sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))

  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))

  (define (sqrt-iter guess x)
    ((iterative-improve good-enough? improve) guess))
  (sqrt-iter 1.0 x))

(sqrt 25)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? guess)
    (< (abs (- guess (f guess)))
       tolerance))
  ((iterative-improve close-enough? f) first-guess))

(fixed-point cos 1.0)
