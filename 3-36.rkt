#lang sicp

(#%require racket/include)
(include "constraint-propagation.rkt")

(define (c+ x y)
  (let ([z (make-connector)])
    (adder x y z)
    z))

(define (c* x y)
  (let ([z (make-connector)])
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ([z (make-connector)])
    ;; small trick, if you change the order of args
    ;; instead of mul you get div cause
    ;; xy = z
    ;; y = z/y
    ;; then
    ;; z -> x
    ;; x -> z
    ;; y -> y
    ;; you need to swap x with z
    (multiplier z y x)
    z))

(define (cv value)
  (let ([c (make-connector)])
    (constant value c)
    c))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

(probe "Celsius" C)
(probe "Fahrenheit" F)

(set-value! C 25 'a)
