#lang sicp

(#%require racket/include)
(include "constraint-propagation.rkt")

(define C (make-connector))
(define F (make-connector))

(define (celsius-fahreinheit-converter c f)
  (let ([u (make-connector)]
        [v (make-connector)]
        [w (make-connector)]
        [x (make-connector)]
        [y (make-connector)])
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(celsius-fahreinheit-converter C F)

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)


(set-value! C 25 'user)

(define (averager a b c)
  (let ([u (make-connector)]
        [v (make-connector)])
    (adder a b u)
    (multiplier u v c)
    (constant 0.5 v)
    'ok))

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(averager a b c)

(probe "Average" c)

(set-value! a 2 'a)
(set-value! b 3 'a)
