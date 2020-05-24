#lang sicp

(#%require racket/include)
(include "constraint-propagation.rkt")

(define (squarer a b)
  (multiplier a a b))

(define a (make-connector))
(define b (make-connector))

(probe "a" a)

(squarer a b)

(set-value! b 25 'repl)

;; that wont work cause a and a are the same
