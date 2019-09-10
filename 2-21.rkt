#lang racket

(define (square-list xs)
  (map (lambda (x) (* x x)) xs))

(square-list (list 1 2 3 4 5))
