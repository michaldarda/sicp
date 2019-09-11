#lang racket

(define (square-list xs)
  (if (null? xs)
      null
      (cons ((lambda (x) (* x x)) (car xs))
            (square-list (cdr xs)))))

(square-list (list 1 2 3 4 5))

(define (square-list-map xs)
  (map (lambda (x) (* x x)) xs))

(square-list-map (list 1 2 3 4 5))
