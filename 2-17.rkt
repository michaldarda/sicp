#lang racket

(define (last-pair xs)
  (let ([x (cdr xs)])
    (if (null? x)
        (car xs)
        (last-pair x))))

(last-pair (list 1 2 3 4))
