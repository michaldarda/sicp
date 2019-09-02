#lang racket

(define (square x) (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (loop accu n)
    (if (= n 0) accu
        (loop (compose f f) (- n 1))))

  (loop f n))

((repeated square 2) 5)
