#lang racket

(define (square x) (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (loop accu n)
    (if (= n 0) accu
        (loop (compose f f) (- n 1))))

  (loop f n))

(define dx 0.00001)

(define (smooth f)
  (lambda (x) (/ 3 (+ (- (f x) dx) (f x) (+ (f x) dx)))))

(((repeated smooth 3) square) 5)
