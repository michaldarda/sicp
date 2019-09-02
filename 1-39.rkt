#lang racket

(define (square x) (* x x))

(define (tan-cf x k)
  (define (next-odd i)
    (+ 1 (* 2 (- i 1))))

  (define (numerator x i)
    (if (= i 1) x (square x)))

  (define (tan-cf-inner i)
    (if (= i k) 1.0
        (/ (numerator x i)
           (- (next-odd i)
              (tan-cf-inner (+ i 1))))))

  (tan-cf-inner 1))


(tan-cf 50 1000)
