#lang racket

(define (cont-frac n d k)
  (define (cont-frac-inner counter)
    (if (= counter k) (/ (n counter) (d counter))
        (/ (n counter) (+ (d counter) (cont-frac-inner (+ counter 1))))))
  (cont-frac-inner 0))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)

(define (fib n)
  (define (fib-iter a b counter)
    (if (= counter 0) b
        (fib-iter (+ a b) a (dec counter))))
  (fib-iter 1 0 n))
