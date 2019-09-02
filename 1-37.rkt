#lang racket

(define (cont-frac n d k)
  (define (cont-frac-inner i)
    (if (= i k) (/ (n i) (d i))
        (/ (n i) (+ (d i) (cont-frac-inner (+ i 1))))))
  (cont-frac-inner 0))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)

(define (cont-frac-iter n d k)
  (define (loop i accu)
    (if (= i 0)
        accu
        (loop (- i 1) (/ (n i) (+ (d i) accu)))))
  (loop k (/ (n k) (d k))))

(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                10)
