#lang racket

(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define add1
  (lambda (n) (lambda (f) (lambda (x) (f ((n f) x))))))

;; to be honest I don't understand this..
;; need to go back here and reeavaluate the solution
(define (add n)
  (lambda (m) ((n
                (lambda (n) (lambda (f) (lambda (x) (f ((n f) x)))))
                ) m)))

;; where n is a church numeral
(define (convert n)
  ((n (lambda (x) (+ x 1))) 0))

(convert zero)
(convert one)
(convert two)
(convert (add1 two))
(convert ((add two) two))
