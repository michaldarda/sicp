#lang racket

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))


;; 2-04.rkt> (cons 1 2)
;; #<procedure:...cp-2019/2-04.rkt:4:2>
;; 2-04.rkt> (car (cons 1 2))
;; 1
;; 2-04.rkt>
;; 2-04.rkt> (car (cons 1 2))
;; 1
;; 2-04.rkt> (cdr (cons 1 2))
;; 2
;; 2-04.rkt> (lambda (m) (m 1 2))
;; #<procedure>
;; 2-04.rkt> ((lambda (m) (m 1 2)) (lambda (p q) p))
;; 1
;; 2-04.rkt> ((lambda (m) (m 1 2)) (lambda (p q) q))

(define (cdr z)
  (z (lambda (p q) q)))
