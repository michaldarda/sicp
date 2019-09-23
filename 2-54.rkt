#lang racket

(define (equal? a b)
  (if (and (null? a)
           (null? b))
      #t
      (and (eq? (car a) (car b))
           (equal? (cdr a) (cdr b)))))

(define a '(a b c))
(define b '(a (is a) c))
(define c '(a d c))
(define d '(a d c d))

(equal? a a)
(equal? b b)
(equal? c c)
(equal? d d)
(equal? a d)
(equal? a b)
(equal? a c)
(equal? c b)
(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))
