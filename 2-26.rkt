#lang racket

(define l1 (list 1 2 3 4))
(define l2 (list 1 2 3 4))
(cons l1 l2)
(pair? (cons l1 l2))
(list? (cons l1 l2))
(car (cons l1 l2))
(cdr (cons l1 l2))
(append l1 l2)
(list l1 l2)
