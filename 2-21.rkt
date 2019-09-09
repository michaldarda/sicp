#lang racket

(define (square x) (* x x))

(define (square-list xs) (map square xs))

(square-list (list 1 2 3 4 5))
