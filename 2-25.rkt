#lang racket

(define list-1 (list 1 3 (list 5 7) 9))
(car (cdr (car (cddr list-1))))

(caar (list (list 7)))

(define list-3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list-3))))))))))))
