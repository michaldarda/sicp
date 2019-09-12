#lang racket

(define tree (list 1
                   (list 2 (list 3 4) 5)
                   (list 6 7)))

(define (square-tree t)
  (cond
    [(null? t) null]
    [(not (pair? t)) (* t t)]
    [else (cons (square-tree (car t)) (square-tree (cdr t)))]))

(square-tree tree)
