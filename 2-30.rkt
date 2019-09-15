#lang racket

(define (square x) (* x x))

(define tree (list 1
                   (list 2 (list 3 4) 5)
                   (list 6 7)))

(define (square-tree t)
  (cond
    [(null? t) null]
    [(not (pair? t)) (* t t)]
    [else (cons (square-tree (car t)) (square-tree (cdr t)))]))

(square-tree tree)

(define (square-tree-2 xs)
  (map (lambda (x)
         (cond [(list? x) (square-tree-2 x)]
               [else (square x)]))
       xs
       ))

(square-tree-2 tree)
