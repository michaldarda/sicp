#lang racket

(define (square x) (* x x))

(define tree (list 1
                   (list 2 (list 3 4) 5)
                   (list 6 7)))


(define (tree-map f xs)
  (map (lambda (x)
         (cond [(list? x) (tree-map f x)]
               [else (f x)]))
       xs))

(define (square-tree tree) (tree-map square tree))
(square-tree tree)
