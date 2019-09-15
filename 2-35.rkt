#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define x (cons (list 1 2) (list 3 4)))

(define (count-leaves x)
  (accumulate (lambda (x y) (+ 1 y))
              0
              (enumerate-tree x)))

(count-leaves '(1 2 () () (3 () ())))
(count-leaves (list x x))
