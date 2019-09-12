#lang racket

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch bm)
  (car bm))

(define (right-branch bm)
  (cadr bm))

(define (branch-length br)
  (car br))

(define (branch-structure br)
  (cadr br))

(define (total-weight bm)
  (cond [(not (pair? (right-branch bm))) (right-branch bm)]
        [(not (pair? (left-branch bm))) (total-weight (right-branch bm))]
        [else (+ (total-weight (left-branch bm))
                 (total-weight (right-branch bm)))]))

(define x (make-mobile (make-branch 4 10) (make-branch 4 10)))
(define y (make-mobile (make-branch 4 10) x))
