#lang racket

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (let ([x1 (xcor-vect v1)]
        [x2 (xcor-vect v2)]
        [y1 (ycor-vect v1)]
        [y2 (ycor-vect v2)])
    (make-vect (+ x1 x2)
               (+ y1 y2))))

(define (sub-vect v1 v2)
  (let ([x1 (xcor-vect v1)]
        [x2 (xcor-vect v2)]
        [y1 (ycor-vect v1)]
        [y2 (ycor-vect v2)])
    (make-vect (- x1 x2)
               (- y1 y2))))

(define (scale-vect s v)
  (let ([x (xcor-vect v)]
        [y (ycor-vect v)])
    (make-vect (* s x)
               (* s y))))
