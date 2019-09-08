#lang racket

(define (make-interval a b) (cons a b))

(define (lower-bound interval) (car interval))

(define (upper-bound interval) (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ([p1 (* (lower-bound x) (lower-bound y))]
        [p2 (* (lower-bound x) (upper-bound y))]
        [p3 (* (upper-bound x) (lower-bound y))]
        [p4 (* (upper-bound x) (upper-bound y))])
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (width-interval int)
  (/ (- (upper-bound int) (lower-bound int)) 2))

(define int1 (make-interval 0 2))
(define int2 (make-interval 2 3))

(= (+ (width-interval int1) (width-interval int2))
   (width-interval (add-interval int1 int2)))

(= (- (width-interval int1) (width-interval int2))
   (width-interval (sub-interval int1 int2)))

(= (* (width-interval int1) (width-interval int2))
   (width-interval (mul-interval int1 int2)))

(= (/ (width-interval int1) (width-interval int2))
   (width-interval (div-interval int1 int2)))
