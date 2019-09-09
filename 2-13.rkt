#lang racket

(define (mul-interval x y)
  (let ([p1 (* (lower-bound x) (lower-bound y))]
        [p2 (* (lower-bound x) (upper-bound y))]
        [p3 (* (upper-bound x) (lower-bound y))]
        [p4 (* (upper-bound x) (upper-bound y))])
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (make-interval a b) (cons a b))

(define (lower-bound interval) (car interval))

(define (upper-bound interval) (cdr interval))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center int)
  (/ (+ (lower-bound int) (upper-bound int)) 2))

(define (width int)
  (/ (- (upper-bound int) (lower-bound int)) 2))

(define (make-center-percent c p)
  (make-interval (- c (* c p))
                 (+ c (* c p))))

(define (percentage-tolerance-interval int)
  (let ([center (center int)]
        [lower (lower-bound int)])
    (/ (- center lower) center)))

(define tolerance 0.5)
(define int1 (make-center-percent 50 tolerance))
(define int2 (make-center-percent 50 0.4))

(percentage-tolerance-interval (mul-interval int1 int2))
(+ (percentage-tolerance-interval int1) (percentage-tolerance-interval int2))
