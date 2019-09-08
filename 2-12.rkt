#lang racket

(define (percent->float p)
  (* p 0.01))

(define (float->percent n)
  (* n 100))

;;

(define (make-interval a b) (cons a b))

(define (lower-bound interval) (car interval))

(define (upper-bound interval) (cdr interval))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center int)
  (/ (+ (lower-bound int) (upper-bound int)) 2))

(define (width int)
  (/ (- (upper-bound int) (lower-bound int)) 2))

;; p - percentage value 0-100
(define (make-center-percent c p)
  (make-interval (- c (percent->float (* c p)))
                 (+ c (percent->float (* c p)))))

(define (percentage-tolerance-interval int)
  (let ([center (center int)]
        [lower (lower-bound int)])
    (float->percent (/ (- center lower) center))))

(define tolerance 50)
(define int (make-center-percent 50 tolerance))

(= (percentage-tolerance-interval int) tolerance)
