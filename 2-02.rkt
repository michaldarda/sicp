#lang racket

(define (average x y) (/ (+ x y) 2))

(define make-point cons)
(define x-point car)
(define y-point cdr)
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)
(define (midpoint-segment s)
  (let ([start (start-segment s)]
        [end (end-segment s)])
    (make-point (average (x-point start) (x-point end))
                 (average (y-point start) (y-point end)))))

(define p1 (make-point 0 1))
(define p2 (make-point 2 2))

(define s (make-segment p1 p2))

; (print-point (midpoint-segment s))
