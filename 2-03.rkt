#lang racket

(define (average x y) (/ (+ x y) 2))

(define (square x) (* x x))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- x (square guess))) 0.001))

  (define (improve guess)
    (average guess (/ x guess)))

  (define (sqrt-iter x guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter x (improve guess))))
  (sqrt-iter x 1.0))

(define make-point cons)
(define x-point car)
(define y-point cdr)
(define (distance-point p1 p2)
  (sqrt (+ (square (- (x-point p1) (x-point p2)))
           (square (- (y-point p1) (y-point p2))))))
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

(print-point (midpoint-segment s))

;;

(define (make-rectangle top-left dimensions)
  (cons top-left dimensions))
(define (top-left-rectangle r) (car r))
(define (dimensions-rectangle r) (cdr r))

(define (a-dimension-rectangle r) (car (dimensions-rectangle r)))
(define (b-dimension-rectangle r) (cdr (dimensions-rectangle r)))

(define (perimeter-rectangle r)
  (+ (* 2 (a-dimension-rectangle r))
     (* 2 (b-dimension-rectangle r))))

(define (area-rectangle r)
  (* (a-dimension-rectangle r)
     (b-dimension-rectangle r)))

(define rect (make-rectangle (make-point 0 0) (cons 2 3)))

(define rect-perimeter (perimeter-rectangle rect))
(define rect-area (area-rectangle rect))

;; different representation

(define (make-rectangle-2 top-left bottom-right)
  (cons top-left bottom-right))

(define (top-left-rectangle-2 r) (car r))
(define (bottom-right-rectangle-2 r) (cdr r))

(define (a-dimension-rectangle-2 r)
  (let* ([top-left (top-left-rectangle-2 r)]
         [bottom-right (bottom-right-rectangle-2 r)]
         [top-right (make-point
                     (x-point bottom-right)
                     (y-point top-left))])
    (distance-point top-left top-right)))

(define (b-dimension-rectangle-2 r)
  (let* ([top-left (top-left-rectangle-2 r)]
         [bottom-right (bottom-right-rectangle-2 r)]
         [top-right (make-point
                     (x-point bottom-right)
                     (y-point top-left))])
    (distance-point top-right bottom-right)))

;; implementation remained the same, racket
;; prevents for changing the def so I needed to use another name

;; implementation of perimeter-rectangle and area-rectangle
;; remains the same as long point responds to a-dimension
;; and b-dimension
(define (perimeter-rectangle-2 r)
  (+ (* 2 (a-dimension-rectangle-2 r))
     (* 2 (b-dimension-rectangle-2 r))))

(define (area-rectangle-2 r)
  (* (a-dimension-rectangle-2 r)
     (b-dimension-rectangle-2 r)))

(define rect2 (make-rectangle-2 (make-point 0 0) (make-point 2 (- 3))))

(define rect2-perimeter (perimeter-rectangle-2 rect2))
(define rect2-area (area-rectangle-2 rect2))

(< (abs (- rect2-perimeter rect-perimeter)) 0.001)
(< (abs (- rect2-area rect-area)) 0.001)
