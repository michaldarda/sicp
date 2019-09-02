#lang racket

(define tolerance 0.00001)
(define dx 0.00001)

(define (square x) (* x x))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (average a b) (/ (+ a b) 2))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (loop accu n)
    (if (= n 0) accu
        (loop (compose f f) (- n 1))))

  (loop f n))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let [(next (f guess))]
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-dumped f)
  (lambda (x) (average x (f x))))

(define (nth-root n x)
  (fixed-point
   ((repeated average-dumped (- n 1)) (lambda (y) (/ x (expt y (- n 1)))))
   1.0))

(define (sqrt x)
  (nth-root 2 x))
