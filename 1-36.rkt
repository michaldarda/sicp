#lang racket

(define tolerance 0.00001)

(define (average a b) (/ (+ a b) 2))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let [(next (f guess))]
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; y = x/y does not converge
;; (define (sqrt x)
;;   (fixed-point (lambda (y) (/ x y)) 1.0))

;; (sqrt 25

;; better
;; y = x/y // + y
;; y + y = x/y + y
;; 2y = x/y + y // 2
;; y = 1/2(x/y + y)
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(sqrt 25)

(display "***")

(fixed-point (lambda (x) (/ (log 1000) (log x))) 1.1)

(display "***")

;; x = log(1000) / log(x) // 2
;; x = log(1000) / log(x) // + x
;; 2x = log(1000) / log(x) + x
;; x = 1/2(log(1000) / log(x) + x)


(fixed-point (lambda (x) (average (/ (log 1000) (log x)) x)) 1.1)

;; conclusion

;; with average dumping fixed-point needed way less steps then without
;; average dumping
