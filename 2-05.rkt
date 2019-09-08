#lang racket

;; 2^a*3^b = y
;; a > 0 && b > 0

;; 2*2*2 * 2.. * 3 * 3 * 3 * 3 =

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car n)
  (define (factors-of-2 n a)
    (cond [(= (remainder n 2) 0) (factors-of-2 (/ n 2) (+ a 1))]
          [(= (remainder n 3) 0) (factors-of-2 (/ n 3) a)]
          [else a]))
  (factors-of-2 n 0))

(define (cdr n)
  (define (factors-of-3 n a)
    (cond [(= (remainder n 2) 0) (factors-of-3 (/ n 2) a)]
          [(= (remainder n 3) 0) (factors-of-3 (/ n 3) (+ a 1))]
          [else a]))
  (factors-of-3 n 0))

(define pair (cons 3 4))
(car pair)
(cdr pair)
