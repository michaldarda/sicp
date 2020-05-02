#lang racket

;; lets assume this guy is given to us
(define (rand-update x)
  (modulo (+ (* 214013 x) 2531011) (expt 2 32)))
;;

(define random-init 1)

(define rand
  (let ([x random-init])
    (lambda ()
      (set! x (rand-update x))
      x)))
;;

(define (random-in-range low high)
  (let ([range (- high low)])
    (+ low (random range))))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials
                          cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond [(= trials-remaining 0) (/ trials-passed trials)]
          [(experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1))]
          [else
           (iter (- trials-remaining 1)
                 trials-passed)]))
  (iter trials 0))

(estimate-pi 10)

(define (estimate-integral predicate x1 x2 y1 y2 trials)
  (define (test)
    (predicate (random-in-range x1 x2) (random-in-range y1 y2)))
  (let ([area (* (- x2 x1) (- y2 y1))])
    (* (monte-carlo trials test)
       area)))


(define (square x)
  (* x x))

(define P (lambda (x y)
            (<=
             (+ (square (- x 5))
                (square (- y 7)))
             (square 3))))

;; pi r^2 = computed
;; computed/(3^2)
(define (estimate-pi2 trials)
  (let ([estimated-integral
         (exact->inexact
          (estimate-integral P 2 8 4 10 trials))])
    (/ estimated-integral (square 3))))

(estimate-pi2 10000)
