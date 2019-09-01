#lang racket

(define (square x) (* x x))
(define (runtime) (current-inexact-milliseconds))

(define (expmod base exp m)
  (cond [(= exp 0) 1]
        [(even? exp) (remainder (square (expmod base (/ exp 2) m)) m)]
        [else (remainder (* base (expmod base (- exp 1) m)) m)]))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond [(= times 0) true]
        [(fermat-test n) (fast-prime? n (- times 1))]
        [else false]))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 5)
      (report-prime n (- (runtime)
                                start-time))
      0))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline))

(define (search-for-primes a b)
  (if (>= a b)
      '()
      (begin
        (timed-prime-test a)
        (search-for-primes (+ a 1) b))))

(search-for-primes 1000 1020)

;; 1009 *** 0.2490234375
;; 1013 *** 0.2548828125
;; 1019 *** 0.261962890625

(search-for-primes 10000 10040)
;; 10007 *** 0.31494140625
;; 10009 *** 0.301025390625
;; 10037 *** 0.30908203125

(search-for-primes 100000 100050)
;; 100003 *** 0.35107421875
;; 100019 *** 0.35595703125
;; 100043 *** 0.35595703125
