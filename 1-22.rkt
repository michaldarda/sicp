#lang racket

(define (runtime) (current-inexact-milliseconds))

(define (square x) (* x x))

(define (divides? a b) (= (remainder b a) 0))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime)
                         start-time))
      '()))

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
(search-for-primes 10000 10040)
(search-for-primes 100000 100050)
