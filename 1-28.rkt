#lang racket

(define (assert condition)
  (if (not condition)
      (error "Condition not true")
      true))

(define (assert-not condition)
  (if condition
      (error "Condition not true")
      true))

(define (square x) (* x x))

(define (expmod base exp m)
  (define (non-trivial-square-root? base exp)
    (and (not (= base 1))
         (= base exp)
         (= (square base) (remainder 1 exp))))

  (cond [(= exp 0) 1]
        [(non-trivial-square-root? base exp) 0]
        [(even? exp) (remainder (square (expmod base (/ exp 2) m)) m)]
        [else (remainder (* base (expmod base (- exp 1) m)) m)]))

(define (miller-rabin-test n)
  (define (try-it a)
    (if (= a 0)
        true
        (and (= (expmod a (- n 1) n) 1) (try-it (- a 1)))))
  (try-it (- n 1)))

(assert-not (miller-rabin-test 561))
(assert-not (miller-rabin-test 1105))
(assert-not (miller-rabin-test 1729))
(assert-not (miller-rabin-test 2465))
(assert-not (miller-rabin-test 2821))
(assert-not (miller-rabin-test 6601))

(assert (miller-rabin-test 1009))
(assert (miller-rabin-test 1013))
(assert (miller-rabin-test 1019))
(assert (miller-rabin-test 10007))
(assert (miller-rabin-test 10009))
(assert (miller-rabin-test 10037))
(assert (miller-rabin-test 100003))
(assert (miller-rabin-test 100019))
(assert (miller-rabin-test 100043))
