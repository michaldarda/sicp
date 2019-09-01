#lang sicp

(define (assert-eql actual expected)
  (if (not (= expected actual))
      (error "Does not match condition, expected: "
             expected " actual: " actual)))

(define (factorial n)
  (if (= n 0) 1
      (* n (factorial (- n 1)))))

(assert-eql (factorial 5) 120)

(define (factorial-iter n)
  (define (factorial-iter-inner product counter)
    (if (> counter n) product
        (factorial-iter-inner (* counter product) (+ counter 1))))
  (factorial-iter-inner 1 1))

(assert-eql (factorial-iter 5) 120)

(define (fib n)
  (define (fib-iter a b counter)
    (if (= counter 0) b
        (fib-iter (+ a b) a (dec counter))))
  (fib-iter 1 0 n))

(assert-eql (fib 1) 1)
(assert-eql (fib 2) 1)
(assert-eql (fib 5) 5)
(assert-eql (fib 8) 21)
(assert-eql (fib 20) 6765)

;; 1.11
(define (f-recur n)
  (if (< n 3) n
      (+ (f-recur (- n 1))
         (* 2 (f-recur (- n 2)))
         (* 3 (f-recur (- n 3))))))

(assert-eql (f-recur 3) 4)
(assert-eql (f-recur 4) 11)
(assert-eql (f-recur 10) 1892)

(define (f-iter n)
  (define (f-iter-inner a b c counter)
    (if (= counter 0) c
        (f-iter-inner (+ a (* 2 b) (* 3 c)) a b (- counter 1))))
  (f-iter-inner 2 1 0 n))

(assert-eql (f-iter 3) 4)
(assert-eql (f-iter 4) 11)
(assert-eql (f-iter 10) 1892)

;; 1.12
(define (pascal-triangle row col)
  (cond
    ((< row 0) 0)
    ((< col 0) 0)
    ((< row col) 0)
    ((= row 0) 1)
    ((= col 0) 1)
    ((= col row) 1)
    (else (+
           (pascal-triangle (- row 1) (- col 1))
           (pascal-triangle (- row 1) col)))))

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))


(assert-eql (expt 10 0) 1)
(assert-eql (expt 5 3) 125)

(define (expt-iter b n)
  (define (expt-iter-inner a counter)
    (if (= counter 0)
        a
        (expt-iter-inner (* a b) (- counter 1))))
  (expt-iter-inner 1 n))

(assert-eql (expt-iter 10 0) 1)
(assert-eql (expt-iter 5 3) 125)

(define (fast-expt b n)
  (define (square x) (* x x))
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(assert-eql (fast-expt 10 0) 1)
(assert-eql (fast-expt 5 3) 125)

;; 1.16
(define (fast-expt-iter b n)
  (define (square x) (* x x))
  (define (fast-expt-inner a b counter)
    (cond ((= counter 0) a)
          ((even? counter) (fast-expt-inner a (square b) (/ counter 2)))
          (else (fast-expt-inner (* a b) b (- counter 1)))))
  (fast-expt-inner 1 b n))

(assert-eql (fast-expt-iter 10 0) 1)
(assert-eql (fast-expt-iter 2 2) 4)
(assert-eql (fast-expt-iter 5 3) 125)
(assert-eql (fast-expt-iter 5 5) 3125)

;; 1.17

(define (mul-recur a b)
  (if (= b 0) 0
      (+ a (mul-recur a (- b 1)))))

(assert-eql (mul-recur 1 2) 2)
(assert-eql (mul-recur 2 2) 4)
(assert-eql (mul-recur 36 36) 1296)

(define (mul a b)
  (define (mul-inner b accu)
    (cond [(= b 0) accu]
          [else (mul-inner (- b 1) (+ accu a))]))

  (mul-inner b 0))

(assert-eql (mul 1 2) 2)
(assert-eql (mul 2 2) 4)
(assert-eql (mul 36 36) 1296)

;; 1.18

;; b^(n/2)^2 = (b^2)^n/2

;; a * b = 2(1/2* a * 1/2b)

(define (mul-log-iter a b)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))

  (define (mul-log-inner a b accu)
    (cond [(= b 0) accu]
          [(even? b) (mul-log-inner (double a) (halve b) accu)]
          [else (mul-log-inner a (- b 1) (+ accu a))]))

  (mul-log-inner a b 0))

(assert-eql (mul-log-iter 1 0) 0)
(assert-eql (mul-log-iter 1 2) 2)
(assert-eql (mul-log-iter 2 2) 4)
(assert-eql (mul-log-iter 36 36) 1296)

;; 1.19


(define (fib-log n)
  (define (square x) (* x x))

  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (square p) (square q))
                     (+ (square q) (* 2 p q))
                     (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
  (fib-iter 1 0 0 1 n))


;; gcd

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; Example: Testing for primality
;; naive way

(define (square x) (* x x))

(define (divides? a b) (= (remainder b a) 0))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
  (= n (smallest-divisor n)))

;; fermat test

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

;; 1.21
(assert-eql (smallest-divisor 199) 199)
(assert-eql (smallest-divisor 1999) 1999)
(assert-eql (smallest-divisor 19999) 7)

;; 1.22

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime)
                       start-time))))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline))

;;

(define (search-for-primes a b)
  (timed-prime-test a)
  (if (< a b)
      (search-for-primes (+ a 1) b)))

;; (search-for-primes 1000 1020)
;; 1009 *** 4
;; 1013 *** 4
;; 1019 *** 4

;; (search-for-primes 10000 10038)
;; 10007 *** 12
;; 10009 *** 18
;; 10037 *** 12

;; (search-for-primes 100000 100050)
;; 100003 *** 35
;; 100019 *** 36
;; 100043 *** 36


;; (search-for-primes 1000000 1000050)
;; 1000003 *** 196
;; 1000033 *** 197
;; 1000037 *** 199

;; (sqrt 10)
;; 3.1622776601683795
;; 12.rkt> (* 4 (sqrt 10))
;; 12.649110640673518
;; 12.rkt> (* 12 (sqrt 10))
;; 37.94733192202055
;; 12.rkt> (* 36 (sqrt 10))
;; 113.84199576606166

;; This indeed confirms that this algorithm has O(sqrt(n)) complexity

;; 1.23


(define (fast-smallest-divisor n) (fast-find-divisor n 2))

(define (fast-find-divisor n test-divisor)
  (define (next n)
    (if (= n 2) 3 (+ n 2)))

  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (fast-find-divisor n (next test-divisor)))))

(define (faster-prime? n)
  (= n (smallest-divisor n)))

(define (timed-faster-prime-test n)
  (start-prime-test n (runtime)))

(define (start-faster-prime-test n start-time)
  (if (faster-prime? n)
      (report-faster-prime n (- (runtime)
                         start-time))))

(define (report-faster-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline))

;; 12.rkt> (timed-faster-prime-test 1009)
;; 1009 *** 14
;; 12.rkt> (timed-faster-prime-test 1013)
;; 1013 *** 11
;; 12.rkt> (timed-faster-prime-test 1019)
;; 1019 *** 11

;; 12.rkt> (timed-faster-prime-test 10007)
;; 10007 *** 20
;; 12.rkt> (timed-faster-prime-test 10009)
;; 10009 *** 22
;; 12.rkt> (timed-faster-prime-test 10037)
;; 10037 *** 19

;; 12.rkt> (timed-faster-prime-test 100003)
;; 100003 *** 50
;; 12.rkt> (timed-faster-prime-test 100019)
;; 100019 *** 43
;; 12.rkt> (timed-faster-prime-test 100043)
;; 100043 *** 43

;; 12.rkt> (timed-faster-prime-test 1000003)
;; 1000003 *** 138
;; 12.rkt> (timed-faster-prime-test 1000033)
;; 1000033 *** 137
;; 12.rkt> (timed-faster-prime-test 1000037)
;; 1000037 *** 137

;; Its faster for bigger inputs but for smaller ones seems even slower??

;; 1.24

(define (timed-fast-prime-test n)
  (start-fast-prime-test n (runtime)))

(define (start-fast-prime-test n start-time)
  (if (fast-prime? n 100)
      (report-faster-prime n (- (runtime)
                                start-time))))

(define (report-fast-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline))

;; ex 1.25

;; probably yes, havent tested though

;; ex 1.26

;; this is wrong, if you use square it only computes expmod once,
;; somehow caches it, otherwise it evaluates expmod twice
