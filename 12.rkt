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
