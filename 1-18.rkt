#lang racket

(define (assert-eql actual expected)
  (if (not (= expected actual))
      (error "Does not match condition, expected: "
             expected " actual: " actual)
      true))

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
