#lang sicp

(define (assert-eql actual expected)
  (if (not (= expected actual))
      (error "Does not match condition, expected: " expected " actual: " actual)))

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
  (define (fib-iter a b c)
    (cond ((= c 0) a)
          (else (fib-iter (+ a b) a (dec c)))))
  (fib-iter 1 0 n))

(assert-eql (fib 1) 1)
(assert-eql (fib 2) 2)
(assert-eql (fib 5) 8)
(assert-eql (fib 20) 10946)