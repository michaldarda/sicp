#lang sicp

(define (assert condition)
  (if (not condition)
      (error "Does not match condition")))

(define (factorial n)
  (if (= n 0) 1
      (* n (factorial (- n 1)))))

(assert (= (factorial 5) 120))

(define (fib n)
  (define (fib-iter a b c)
    (cond ((= c 0) a)
          (else (fib-iter (+ a b) a (dec c)))))
  (fib-iter 1 0 n))

(assert (= (fib 1) 1))
(assert (= (fib 2) 2))
(assert (= (fib 5) 8))
(assert (= (fib 20) 10946))