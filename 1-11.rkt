#lang racket

;; helpers

(define (dec n) (- n 1))

(define (assert-eql actual expected)
  (if (not (= expected actual))
      (error "Does not match condition, expected: "
             expected " actual: " actual)
      true))

;; examples

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

;; actual

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
