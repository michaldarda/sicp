#lang racket

(define (assert-eql actual expected)
  (if (not (= expected actual))
      (error "Does not match condition, expected: "
             expected " actual: " actual)
      true))

(define (inc x) (+ 1 x))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 1 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(assert-eql (product identity 1 inc 5) 120)

(define (accumulate-recur combiner null-value term a next b)
  (if (> a b)
      null-value
      (* (term a)
         (accumulate-recur combiner null-value term (next a) next b))))

(define (sum-recur term a next b)
  (accumulate-recur + 1 term a next b))

(define (product-recur term a next b)
  (accumulate-recur * 1 term a next b))

(assert-eql (product-recur identity 1 inc 5) 120)
