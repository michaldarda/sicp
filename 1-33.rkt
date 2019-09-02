#lang racket

(define (assert-eql actual expected)
  (if (not (= expected actual))
      (error "Does not match condition, expected: "
             expected " actual: " actual)
      true))

(define (inc x) (+ 1 x))
(define (square x) (* x x))

(define (divides? a b) (= (remainder b a) 0))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
  (= n (smallest-divisor n)))

;;

(define (filtered-accumulate combiner null-value
                             term a next b
                             predicate)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (if (predicate a)
                  (combiner result (term a))
                  result))))
  (iter a null-value))

(define (sum-of-prime-squares a b)
  (filtered-accumulate + 0 square a inc b prime?))
