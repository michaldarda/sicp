#lang racket

(define (assert-eql actual expected)
  (if (not (= expected actual))
      (error "Does not match condition, expected: "
             expected " actual: " actual)
      true))

(define (inc n) (+ n 1))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(assert-eql (factorial 5) 120)

(define (product-recur term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recur term (next a) next b))))

(define (factorial-recur n)
  (product-recur identity 1 inc n))

(assert-eql (factorial-recur 5) 120)

(define (pi-approx n)
  (define (term n)
    (cond [(even? n) (/ (+ n 2) (+ n 1))]
          [else (/ (+ n 1) (+ n 2))]))
  (* 4.0 (product term 1 inc n)))

(pi-approx 10000)
