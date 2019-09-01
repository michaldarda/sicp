#lang racket

(define (assert-eql actual expected)
  (if (not (= expected actual))
      (error "Does not match condition, expected: "
             expected " actual: " actual)
      true))

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
