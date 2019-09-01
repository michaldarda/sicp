#lang racket

(define (assert-eql actual expected)
  (if (not (= expected actual))
      (error "Does not match condition, expected: "
             expected " actual: " actual)
      true))

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
