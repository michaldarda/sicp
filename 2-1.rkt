#lang racket

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (define (sign n d)
    (cond [(and (positive? n) (positive? d)) 1]
          [(and (negative? n) (negative? d)) 1]
          [(and (positive? n) (negative? d)) (- 1)]
          [(and (negative? n) (positive? d)) (- 1)]))
  (let ([g (abs (gcd n d))]
        [sign (sign n d)]
        [n (abs n)]
        [d (abs d)])
    (cons (* sign (/ n g)) (/ d g))))
