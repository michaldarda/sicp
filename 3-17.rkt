#lang sicp

(define count-pairs
  (let ([counted '()])
    (lambda (x)
      (cond [(not (pair? x)) 0]
            [(memq x counted) 0]
            [else (begin
                    (set! counted (cons x counted))
                    (+ (count-pairs (car x))
                       (count-pairs (cdr x))
                       1))]))))

(define x (list 'foo 'bar 'baz))
(count-pairs x)

(define z (list 'foo))
(define y (cons z z))
(count-pairs (list y))
