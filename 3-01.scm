(define (make-accumulator sum)
  (lambda (n)
    (begin (set! sum (+ sum n))
           sum)))

(define W (make-accumulator 1))
(W 10)
(W 10)
(W 20)

(define W2 (make-accumulator 0))
(W2 0)
(W2 81)
