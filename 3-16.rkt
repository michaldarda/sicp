#lang sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define x (list 'foo 'bar 'baz))
(count-pairs x)

(define z (list 'foo))
(define y (cons z z))
(count-pairs (list y))
