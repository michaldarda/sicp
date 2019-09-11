#lang racket

(define (for-each f seq)
  (if (null? seq)
      #t
      (and (f (car seq))
           (for-each f (cdr seq)))))

(for-each (lambda (x)
            (display x)
            (newline))
          (list 57 321 88))
