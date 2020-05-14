#lang sicp

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

(define contains-cycle?
  (let ([traversed '()])
    (lambda (x)
      (cond [(not (pair? x)) #f]
            [(memq x traversed) #t]
            [else (begin
                    (set! traversed (cons x traversed))
                    (or (contains-cycle? (car x))
                        (contains-cycle? (cdr x))))]))))

(contains-cycle? z)

(contains-cycle? '((list 1 2) (list 1 2)))
