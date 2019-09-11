#lang racket

(define (square x) (* x x))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              ;; here instead of
              ;; (cons answer
              ;;       (square (car things))))))
              ;; should be
              (append answer
                      (list (square (car things)))))))
  (iter items '()))

(square-list (list 1 2 3 4 5))
