#lang racket

(define (deep-reverse xs)
  (cond [(null? (cdr xs)) xs]
        [(list? (car xs)) (append (deep-reverse (cdr xs))
                                  (list (deep-reverse (car xs))))]
        [else (append (deep-reverse (cdr xs))
                      (list (car xs)))]))

(deep-reverse (list 1 (list 2 3 4) 3))
