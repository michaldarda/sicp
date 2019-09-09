#lang racket

(define (same-parity x . xs)
  (define (filter xs accu predicate)
    (cond
      [(null? xs) accu]
      [(predicate (car xs)) (filter (cdr xs)
                                    (append accu (list (car xs)))
                                    predicate)]
      [else (filter (cdr xs) accu predicate)]))
  (filter xs
          (list x)
          (if (even? x) even? odd?)))

(same-parity 1 2 3 4 5 6 7 8 9)
(same-parity 2 3 4 5 6 7 8 9)
