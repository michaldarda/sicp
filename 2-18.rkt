#lang racket

(define (reverse-recur xs)
  (if (null? (cdr xs))
      xs
      (append (reverse-recur (cdr xs)) (list (car xs)))))

(reverse-recur (list 1 2 3))

(define (reverse-iter xs)
  (define (loop xs accu)
    (if (null? xs)
        accu
        (loop (cdr xs) (cons (car xs) accu))))
  (loop xs '()))

(reverse-iter (list 1 2 3))
