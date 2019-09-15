#lang racket

(define (square x) (* x x))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))

(map square '(1 2 3 4 5))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(length '(1 2 3 5))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 2 3) (list 4 5 6))
