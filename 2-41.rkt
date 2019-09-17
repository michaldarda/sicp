#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define n 10)

(accumulate append null (map (lambda (i)
                               (map (lambda (j) (list i j))
                                    (enumerate-interval 1 (- i 1))))
                               (enumerate-interval 1 n)))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                    (map (lambda (k)
                           (list i j k))
                         (enumerate-interval 1 (- j 1))))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (find-all-triples n s)
  (define (sums-to-s? x) (= (accumulate + 0 x) s))
  (filter sums-to-s? (unique-triples n)))

(find-all-triples n 10)
