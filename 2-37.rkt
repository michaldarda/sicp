#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op initial seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op initial (map car seqs))
            (accumulate-n op initial (map cdr seqs)))))


(define m (list (list 1 2 3 4)
                (list 4 5 6 6)
                (list 6 7 8 9)))

(define ?? "missing")

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (r) (accumulate + 0 (map * r v))) m))

(matrix-*-vector (list (list 1 2 3)
                       (list 4 5 6)
                       (list 7 8 9))
                 (list 2 1 3))

(define (transpose m)
  (accumulate-n cons '() m))

(transpose m)

(define (matrix-*-matrix m n)
  (let ([cols (transpose n)])
    (map (lambda (r) (matrix-*-vector cols r)) m)))

(matrix-*-matrix (list (list 0 1)
                       (list 0 0))
                 (list (list 0 0)
                       (list 1 0)))

(matrix-*-matrix (list (list 1 2)
                       (list 3 4))
                 (list (list 2 0)
                       (list 1 2)))
