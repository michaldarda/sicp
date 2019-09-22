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

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

;;

(define (make-queen row col)
  (cons row col))

(define (row-queen qc)
  (car qc))

(define (col-queen qc)
  (cdr qc))

(define (same-queen? a b)
  (and (= (row-queen a) (row-queen b))
       (= (col-queen a) (col-queen b))))

(define (in-check? a b)
  "Checks if queens a b are in check"
  (define (same-row? a b)
    (= (row-queen a) (row-queen b)))
  (define (same-col? a b)
    (= (col-queen a) (col-queen b)))
  (define (same-diagonal? a b)
    (let ([xa (row-queen a)]
          [xb (row-queen b)]
          [ya (col-queen a)]
          [yb (col-queen b)])
      (= (abs (- xa xb))
         (abs (- ya yb)))))
  (or (same-row? a b)
      (same-col? a b)
      (same-diagonal? a b)))

(define empty-board null)
(define (safe? k positions)
  (letrec ([queen (list-ref positions (- k 1))]
           [queens-that-are-in-check
            (filter (lambda (q)
                      (and (not (same-queen? queen q))
                           (in-check? queen q)))
                    positions)])
    (empty? queens-that-are-in-check)))

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list (make-queen new-row k))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 1)
(queens 2)
(queens 6)
