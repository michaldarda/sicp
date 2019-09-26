#lang racket

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ([x1 (car set1)]
            [x2 (car set2)])
        (cond [(= x1 x2)
               (cons x1 (intersection-set (cdr set1)
                                          (cdr set2)))]
              [(< x1 x2) (intersection-set (cdr set1)
                                           set2)]
              [(> x1 x2) (intersection-set set1
                                           (cdr set2))]))))

(intersection-set (list 1 2 3) (list 1 2 3))

(define (element-of-set? x set)
  (cond [(empty? set) #f]
        [(= x (car set)) #t]
        [(< x (car set)) #f]
        [else (element-of-set? x (cdr set))]))

(define (adjoin-set x set)
  (define (loop set accu)
    (cond [(empty? set) (append accu (list x))]
          [(= x (car set)) set]
          [(< x (car set))
           (append accu (list x) set)]
          [else (loop (cdr set) (append accu (list (car set))))]))
  (loop set null))

(adjoin-set 3 (list 1 2 4))
(adjoin-set 10 (list 1 2 4))
