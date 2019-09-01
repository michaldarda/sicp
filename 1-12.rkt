#lang racket

(define (pascal-triangle row col)
  (cond
    ((< row 0) 0)
    ((< col 0) 0)
    ((< row col) 0)
    ((= row 0) 1)
    ((= col 0) 1)
    ((= col row) 1)
    (else (+
           (pascal-triangle (- row 1) (- col 1))
           (pascal-triangle (- row 1) col)))))
