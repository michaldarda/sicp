#lang racket

(define (make-segment start end)
  (cons start end))

(define start-segment car)
(define end-segment cdr)
