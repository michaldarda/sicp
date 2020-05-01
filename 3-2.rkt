#lang racket

(define (make-monitored f)
  (define calls 0)
  (define (call-and-increment! arg)
    (begin (set! calls (+ calls 1))
           (f arg)))
  (define (dispatch m)
    (cond [(eq? m 'how-many-calls?) calls]
          [else (call-and-increment! m)]))
  dispatch)

(define (square x)
  (* x x))

(define msquare (make-monitored square))

(msquare 2)

(msquare 8)

(msquare 64)

(msquare 'how-many-calls?)
