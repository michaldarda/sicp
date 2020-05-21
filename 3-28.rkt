#lang sicp

(#%require racket/include)
(include "wire.rkt")


(define (logical-and a b)
  (if (eq? a b) a 0))


(define (logical-or a b)
  (cond [(eq? a 1) 1]
        [(eq? b 1) 1]
        [else 0]))

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ([new-value (logical-and (get-signal a1)
                                  (get-signal a2))])
      (after-delay
       and-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ([new-value (logical-or (get-signal a1)
                                 (get-signal a2))])
      (after-delay
       or-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure))

(probe 'c c)

(or-gate a b c)

(set-signal! b 1)
(propagate)
