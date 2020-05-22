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

(define (logical-not s)
  (cond [(= s 0) 1]
        [(= s 1) 0]
        [else (error "Invalid signal " s)]))

(define (inverter input output)
  (define (invert-input)
    (let ([new-value (logical-not (get-signal input))])
      (after-delay
       inverter-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ([new-value (logical-and (get-signal a1)
                                  (get-signal a2))])
      (after-delay
       and-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (or-gate a1 a2 output)
  (let ([a1-inverted (make-wire)]
        [a2-inverted (make-wire)]
        [and-result (make-wire)])
    (inverter a1 a1-inverted)
    (inverter a2 a2-inverted)
    (and-gate a1-inverted a2-inverted and-result)
    (inverter and-result output)
    'ok))

(probe 'c c)

(or-gate a b c)

(set-signal! b 1)
(propagate)
