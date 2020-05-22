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

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))


(define (full-adder a b c-in sum c-out)
  (let ((c1 (make-wire))
        (c2 (make-wire))
        (s  (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry-adder as bs ss c-in)
  (let ([c-out (make-wire)])
     (if (null? as)
         'ok
         (begin
           (full-adder (car as) (car bs) c-in (car ss) c-out)
           (ripple-carry-adder (cdr as) (cdr bs) (cdr ss) c-out)))))


(define a1 (make-wire))
(define a2 (make-wire))
(define as (list a1 a2))

(define b1 (make-wire))
(define b2 (make-wire))

(define bs (map (lambda (b)
                  (set-signal! b 1)
                  b)
                (list b1 b2)))

(define s1 (make-wire))
(define s2 (make-wire))


(define ss (list s1 s2))

(probe 's1 s1)
(probe 's2 s1)

(define carry (make-wire))

(ripple-carry-adder as bs ss carry)

(propagate)