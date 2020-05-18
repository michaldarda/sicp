#lang sicp

(define (make-wire)
  (cons 0 '()))
(define get-signal car)
(define set-signal! set-cdr!)

(define (add-action! wire action)
  (let ([actions (car wire)])
    (set-cdr! wire (cons action actions))))

(define (sleep time)
  (let ([start (runtime)])
    (define (loop)
      (let ([elapsed (- (runtime) start)])
        (if (< elapsed time)
            (loop))))
    (loop)))


(define (after-delay delay f)
  (sleep delay)
  f)

(define (logical-and a b)
  (if (eq? a b) a 0))


(define (logical-or a b)
  (cond [(eq? a 1) 1]
        [(eq? b 1) 1]
        [else 0]))

(define and-gate-delay 0)
(define or-gate-delay 0)

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
