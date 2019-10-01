#lang racket

(define (make-from-map-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* x (cos y)))
          ((eq? op 'imag-part)
           (* x (sin y)))
          ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)
