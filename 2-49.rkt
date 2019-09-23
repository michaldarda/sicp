#lang racket
(#%require sicp-pict)

(let ((top-left (make-vect 0 1))
      (top-right (make-vect 1 1))
      (bottom-left (make-vect 0 0))
      (bottom-right (make-vect 1 0)))
  (paint (segments->painter (list
                             (make-segment top-left top-right)
                             (make-segment top-right bottom-right)
                             (make-segment bottom-right bottom-left)
                             (make-segment bottom-left top-left)
                             ))))

(let ((top-left (make-vect 0 1))
      (top-right (make-vect 1 1))
      (bottom-left (make-vect 0 0))
      (bottom-right (make-vect 1 0)))
  (paint (segments->painter (list
                             (make-segment top-left bottom-right)
                             (make-segment top-right bottom-left)
                             ))))

(let ((top-left (make-vect 0 0.5))
      (top-right (make-vect 0.5 0.5))
      (bottom-left (make-vect 0 0))
      (bottom-right (make-vect 0.5 0)))
  (paint (segments->painter (list
                             (make-segment top-left top-right)
                             (make-segment top-right bottom-right)
                             (make-segment bottom-right bottom-left)
                             (make-segment bottom-left top-left)
                             ))))
