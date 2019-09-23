#lang racket
(#%require sicp-pict)

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter
            painter1
            (make-vect 0.0 0.0)
            split-point
            (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter
            painter2
            split-point
            (make-vect 1.0 0.0)
            (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(paint (beside einstein einstein))

(define (below painter1 painter2)
  (let ((split-point (make-vect 1.0 1.0)))
    (let ((paint-up
           (transform-painter
            painter1
            (make-vect 0 0.5)
            (make-vect 1 0.5)
            (make-vect 0 1)
            ))
          (paint-bottom
           (transform-painter
            painter1
            (make-vect 0 0)
            (make-vect 1 0)
            (make-vect 0 0.5)
            )))
      (lambda (frame)
        (paint-up frame)
        (paint-bottom frame)
        ))))

  (paint (below einstein einstein))
