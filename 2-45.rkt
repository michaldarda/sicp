#lang sicp
(#%require sicp-pict)

(define (split f1 f2)
  (define (proc painter n)
    (if (= n 0)
        painter
        (let ((smaller (proc painter (- n 1))))
          (f2 painter (f1 smaller smaller)))))
  proc)

(define right-split (split beside below))
(define up-split (split below beside))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))


(paint (up-split einstein 1))
(newline)
(paint (right-split einstein 1))
(newline)
(paint (corner-split einstein 1))
(newline)
(paint (corner-split einstein 4))
