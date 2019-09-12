#lang racket

(define not-pair? (negate pair?))

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch bm)
  (car bm))

(define (right-branch bm)
  (cadr bm))

(define (branch-length br)
  (car br))

(define (branch-structure br)
  (cadr br))

(define (total-weight bm)
  (cond [(not-pair? bm) bm]
        [(not-pair? (right-branch bm)) (right-branch bm)]
        [(not-pair? (left-branch bm)) (total-weight (right-branch bm))]
        [else (+ (total-weight (left-branch bm))
                 (total-weight (right-branch bm)))]))

(define x (make-mobile (make-branch 4 10) (make-branch 4 10)))
(define y (make-mobile (make-branch 4 10) x))

(total-weight x)
(total-weight y)

(define z (make-mobile (make-branch 2 3) (make-branch 2 3)))
(total-weight z)

(define (balanced? bm)
  (define (torque b)
    (* (branch-length b) (total-weight (branch-structure b))))

  (if (not-pair? bm)
      true
      (and (= (torque (left-branch bm))
              (torque (right-branch bm)))
           (balanced? (branch-structure (left-branch bm)))
           (balanced? (branch-structure (right-branch bm))))))

(balanced? x)
(define d (make-mobile (make-branch 10 z) (make-branch 12 5)))
(balanced? d)
