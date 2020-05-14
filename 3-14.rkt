#lang sicp

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ([temp (cdr x)])
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))

;; 1st iteration

;; temp (list 'b 'c 'd)

;; x =  ('a '())

;; loop (list 'b 'c 'd) (list 'a '())

;; temp 'c 'd
;; x = 'b 'a '()

;; loop ('c 'd) (list 'b 'a)

;; so this fn reverses a list

;; (mystery v) returns ('d 'c 'b 'a)

;; and value of v is 'a (last set-cdr! x y)
