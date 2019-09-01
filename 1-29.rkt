#lang racket

(define (cube x) (* x x x))

(define (simpson f a b n)
  (define h (/ (- b a) n))

  (define (loop n accu b)
    (if (= n 0)
        (+ accu (f a))
        (loop (- n 1)
              (if (even? n)
                  (+ accu (* 2 (f (- b h))))
                  (+ accu (* 4 (f (- b h)))))
              (- b h))))

  (* (/ h 3) (+ (f a) (loop n (f b) b))))


(simpson cube 0 1 10.0)
(simpson cube 0 1 100.0)
(simpson cube 0 1 1000.0)
