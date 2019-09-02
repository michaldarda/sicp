#lang racket

(define (times n f)
  (define (loop i)
    (if (= i n)
        null
        (begin
          (f i)
          (loop (+ i 1)))))
  (loop 1))

;;

(define (cont-frac n d k)
  (define (loop i accu)
    (if (= i 0)
        accu
        (loop (- i 1) (/ (n i) (+ (d i) accu)))))
  (loop k (/ (n k) (d k))))

(define (d i)
  (cond
    [(= i 1) 1]
    [(= (remainder (+ i 1) 3) 0) (* (quotient (+ i 1) 3) 2)]
    [else 1]))


(times 20 (lambda (i) (begin
                       (display i)
                       (display ";")
                       (display (d i))
                       (newline))))

(cont-frac (lambda (x) 1.0)
           d
           10)
