#lang racket

;; lets assume this guy is given to us
(define (rand-update x)
  (modulo (+ (* 214013 x) 2531011) (expt 2 32)))
;;

(define random-init 1)

(define rand
  (let ([x random-init])
    (lambda (message)
      (define (generate)
        (set! x (rand-update x))
        x)
      (define (reset new-val)
        (set! x new-val))

      (cond [(eq? message 'generate) (generate)]
            [(eq? message 'reset) reset]))))

(rand 'generate)
(rand 'generate)
(rand 'generate)

((rand 'reset) 16)

(rand 'generate)
(rand 'generate)
