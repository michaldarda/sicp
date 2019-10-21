(define random-init 2137)

(define (rand-update x)
  (let ((m (expt 2 32))
        (a 1664525)
        (b 1013904423))
    (remainder (+ (* a x) b) m)))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (rand m)
  (let ((x random-init))
    (define (generate)
      (set! x (rand-update x))
      x)
    (define (reset new-value)
      (set! x new-value))
    (cond ((eq? m 'generate)
           (generate))
          ((eq? m 'reset)
           reset)
          (else (error "Unknown method -- RAND" m)))))

(rand 'generate)
((rand 'reset) 10)
(rand 'generate)
