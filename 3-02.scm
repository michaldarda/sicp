(define (make-monitored f)
  (define counter 0)
  (lambda (x)
    (cond [(eq? x 'how-many-times?) counter]
          [(eq? x 'reset-count) (set! counter 0)]
          [else (begin
                  (set! counter (+ counter 1))
                  (f x))])))

(define msqrt (make-monitored sqrt))

(msqrt 2)
(msqrt 2)
(msqrt 'how-many-times?)
(msqrt 'reset-count)
(msqrt 'how-many-times?)
