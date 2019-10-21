(import (chicken random))

(define random pseudo-random-integer)

(define random-init 2137)

(define (rand-update x)
  (let ((m (expt 2 32))
        (a 1664525)
        (b 1013904423))
    (remainder (+ (* a x) b) m)))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define rand
  (let ((x random-init))
    (lambda () (set! x (rand-update x)) x)))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else (iter (- trials-remaining 1)
                      trials-passed))))
  (iter trials 0))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(estimate-pi 1000)

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (test)
    (P (random-in-range x1 x2) (random-in-range y1 y2)))
  (monte-carlo trials test))

(define (P x y)
  (<= (+ (square (- x 5)) (square (- y 7)))
      (square 3)))

(estimate-integral P 2 8 4 10 1000)

(define (estimate-pi)
  (define (circle x y)
    (<= (+ (* x x) (* y y)) 1))
  (* (estimate-integral circle -1 1 -1 1 100000)
     4.0))

(estimate-pi)
