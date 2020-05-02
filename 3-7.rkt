#lang racket

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch given-password m)
    (cond [(not (eq? given-password password))
           (lambda (_) "Incorrect password")]
          [(eq? m 'withdraw) withdraw]
          [(eq? m 'deposit) deposit]
          [else (error "Unknown request:
                 MAKE-ACCOUNT" m) ]))
  dispatch)

(define acc
  (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 50)

(define (make-joint acc password new-password)
  (define (erroring-fn m) (lambda (_) m))
  (lambda (given-password m)
    (if (eq? given-password new-password)
        (acc password m)
        (erroring-fn "Incorrect password"))))

(define paul-acc (make-joint acc 'secret-password 'some-other-password))

((paul-acc 'some-other-password 'withdraw) 40)
((paul-acc 'secret-password 'withdraw) 40)
