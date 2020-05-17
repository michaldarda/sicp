#lang racket

(define (make-account balance password)
  (define incorrect-attempts 0)
  (define maximum-incorrect-attempts 7)
  (define (call-the-cops)
    "You have exceeded number of incorrect attempts, calling the cops")
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (erroring-fn msg)
    (lambda (_) msg))
  (define (password-protected f m given-password)
    (if (eq? given-password password)
         (begin
           (set! incorrect-attempts 0)
           (f m))
         (begin
           (set! incorrect-attempts (+ incorrect-attempts 1))
           (erroring-fn
            (if (> incorrect-attempts maximum-incorrect-attempts)
                (call-the-cops)
                "Incorrect password")
            ))))
  (define (dispatch m)
    (cond [(eq? m 'withdraw) withdraw]
          [(eq? m 'deposit) deposit]
          [else (error "Unknown request:
                 MAKE-ACCOUNT" m) ]))

  (lambda (given-password m)
    (password-protected dispatch m given-password)))

(define acc
  (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)


((acc 'some-other-password 'deposit) 50)
