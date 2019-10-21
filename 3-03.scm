(define (make-account balance passwd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch message)
    (cond [(eq? message 'withdraw) withdraw]
          [(eq? message 'deposit) deposit]
          [else (error "Unknown request: MAKE-ACCOUNT"
                       message)]))

  (define (guard user-passwd message)
    (define (wrong-password . args)
      "Wrong password")

    (if (eq? user-passwd passwd)
        (dispatch message)
        wrong-password))

  guard)

(define acc (make-account 100 'szikaka))

((acc 'szikaka 'deposit) 100)
((acc 'foo 'deposit) 100)
