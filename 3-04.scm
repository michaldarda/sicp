(define (make-account balance passwd)
  (let ([invalid-access-attempts 0])
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

     (define (call-the-cops)
       (display "Calling the cops nee-naw nee-naw!"))

     (define (guard user-passwd message)
       (define (wrong-password . args)
         (if (>= invalid-access-attempts 7)
             (call-the-cops)
             (begin
               (set! invalid-access-attempts (+ invalid-access-attempts 1))
               "Wrong password")))

       (if (eq? user-passwd passwd)
           (dispatch message)
           wrong-password))

     guard))

(define acc (make-account 100 'szikaka))

((acc 'foo 'deposit) 100)
((acc 'foo 'deposit) 100)
((acc 'foo 'deposit) 100)
((acc 'foo 'deposit) 100)
((acc 'foo 'deposit) 100)
((acc 'foo 'deposit) 100)
((acc 'foo 'deposit) 100)
((acc 'foo 'deposit) 100)
