(define (make-account balance passwd)
  (let ([invalid-access-attempts 0]
        [access-passwds (list passwd)])
    (define (withdraw amount)
       (if (>= balance amount)
           (begin (set! balance (- balance amount))
                  balance)
           "Insufficient funds"))

     (define (deposit amount)
       (set! balance (+ balance amount))
       balance)

     (define (add-access passwd)
       (set! access-passwds (cons passwd access-passwds)))

     (define (dispatch message)
       (cond [(eq? message 'withdraw) withdraw]
             [(eq? message 'deposit) deposit]
             [(eq? message 'add-access) add-access]
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

       (if (memq user-passwd access-passwds)
           (dispatch message)
           wrong-password))

     guard))

(define (make-joint acc passwd new-passwd)
  ((acc passwd 'add-access) new-passwd))

(define acc (make-account 100 'szikaka))

(make-joint acc 'szikaka 'foobar)

((acc 'foobar 'deposit) 100)
