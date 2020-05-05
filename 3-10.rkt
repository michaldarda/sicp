(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance
                       (- balance amount))
                 balance)
          "Insufficient funds"))))

;; is equivalent to

(define (make-withdraw initial-amount)
  ((lambda (balance)
     (lambda (amount)
       (if (>= balance amount)
           (begin (set! balance (- balance amount))
                  balance)
           "Insuficient funds")))) initial-amount)

;; Global env

;; make-withdraw: -> parameters initial-amount, body, env global

(define W1 (make-withdraw 100))

;; E1 -> Global
;; initial-amount: 100

;; E2 -> E1
;; balance: 100

;; Global
;; W1 -> parameters amount,
;; body: (lambda (amount) ...), E2

(W1 50)

;; E3 -> E2
;; amount: 50

;; E2 -> E1 (changes value inside)
;; balance: 50

;; conclusion: let creates additional binding

(define W2 (make-withdraw 100))

;; E4 -> Global
;; initial-amount: 100

;; E5 -> E4
;; balance: 100

;; Global
;; W2 -> parameters amount,
;; body: (lambda (amount) ...), E5
