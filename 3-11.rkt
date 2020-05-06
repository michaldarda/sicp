(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    amount)
  (define (dispatch m)
    (cond [(eq? m 'withdraw) withdraw] [(eq? m 'deposit) deposit]
          [else (error "Unknown request: MAKE-ACCOUNT " m)]))
  dispatch)

;; Global
;; make-account:

(define acc (make-account 50))

;; E1 -> Global
;; balance: 50
;; withdraw: (P, E1)
;; deposit: (P, E1)
;; dispatch: (P, E1)

;; Global
;; make-account: (P, Global)
;; acc: (dispatch-P, E1)

((acc 'deposit) 40)

;; all starts with
(acc 'deposit)

;; that results in calling dispatch procedure with E1
;; so new env is created
;;
;; E2->E1
;; m: 'deposit
;;
;; and we get back deposit procedure and E2 gets destroyed
;; then we call

;; (deposit 40)
;;
;; new env gets created
;;
;; E2'->E1
;; amount: 40
;;
;; deposit procedure modifies E1 env
;;
;; E1 -> Global
;; balance: 90
;; withdraw: (P, E1)
;; deposit: (P, E1)
;; dispatch: (P, E1)
;;
;; and returns new balance
90
;; E2' is destroyed

;; then we call
((acc 'withdraw) 60)
;; first we invoke new fn, so new env gots created
;;
;; E2''-> E1
;; m: 'withdraw
;;
;; we got back (withdraw, E1), E2'' gots destroyed
;;
;; then we call
;; (withdraw 40)
;; new env gots created
;;
;; E2'''->E1
;; amount: 60
;; deposit procedure modifies E1 env
;;
;; E1 -> Global
;; balance: 40
;; withdraw: (P, E1)
;; deposit: (P, E1)
;; dispatch: (P, E1)
;; new balance got returned
60
;; E2''' is destroyed

;; local state for account is kept in E1
(define acc2 (make-account 100))
;; above declaration creates new environment E2->Global
;; with its own balance and internal procedures,
;; so nothing is shared between acc and acc2
