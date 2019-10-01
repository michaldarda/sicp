#lang racket

(define *op-table* (make-weak-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) #f))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (install-standard-deriv-package)
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else (list '+ a1 a2))))

  (define (make-product m1 m2)
    (cond ((or (=number? m1 0)
               (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2))
           (* m1 m2))
          (else (list '* m1 m2))))

  (define (deriv-of-sum operands var)
    (make-sum (deriv (car operands) var)
              (deriv (cadr operands) var)))

  (define (deriv-of-product operands var)
    (let ([multiplier (car operands)]
          [multiplicand (cadr operands)])
      (make-sum
       (make-product
        multiplier
        (deriv multiplicand var))
       (make-product
        (deriv multiplier var)
        multiplicand))))

  (put 'deriv '+ deriv-of-sum)
  (put 'deriv '* deriv-of-product)
  'done)

(install-standard-deriv-package)

(define (install-exponents-derive-package)
  (define (make-exponentiation b e)
    (cond ((=number? e 0) 1)
          ((=number? e 1) b)
          ((and (number? b) (number? e))
           (expt b e))
          (else (list '** b e))))

  (define (make-product m1 m2)
    (cond ((or (=number? m1 0)
               (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2))
           (* m1 m2))
          (else (list '* m1 m2))))

  (define (deriv-of-exponentiation operands var)
    (let ([base (car operands)]
          [exponent (cadr operands)])
      (make-product
       exponent
       (make-exponentiation base (- exponent 1)))))

  (put 'deriv '** deriv-of-exponentiation)
  'done)

(install-exponents-derive-package)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var)
             1
             0))
        (else ((get 'deriv (operator exp))
               (operands exp)
               var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(** x 3) 'x)
