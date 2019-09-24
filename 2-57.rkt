#lang racket

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum . as)
  (append (list '+) as))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addent s) (cadr s))
(define (augend s)
  (let ([rest (cddr s)])
    (if (= (length rest) 1)
        (car rest)
        (apply make-sum rest))))

(define (make-product . ms)
  (append (list '*) ms))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p)
  (let ([rest (cddr p)])
    (if (= (length rest) 1)
        (car rest)
        (apply make-product rest))))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e))
         (expt b e))
        (else (list '** b e))))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addent exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (exponent exp)
          (make-exponentiation (base exp) (- (exponent exp) 1))))
        (else (error "unknown expression type: DERIV" exp))))

(deriv '(* x y (+ x 3)) 'x)
