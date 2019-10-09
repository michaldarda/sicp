#lang racket

(define (square x) (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (loop accu n)
    (if (= n 0) accu
        (loop (compose accu f) (- n 1))))

  (loop identity n))

(define *op-table* (make-weak-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) #f))

(define (put-coercion source-type target-type proc)
  (put 'coercion (list source-type target-type) proc))

(define (get-coercion source-type target-type)
  (get 'coercion (list source-type target-type)))

(define (put-super type super-type)
  (put 'super type super-type))

(define (get-super type)
  (get 'super type))

;;

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'integer)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond [(number? datum) 'integer]
        [(pair? datum) (car datum)]
        [else (error "Bad tagged datum -- TYPE-TAG" datum)]))

(define (contents datum)
  (cond [(number? datum) datum]
        [(pair? datum) (cdr datum)]
        [else (error "Bad tagged datum -- CONTENTS" datum)]))

(define (coerce args)
  (letrec ([hierarchies-length
            (map (lambda (a) (length (hierarchy a))) (map type-tag args))]
           [min-hierarchy (apply min hierarchies-length)])
    (map
     (lambda (arg hierarchy-length)
       (let ([n (- hierarchy-length min-hierarchy)])
         (if (< n 0)
             arg
             ;; raise that many times to match
             ;; item which hierachy is minimal
             ;; in other words which types
             ;; hierarchy to the top is the shortest
             ((repeated raise n) arg))))
     args
     hierarchies-length)))

(define (apply-generic op . args)
  (let ([type-tags (map type-tag args)])
    (let ([proc (get op type-tags)])
      (if proc
          (apply proc (map contents args))
          (let ([coerced-args (coerce args)])
            (if coerced-args
                (apply apply-generic (cons op coerced-args))
                (error "No method for these types"
                       (list op type-tags))))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))
  (put 'exp '(integer integer)
       (lambda (x y) (tag (expt x y))))
  (put 'equ? '(integer integer)
       (lambda (x y) (= x y)))
  (put 'zero? '(integer)
       (lambda (x) (= x 0)))
  (put 'make 'integer
       (lambda (x) (tag x)))
  'done)

(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'exp '(real real)
       (lambda (x y) (tag (expt x y))))
  (put 'equ? '(real real)
       (lambda (x y) (= x y)))
  (put 'zero? '(real)
       (lambda (x) (= x 0)))
  (put 'make 'real
       (lambda (x) (tag (* x 1.0))))
  'done)

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (define (=zero? x)
    (= (numer x) 0))
  ;; it does not coerce to any type, just makes
  ;; the division
  (define (to-real x)
    (let ([numer (numer x)]
          [denom (denom x)])
      (/ (* numer 1.0) denom)))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational) =zero?)
  (put 'to-real 'rational to-real)
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  (define (equ? z1 z2)
    (and (= (magnitude z1) (magnitude z2))
         (= (angle z1) (angle z2))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'equ? '(polar) equ?)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-rectangular-package)
(install-polar-package)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-many-complex . args)
    (let ([real-parts (map real-part args)]
          [imag-parts (map imag-part args)])
      (make-from-real-imag (apply + real-parts)
                           (apply + imag-parts))))
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (equ? z1 z2)
    (and (= (imag-part z1) (imag-part z2))
         (= (real-part z1) (real-part z2))))
  (define (=zero? z)
    (equ? z (make-from-real-imag 0 0)))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex) =zero?)
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-integer-package)
(install-real-package)
(install-rational-package)
(install-complex-package)

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (exp x y) (apply-generic 'exp x y))

(define (make-real x)
  ((get 'make 'real) x))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; coercions could also go to the raise package
(define (integer->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'integer 'complex integer->complex)

(define (integer->real n)
  (make-real (contents n)))

(put-coercion 'integer 'real integer->real)

(define (real->rational n)
  (make-rational (contents n) 1))

(put-coercion 'real 'rational real->rational)

(define (rational->complex rat)
  (make-complex-from-real-imag ((get 'to-real 'rational) (contents rat)) 0))

(put-coercion 'rational 'complex rational->complex)

(define (install-raise-package)
  (define (raise val)
    (letrec ([type (type-tag val)]
             [super-type (get 'super type)]
             [coercion (get-coercion type super-type)])
      (if (null? super-type)
          null
          (if coercion
              (coercion val)
              (error "No way to convert" type "into" super-type)))))

  (define (hierarchy base)
    (define (loop base acc)
      (let ([super-type (get 'super base)])
        (if (null? super-type)
            acc
            (loop super-type (append acc (list super-type))))))

    (loop base null))

  (put 'super 'integer 'real)
  (put 'super 'real 'rational)
  (put 'super 'rational 'complex)
  (put 'super 'complex null)
  (put 'raise 'hierarchy hierarchy)
  (put 'raise 'raise raise)
  'done)

(install-raise-package)

(define (raise x) ((get 'raise 'raise) x))
(define (hierarchy base) ((get 'raise 'hierarchy) base))
(raise 1)
(raise (raise 1))
(raise (raise (raise 1)))

(hierarchy 'integer)
(hierarchy 'complex)
(coerce (list 1 (raise 1) (raise (raise (raise 1)))))

(add 1 (raise 1))
