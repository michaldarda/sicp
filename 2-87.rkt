#lang racket
(require threading)

(define ??? null)

(define (square x) (mul x x))
(define (sqrt x) (exp 0.5 x))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (loop accu n)
    (if (= n 0) accu
        (loop (compose accu f) (- n 1))))

  (loop identity n))

;; operations not defined in SICP

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
  (put 'super type super-type)
  (put 'subtype super-type type))

(define (get-super type)
  (get 'super type))

(define (get-subtype type)
  (get 'subtype type))

;; framework

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
  (define (try-drop value)
    (with-handlers ([exn:fail? (lambda (exn) value)])
      (drop value)))

  (let ([type-tags (map type-tag args)])
    (let ([proc (get op type-tags)])
      (if proc
          (try-drop (apply proc (map contents args)))
          (let ([coerced-args (coerce args)])
            (if coerced-args
                (try-drop (apply apply-generic (cons op coerced-args)))
                (error "No method for these types"
                       (list op type-tags))))))))

;; packages

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
  (put 'atane '(integer integer)
       (lambda (x y) (atan x y)))
  (put 'make 'integer
       (lambda (x) (tag x)))
  (put 'sine '(integer)
       (lambda (x) (sin x)))
  (put 'cosine '(integer)
       (lambda (x) (cos x)))
  (put '=zero? '(integer)
       (lambda (x) (zero? x)))
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
  (put 'sine '(real)
       (lambda (x) (sin x)))
  (put 'cosine '(real)
       (lambda (x) (cos x)))
  (put 'atane '(real real)
       (lambda (x y) (atan x y)))
  (put '=zero? '(integer)
       (lambda (x) (zero? x)))
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
  (define (exp x n) (make-rat (exp (numer x) n)
                              (exp (denom x) n)))
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
  (put 'sine '(rational)
       (lambda (x) (sin (/ (numer x) (denom x)))))
  (put 'cosine '(rational)
       (lambda (x) (cos (/ (numer x) (denom x)))))
  (put 'exp '(rational real) exp)
  (put 'atane '(rational rational)
       (lambda (x y) (atane (/ (numer x) (denom x))
                            (/ (numer y) (denom y)))))
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
    (sqrt (add (square (real-part z))
               (square (imag-part z)))))
  (define (angle z)
    (atane (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (cosine a)) (mul r (sine a))))
  (define (=zero-rect? z)
    (and (equ? (real-part z) 0)
         (equ? (imag-part z) 0)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put '=zero? '(rectangular) =zero-rect?)
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
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  (define (=zero-polar? z)
    (and (equ? (real-part z) 0)
         (equ? (imag-part z) 0)))
  (define (equ? z1 z2)
    (and (= (magnitude z1) (magnitude z2))
         (= (angle z1) (angle z2))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put '=zero? '(polar) =zero-polar?)
  (put 'equ? '(polar) equ?)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

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
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
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
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

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

  (define (project val)
    (letrec ([type (type-tag val)]
             [subtype (get-subtype type)]
             [coercion (get-coercion type subtype)])
      (if (null? subtype)
          null
          (if coercion
              (coercion val)
              (error "No way to convert" type "into" subtype)))))

  (define (drop val)
    (let ([subtype (get-subtype (type-tag val))])
      (if subtype
          (let ([vall (project val)])
            (if (equ? val (raise vall))
                (drop vall)
                val))
          val)))


  (define (hierarchy base)
    (define (loop base acc)
      (let ([super-type (get 'super base)])
        (if (false? super-type)
            acc
            (loop super-type (append acc (list super-type))))))

    (loop base null))

  (put-super 'integer 'real)
  (put-super 'real 'rational)
  (put-super 'rational 'complex)
  ;(put-super 'complex null)
  (put 'raise 'hierarchy hierarchy)
  (put 'raise 'raise raise)
  (put 'raise 'project project)
  (put 'raise 'drop drop)
  'done)

(define (install-polynomial-package)
  ;;internal procedures
  (define (make-poly var term-list)
    (cons var term-list))

  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        ;; here we can use the ordered list
        ;; implementation from previous chapters
        ;; it would be more safe to use that
        (cons term term-list)))

  (define (empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list)
    (null? term-list))

  (define (make-term order coeff)
    (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-terms L1 L2)
    (cond [(empty-termlist? L1) L2]
          [(empty-termlist? L2) L1]
          [else
           (let ([t1 (first-term L1)]
                 [t2 (first-term L2)])
             (cond [(> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2))]
                   [(< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2)))]
                   [else
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2)))]))]))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (empty-termlist)
        (let ([t2 (first-term L)])
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))

  (define (=zero-poly? p)
    (andmap =zero? (map coeff (term-list p))))

  ;; interface to the rest of system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put '=zero? '(polynomial) =zero-poly?)
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done
  )

;; generic functions and costructors

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (sine x) (apply-generic 'sine x))
(define (atane x y) (apply-generic 'atane x y))
(define (cosine x) (apply-generic 'cosine x))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (exp x y) (apply-generic 'exp x y))
(define (raise x) ((get 'raise 'raise) x))
(define (project x) ((get 'raise 'project) x))
(define (hierarchy base) ((get 'raise 'hierarchy) base))
(define (drop x) ((get 'raise 'drop) x))

(define (make-real x)
  ((get 'make 'real) x))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

;; coercions up the tower

(define (integer->real n)
  (make-real (contents n)))

(put-coercion 'integer 'real integer->real)

(define (real->rational n)
  (make-rational (contents n) 1))

(put-coercion 'real 'rational real->rational)

(define (rational->complex rat)
  (make-complex-from-real-imag ((get 'to-real 'rational)
                                (contents rat))
                               0))

(put-coercion 'rational 'complex rational->complex)

;; (define (complex->polynomial c)
;;   (make-polynomial 'x (list (0 c))))

;; (put-coercion 'complex 'polynomial complex->polynomial)

;; coercions down the tower

(define (complex->rational z)
  (make-rational (real-part z) 1))

(put-coercion 'complex 'rational complex->rational)

(define (rational->real n)
  (make-real ((get 'to-real 'rational) (contents n))))

(put-coercion 'rational 'real rational->real)

(define (real->integer n)
  (inexact->exact (floor (contents n))))

(put-coercion 'real 'integer real->integer)

(install-integer-package)
(install-real-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-raise-package)
(install-polynomial-package)

(define p1 (make-polynomial 'x (list (list 1 1))))
(define p2 (make-polynomial 'x (list (list 1 0))))
p1
p2
(add p1 p1)
(=zero? (mul p2 p2))
(mul p1 p1)
