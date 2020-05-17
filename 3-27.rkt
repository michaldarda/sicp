#lang sicp

(define (make-entry key value)
  (cons key value))

(define (key entry) (car entry))

(define (value entry) (cdr entry))

(define set-value! set-cdr!)

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (lookup-key k tree)
  (if (null? tree)
      false
      (let ([comparison-result (compare k (key (entry tree)))])
        (cond [(eq? comparison-result 0)
               (entry tree)]
              [(eq? comparison-result 1)
               (lookup-key
                k
                (left-branch tree))]
              [(eq? comparison-result -1)
               (lookup-key
                k
                (right-branch tree))]))))

(define (insert-tree k v tree)
  (if (null? tree)
      (make-tree (make-entry k v) '() '())
      (let ([comparison-result (compare k (key (entry tree)))])
        (cond [(eq? comparison-result 0) tree]
              [(eq? comparison-result 1)
               (make-tree
                (entry tree)
                (insert-tree k v (left-branch tree))
                (right-branch tree))]
              [(eq? comparison-result -1)
               (make-tree
                (entry tree)
                (left-branch tree)
                (insert-tree k v (right-branch tree)))]))))

(define (lookup key table)
  (let ([record (lookup-key key (cdr table))])
    (if record
        (value record)
        false)))

(define (insert! key value table)
  (let ([record (lookup-key key (cdr table))])
    (if record
        (set-value! record value)
        (set-cdr! table (insert-tree key value (cdr table)))))
  'ok)

(define (compare a b)
  (cond [(< a b) 1]
        [(> a b) -1]
        [else 0]))

(define (make-table)
  (list '*table*))

;;

(define (memoize f)
  (let ([table (make-table)])
    (lambda (x)
      (let ([memoized (lookup x table)])
        (or memoized
            (let ([result (f x)])
              (insert! x result table)
              result))))))

(define (fib n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define memo-fib
  (memoize
   (lambda (n)
     (cond [(= n 0) 0]
           [(= n 1) 1]
           [else
            (+ (memo-fib (- n 1))
               (memo-fib (- n 2)))]))))

(memo-fib 38)
