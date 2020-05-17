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
                (insert-tree k value (left-branch tree)))]
              [(eq? comparison-result -1)
               (make-tree
                (entry tree)
                (insert-tree k value (right-branch tree)))]))))

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

(define (make-table)
  (list '*table*))

(define (compare char-a char-b)
  (let ([charcode-a (char->integer char-a)]
        [charcode-b (char->integer char-b)])
    (cond [(< charcode-a charcode-b) 1]
          [(> charcode-a charcode-b) -1]
          [else 0])))

(define x (make-table))

(insert! #\A 1 x)
(lookup #\A x)
