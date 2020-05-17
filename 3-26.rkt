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

(define (lookup-key x set)
  (cond ((null? set) false)
        ((= x (key (entry set))) (entry set))
        ((< x (key (entry set)))
         (lookup-key
          x
          (left-branch set)))
        ((> x (key (entry set)))
         (lookup-key
          x
          (right-branch set)))))

(define (adjoin-set k v set)
  (cond ((null? set) (make-tree (make-entry k v) '() '()))
        ((= k (key (entry set))) set)
        ((< k (key (entry set)))
         (make-tree
          (entry set)
          (adjoin-set k value (left-branch set))
          (right-branch set)))
        ((> k (key (entry set)))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-set k value (right-branch set))))))

(define (lookup key table)
  (let ([record (lookup-key key (cdr table))])
    (if record
        (value record)
        false)))

(define (insert! key value table)
  (let ([record (lookup-key key (cdr table))])
    (if record
        (set-value! record value)
        (set-cdr! table (adjoin-set key value (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define x (make-table))

(insert! 1 'a x)
(lookup 1 x)
