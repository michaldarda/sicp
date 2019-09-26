#lang racket
(require threading)
(require racket/trace)

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left-branch right-branch)
  (list entry left-branch right-branch))

(define (element-of-set? x s)
  (cond [(null? s) #f]
        [(= x (entry s) #t)]
        [(< x (entry s))
         (element-of-set? x (left-branch s))]
        [(> x (entry s))
         (element-of-set? x (right-branch s))]))

(define (adjoin-set x s)
  (cond [(null? s) (make-tree x '() '())]
         [(= x (entry s)) s]
         [(< x (entry s))
          (make-tree (entry s)
                     (adjoin-set x (left-branch s))
                     (right-branch s))]
         [(> x (entry s))
          (make-tree (entry s)
                     (left-branch s)
                     (adjoin-set x (right-branch s)))]))

(define t (adjoin-set 1 (make-tree 2 '() '())))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree accu)
    (if (null? tree)
        accu
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             accu)))))
  (copy-to-list tree '()))

(trace tree->list-1)
(trace tree->list-2)

(define t1
  (~>> null
       (adjoin-set 1)
       (adjoin-set 2)
       (adjoin-set 3)
       (adjoin-set 4)
       (adjoin-set 5)
       (adjoin-set 6)
       (adjoin-set 7)))

(tree->list-1 t)
(tree->list-1 t)
(tree->list-2 t1)
(tree->list-2 t1)

(define t2
  (~>> null
       (adjoin-set 7)
       (adjoin-set 6)
       (adjoin-set 5)
       (adjoin-set 4)
       (adjoin-set 3)
       (adjoin-set 2)
       (adjoin-set 1)
       ))

(tree->list-1 t1)
(tree->list-2 t2)
