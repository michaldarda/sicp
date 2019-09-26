#lang racket

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

(define (tree->list tree)
  (define (copy-to-list tree accu)
    (if (null? tree)
        accu
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             accu)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ([left-size (quotient (- n 1) 2)])
        (let ([left-result
               (partial-tree elts left-size)])
          (let ([left-tree (car left-result)]
                [non-left-elts (cdr left-result)]
                [right-size (- n (+ left-size 1))])
            (let ([this-entry (car non-left-elts)]
                  [right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)])
              (let ([right-tree (car right-result)]
                    [remaining-elts
                     (cdr right-result)])
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

(define t (list->tree (list 1 2 3 4 5 6 7 8 9)))
(tree->list t)

;; the perfect balanced tree is when left and right subtree has
;; n/2 elements thats why (n / 2)
