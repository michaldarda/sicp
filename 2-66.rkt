#lang racket

(define (make-entry key value) (cons key value))
(define (entry-key entry) (car entry))
(define (entry-value entry) (cdr entry))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left-branch right-branch)
  (list entry left-branch right-branch))

(define (lookup given-key set-of-records)
  (cond [(null? set-of-records) #f]
        [(= given-key (entry-key (entry set-of-records))
            (entry-value set-of-records))]
        [(< given-key (entry-key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records))]
        [(> given-key (entry-key (entry set-of-records)))
         (lookup given-key (right-branch set-of-records))]))
