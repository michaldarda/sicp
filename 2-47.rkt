#lang racket

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (cadr f))

(define (edge2-frame f)
  (caddr f))

(define f1 (make-frame 1 2 3))
(define f2 (make-frame 1 2 3))

(origin-frame f1)
(edge1-frame f1)
(edge2-frame f1)
(origin-frame f2)
(edge1-frame f2)
(edge2-frame f2)
