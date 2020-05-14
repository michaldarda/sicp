#lang sicp

(define (front-ptr queue)
  (car queue))

(define (rear-ptr queue)
  (cdr queue))

(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

;; public facing API

(define (make-queue)
  (let [(front-ptr '())
        (rear-ptr '())]
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an
              empty queue ")
          (car front-ptr)))
    (define (insert-queue! item)
      (let ([new-pair (cons item '())])
        (cond [(empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)]
              [else (set-cdr! rear-ptr new-pair)
                    (set! rear-ptr new-pair)])))
    (define (delete-queue!)
      (cond [(empty-queue?)
             (error "DELETE called with an empty queue")]
            [else (set! front-ptr
                        (cdr front-ptr))]))
    (define (print-queue)
      (display front-ptr))
    (define (dispatch m)
      (cond [(eq? m 'empty-queue?) empty-queue?]
            [(eq? m 'front-queue) front-queue]
            [(eq? m 'insert-queue!) insert-queue!]
            [(eq? m 'delete-queue!) delete-queue!]
            [(eq? m 'print-queue) print-queue]
            [else (error "Unsupported message " m)]))
    dispatch))

(define (empty-queue? queue)
  ((queue 'empty-queue)))

(define (front-queue? queue)
  ((queue 'front-queue)))

(define (insert-queue! queue item)
  ((queue 'insert-queue!) item))

(define (delete-queue! queue)
  ((queue 'delete-queue!)))

(define (print-queue queue)
  ((queue 'print-queue)))

(define x (make-queue))

(insert-queue! x 1)
(insert-queue! x 2)

(print-queue x)
