#lang sicp

(define (make-deque)
  (let [(front-ptr '())
        (rear-ptr '())]
    (define (empty-deque?)
      (null? front-ptr))
    (define (front-deque)
      (if (empty-deque?)
          (error "FRONT called with an
              empty deque ")
          (car front-ptr)))
    (define (rear-deque?) '())
    (define (front-insert-deque! item)
      (let ([new-pair (cons item '())])
        (cond [(empty-deque?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)]
              [else (set-cdr! new-pair front-ptr)
                    (set! front-ptr new-pair)])))
    (define (rear-insert-deque! item)
      (let ([new-pair (cons item '())])
        (cond [(empty-deque?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)]
              [else (set-cdr! rear-ptr new-pair)
                    (set! rear-ptr new-pair)])))
    (define (front-delete-deque!)
      (cond [(empty-deque?)
             (error "DELETE called with an empty deque")]
            [else (set! front-ptr
                        (cdr front-ptr))]))
    (define (rear-delete-deque!) '()) ;; TODO
    (define (print-deque)
      (display front-ptr))
    (define (dispatch m)
      (cond [(eq? m 'empty-deque?) empty-deque?]
            [(eq? m 'front-deque) front-deque]
            [(eq? m 'rear-deque) rear-deque]
            [(eq? m 'front-insert-deque!) front-insert-deque!]
            [(eq? m 'rear-insert-deque!) rear-insert-deque!]
            [(eq? m 'front-delete-deque!) front-delete-deque!]
            [(eq? m 'rear-delete-deque!) rear-delete-deque!]
            [(eq? m 'print-deque) print-deque]
            [else (error "Unsupported message " m)]))
    dispatch))

(define (empty-deque? deque)
  ((deque 'empty-deque)))

(define (front-deque deque)
  ((deque 'front-deque)))

(define (rear-deque deque)
  ((deque 'rear-deque)))

(define (front-insert-deque! deque item)
  ((deque 'front-insert-deque!) item))

(define (rear-insert-deque! deque item)
  ((deque 'rear-insert-deque!) item))

(define (front-delete-deque! deque)
  ((deque 'front-delete-deque!)))

(define (rear-delete-deque! deque)
  ((deque 'rear-delete-deque!)))

(define (print-deque deque)
  ((deque 'print-deque)))

(define x (make-deque))

(front-insert-deque! x 1)
(front-insert-deque! x 2)
(rear-insert-deque! x 3)
(front-delete-deque! x)
;; (rear-delete-deque! x)

(print-deque x)
