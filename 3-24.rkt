#lang sicp

(define (make-table same-key?)
  (let ([local-table (list '*table*)])
    (define (assoc key records)
      (cond [(null? records) false]
            [(same-key? key (caar records)) (car records)]
            [else (assoc key (cdr records))]))
    (define (lookup key)
      (let ([record (assoc key (cdr local-table))])
        (if record
            (cdr record)
            false)))
    (define (insert! key value)
      (let ([record (assoc key (cdr local-table))])
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons key value)
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond [(eq? m 'insert!) insert!]
            [(eq? m 'lookup) lookup]
            [else (error "Unsupported message TABLE " m)]))
    dispatch))

(define x (make-table equal?))
((x 'insert!) 'a 1)
((x 'lookup) 'a)


(define y (make-table (lambda (key1 key2) (< (abs (- key1 key2)) 0.01))))

((y 'insert!) 1.999 'hello)
((y 'lookup) 1.99)
