#lang sicp

(define (make-table same-key?)
  (let ([local-table (list '*table*)])
    (define (assoc key records)
      (cond [(null? records) false]
            [(same-key? key (caar records)) (car records)]
            [else (assoc key (cdr records))]))
    (define (lookup keys)
      (let ([key (car keys)]
            [rest-of-keys (cdr keys)])
        (let ([record (assoc key (cdr local-table))])
          (cond [(null? record) false]
                [(null? rest-of-keys) (cdr record)]
                [else (((cdr record) 'lookup) rest-of-keys)]))))
    (define (insert! keys value)
      (let ([key (car keys)]
            [rest-of-keys (cdr keys)])
        (let ([record (assoc key (cdr local-table))])
          (if (null? rest-of-keys)
              (if record
                  (set-cdr! record value)
                  (set-cdr! local-table
                            (cons (cons key value)
                                  (cdr local-table))))
              (if record
                  (((cdr record) 'insert!) rest-of-keys value)
                  (let ([new-table (make-table same-key?)])
                    (set-cdr! local-table
                              (cons (cons key new-table)
                                    (cdr local-table)))
                    ((new-table 'insert!) rest-of-keys value))))))
      'ok)
    (define (dispatch m)
      (cond [(eq? m 'insert!) insert!]
            [(eq? m 'lookup) lookup]
            [else (error "Unsupported message TABLE " m)]))
    dispatch))

(define x (make-table equal?))
((x 'insert!) '(a b) 1)
((x 'lookup) '(a b))
((x 'insert!) '(a c) 2)
((x 'lookup) '(a c))
