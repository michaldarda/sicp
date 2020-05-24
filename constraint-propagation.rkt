#lang sicp

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond [(null? items) 'done]
          [(eq? (car items) exception) (loop (cdr list))]
          [else (procedure (car items))
                (loop (cdr items))]))
  (loop list))

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-list-my-value))

(define (make-connector)
  (let ([value false]
        [informant false]
        [constraints '()])
    (define (set-my-value newval setter)
      (cond [(not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints)]
            [(not (= value newval))
             (error "Contradiction " (list value newval))]
            [else 'ignored]))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond [(eq? request 'has-value?) (if informant true false)]
            [(eq? request 'value) value]
            [(eq? request 'set-value!) set-my-value]
            [(eq? request 'forget) forget-my-value]
            [(eq? request 'connect) connect]
            [else (error "Unknown operation: CONNECTOR"
                         request)]))
    me))

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (set-value connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))
