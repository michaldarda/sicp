#lang racket

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ([next-branch
               (choose-branch (car bits) current-branch)])
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond [(= bit 0) (left-branch branch)]
        [(= bit 1) (right-branch branch)]
        [else (error "bad bit: CHOOSE-BRANCH" bit)]))

(define (adjoin-set x set)
  (cond [(null? set) (list x)]
        [(< (weight x) (weight (car set))) (cons x set)]
        [else (cons (car set)
                    (adjoin-set x (cdr set)))]))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ([pair (car pairs)])
        (adjoin-set
         (make-leaf (car pair)   ; symbol
                    (cadr pair)) ; frequency
         (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))

;; (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;; (decode sample-message sample-tree)

(define (encode message tree)
  (if (null? message)
      '()
      (append
       (encode-symbol (car message)
                      tree)
       (encode (cdr message) tree))))

;;

(define (encode-symbol symbol tree)
  (define (choose-branch-decoding tree)
    (let ([left-branch-symbols (symbols (left-branch tree))]
          [right-branch-symbols (symbols (right-branch tree))])
      (cond [(member symbol left-branch-symbols) 'left]
            [(member symbol right-branch-symbols) 'right]
            [else (error "probably bad constructed tree") tree])))

  (define (loop tree acc)
    (cond [(null? tree) (error symbol "not found")]
          [(and (leaf? tree) (eq? (symbol-leaf tree) symbol))
           acc]
          [else
           (let ([branch (choose-branch-decoding tree)])
             (cond [(eq? branch 'left)
                    (loop (left-branch tree) (append acc '(0)))]
                   [(eq? branch 'right)
                    (loop (right-branch tree) (append acc '(1)))]
                   [else (error "choose-branch-decoding returned something weird " branch)]))]))

  (loop tree '()))

;; (decode (encode '(A D A B B C A) sample-tree) sample-tree)

(define (successive-merge pairs)
  (if (eq? (length pairs) 1)
      (car pairs)
      (successive-merge
       (adjoin-set (make-code-tree (car pairs)
                                   (cadr pairs))
                   (cddr pairs)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define symbol-freq-pairs
  (list
   (list 'D 1)
   (list 'C 1)
   (list 'B 2)
   (list 'A 3)))

(define tree (generate-huffman-tree symbol-freq-pairs))

(decode (encode '(A D A B B C A) tree) tree)
