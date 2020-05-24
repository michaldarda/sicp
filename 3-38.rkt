#lang racket

(require racket/include)

(include "sicp-concurrency.rkt")

(parallel-execute
 (lambda () (display 1))
 (lambda () (display 2))
 (lambda () (display 3)))
