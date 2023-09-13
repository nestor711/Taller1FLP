#lang eopl

(define (mapping F L1 L2)
  (cond
    ((or (null? L1) (null? L2)) '())
    ((= (F (car L1)) (car L2))
     (cons (list (car L1) (car L2))
           (mapping F (cdr L1) (cdr L2))))
    (else
     (mapping F (cdr L1) (cdr L2)))))