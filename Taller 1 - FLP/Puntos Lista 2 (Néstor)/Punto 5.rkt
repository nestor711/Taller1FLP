#lang racket

(define (list-index P L)
  (cond ((null? L) #f)
        ((P (car L)) 0)
        (else (let ((rest-result (list-index P (cdr L))))
                (if (not (eq? rest-result #f))
                    (+ 1 rest-result)
                    #f)))))