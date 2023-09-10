#lang eopl

;; zip :
;; Proposito:
;; S x L -> L’ : Procedimiento que remueve la primera
;; ocurrencia de un simbolo S en una lista de simbolos L.
; <zip> ::= zip(<funcion>, <lista1>, <lista2>)

(define (zip F L1 L2)
  (if (or (null? L1) (null? L2))
      '()
      (cons (F (car L1) (car L2))
            (zip F (cdr L1) (cdr L2)))))

; Pruebas
;> (zip + ’(1 4) ’(6 2))
;(7 6)
;> (zip * ’(11 5 6) ’(10 9 8))
;(110 45 48)
