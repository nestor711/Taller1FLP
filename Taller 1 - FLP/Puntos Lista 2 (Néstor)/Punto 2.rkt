#lang eopl

;; down :
;; Proposito:
;; down L -> Lista Función que debe retornar una lista con cada elemento de L asociado a un nivel más de paréntesis comparado con su estado original en L.
;;
;;<list> ::= '() | <element> <list>
;;<element> ::= <atom> | <list>
;;<atom> ::= cualquier átomo (número, símbolo, etc.)

(define (down L)
  (cond
    ((null? L) '())   ; Caso base: si la lista es vacía, retornar una lista vacía
    ((list? (car L))  ; Si el primer elemento es una lista, procesarla recursivamente
     (cons (down (car L)) (down (cdr L))))
    (else             ; Si el primer elemento no es una lista, agregar paréntesis
     (cons (list (car L)) (down (cdr L))))))

;; Pruebas
;; > (down ’(1 2 3))
;;((1) (2) (3))
;;> (down ’((una) (buena) (idea)))
;;(((una)) ((buena)) ((idea)))
;;> (down ’(un (objeto (mas)) complicado))
;;((un) ((objeto (mas))) (complicado))

