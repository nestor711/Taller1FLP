#lang racket

;; down :
;; Proposito:
;; down L -> Lista Función que debe retornar una lista con cada elemento de L asociado a un nivel más de paréntesis comparado con su estado original en L.
;;
;;<list> ::= '() | <element> <list>
;;<element> ::= <atom> | <list>
;;<atom> ::= cualquier átomo (número, símbolo, etc.)

(define (down L) ; Se define la función down con argumento una lista L
  (cond 
    [(empty? L) '()]           ; Si la lista es vacía, retorna una lista vacía.
    [else (cons (list (car L)) ; Crea una lista con el primer elemento de L.
                (down (cdr L))) ; Llama a down recursivamente con el resto de la lista.
          ]))

;; Pruebas
;; > (down ’(1 2 3))
;;((1) (2) (3))
;;> (down ’((una) (buena) (idea)))
;;(((una)) ((buena)) ((idea)))
;;> (down ’(un (objeto (mas)) complicado))
;;((un) ((objeto (mas))) (complicado))

