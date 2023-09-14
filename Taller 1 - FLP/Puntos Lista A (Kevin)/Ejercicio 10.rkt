#lang eopl

;; Ejercicio 10
;; up:
;; Propósito:
;; up L-> la función recibe una lista L, la función retorna una nueva lista en el que se haya eliminado los paréntesis anidados, de modo que al final la lista retornada solo tenga los elementos de la lista enviada como parámetro.
;;
;; Gramática BNF
;; <lista> ::= (<elementos>) | '()
;; <elementos> ::= <elemento> | <elemento> <elementos>
;; <elemento> ::= <lista> | <valor>
;; <valor> ::= <int> | <symbol> | <boolean> | <lista>


(define (up l)
  (cond ((null? l) '())
        ((pair? (car l))
         (cons (up (car l)) (up (cdr l))))
        (else (cons (car l) (up (cdr l))))))

(define (Append-Func l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (Append-Func (cdr l1) l2))))

;; Pruebas
;; >  (up '((1 2) (3 4)))
;;    (1 2 3 4)

;; >  (up '((x (y)) z))
;;    ((x (y)) z)
