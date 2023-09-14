#lang eopl

;; Ejercicio 7
;; cartesian-product:
;; Propósito:
;; cartesina-product L1 L2 -> la función recibe dos listas L1 y L2 en el que sus elementos internos no se repitan, la función debe retornar tuplas con la combinación (producto cartesiano) de los elementos la lista L1 con los elementos de la lista L2, el orden de las tuplas no tienen importancia
;;
;; Gramática BNF
;; <cartesian-product> ::= <cartesian-product> <(<list1>, <list2>)>
;; <list1> ::= <symbol> | <symbol> <list1>
;; <list2> ::= <symbol> | <symbol> <list2>
;; <symbol> ::= {<symbol>} (cadena de simbolos en el que ningún símbolo aparece más de una vez)


(define (cartesian-product L1 L2)
  (define (combinar x l)
    (Map-Func (lambda (y) (list x y)) l))
  (cond
    ((null? L1) '()) ; Caso base: Si L1 es vacía, se retorna una lista vacía
    (else
      (Append-Func (combinar (car L1) L2)
              (cartesian-product (cdr L1) L2))))) ; Llamada recursiva con el resto de L1 y L2

(define (Map-Func P l)
  (if (null? l) '()
      (cons (P (car l)) (Map-Func P (cdr l)))))

(define (Append-Func l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (Append-Func (cdr l1) l2))))

;; Pruebas
;; > (cartesian-product '(a b c) '(x y))
;;   ((a x) (a y) (b x) (b y) (c x) (c y))

;; > (cartesian-product '(p q r) '(5 6 7))
;;   ((p 5) (p 6) (p 7) (q 5) (q 6) (q 7) (r 5) (r 6) (r 7))

