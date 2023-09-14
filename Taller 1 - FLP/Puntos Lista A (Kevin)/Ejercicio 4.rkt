#lang eopl

;; Ejercicio 2
;; Función filter-in :
;; Proposito:
;; filter-in P L -> la función recibe un predicado P y una lista L, la función debe retornar los elementos de L que cumplen el predicado P
;;
;; Gramática BNF
;; <filter-in> ::= <filter-in> <(<predicado>, <lista>)>
;; <predicado> ::= <expresión lógica> | " "
;; <lista> ::= <elemento> | <lista> <elemento> | '()
;; <elemento> ::= <int> | <symbol> | <lista>


(define filter-in
  (lambda (P l) ; se recibe un predicado P y una lista L
    (if (null? l) '(); si la lista es vacía se retorna una lista vacía, de modo que no habrá ningún elemento que cumpla con el predicado P
        (if (P (car l)); si el primer elemento de la lista cumple con el predicado P
            (cons (car l) (filter-in P (cdr l))) ; #t: se construye una nueva lista con el primer elemento y se llama recursivamente a la función filter-in con el resto de los elementos de la lista L
            (filter-in P (cdr l)))))) ; #f: en caso de que el primer elemento no cumpla con el predicado P, se pasa recursivamente como predicado el resto de la lista a la función filter-in

;;Pruebas

;; > (filter-in number? '(a 2 (1 3) b 7))
;;   (2 7)

;; > (filter-in symbol? '(a (b c) 17 foo))
;;   (a foo)

;; > (filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3)))
;;   ("univalle" "racket" "flp")