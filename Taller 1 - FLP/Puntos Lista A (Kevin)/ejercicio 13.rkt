#lang eopl


;; Ejercicio 13
;; up:
;; Propósito:
;; up L-> la función recibe una lista L, la función retorna una nueva lista en el que se haya eliminado los paréntesis anidados, de modo que al final la lista retornada solo tenga los elementos de la lista enviada como parámetro.
;;
;; Gramática BNF
;; <lista> ::= (<elementos>) | '()
;; <elementos> ::= <elemento> | <elemento> <elementos>
;; <elemento> ::= <lista> | <valor>
;; <valor> ::= <int> | <symbol> | <boolean> | <lista>

(define (operate lrators lrands)
  (define (aplicar_operador operador operador1 operador2)
    (operador operador1 operador2))
  
  (let operadores ((operators lrators)
             (operands lrands))
    (if (null? operators)
        (car operands)
        (operadores (cdr operators)
              (cons (aplicar_operador (car operators) (car operands) (cadr operands))
                    (cddr operands))))))

;; Pruebas
;; >  (operate (list + * + - *) '(1 2 8 4 11 6))
;;    102

;; >  (operate (list *) '(4 5))
;;    20